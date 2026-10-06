use std::{path::PathBuf, sync::Arc, time::Duration};

use matrix_sdk::{
    Client,
    media::{MediaFormat, MediaRequestParameters, MediaThumbnailSettings},
    ruma::{
        OwnedMxcUri, api::client::media::get_content_thumbnail::v3::Method,
        events::room::MediaSource, uint,
    },
};
use sha2::{Digest, Sha256};
use tokio::sync::Semaphore;
use tokio_util::sync::CancellationToken;

use crate::{
    protocol::{NETWORK, Result, RpcError},
    storage::{INVALID_AVATAR, INVALID_IMAGE, Store, valid_avatar, valid_image},
    timeline::{ImageLease, Timelines},
};

const CANCELLED: RpcError = RpcError::new(
    "avatar_cancelled",
    "Avatar request was cancelled because its account or transport closed",
);
const CACHE_WORKER: RpcError =
    RpcError::new("store_io", "Unable to access private account storage");
const TIMEOUT: RpcError = RpcError::new("avatar_timeout", "Avatar thumbnail request timed out");
pub const STALE_IMAGE: RpcError = RpcError::new(
    "stale_image",
    "The target event no longer contains this image source",
);
const IMAGE_CANCELLED: RpcError = RpcError::new(
    "image_cancelled",
    "Image request was cancelled because its account, subscription, or transport closed",
);
const IMAGE_TIMEOUT: RpcError = RpcError::new("image_timeout", "Image download timed out");

// Stream the SDK's complete source representation into the digest, including
// the key, IV, protocol version and ordered ciphertext hashes. Never allocate
// a JSON buffer containing attachment keys or expose that representation.
pub fn source_id(source: &MediaSource) -> String {
    struct HashWriter(Sha256);
    impl std::io::Write for HashWriter {
        fn write(&mut self, bytes: &[u8]) -> std::io::Result<usize> {
            self.0.update(bytes);
            Ok(bytes.len())
        }
        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }
    let mut writer = HashWriter(Sha256::new());
    serde_json::to_writer(&mut writer, source)
        .expect("SDK MediaSource serialization to a digest is infallible");
    format!("{:x}", writer.0.finalize())
}

fn check_image_live(
    lease: &ImageLease,
    cancel: &CancellationToken,
    transport: &CancellationToken,
) -> Result<()> {
    if lease.cancellation().is_cancelled() || cancel.is_cancelled() || transport.is_cancelled() {
        Err(IMAGE_CANCELLED)
    } else {
        Ok(())
    }
}

pub async fn image(
    store: Arc<Store>,
    client: Client,
    generation: String,
    timelines: &Timelines,
    lease: &ImageLease,
    cancel: CancellationToken,
    transport: CancellationToken,
    slots: &Semaphore,
) -> Result<PathBuf> {
    check_image_live(lease, &cancel, &transport)?;
    lease.validate(timelines).await?;
    let cache_store = store.clone();
    let cache_generation = generation.clone();
    let cache_source = lease.source_id.clone();
    let cached = tokio::task::spawn_blocking(move || {
        cache_store.cached_image(&cache_generation, &cache_source)
    }).await.map_err(|_| CACHE_WORKER)??;
    check_image_live(lease, &cancel, &transport)?;
    lease.validate(timelines).await?;
    check_image_live(lease, &cancel, &transport)?;
    if let Some(path) = cached {
        return Ok(path);
    }
    let _slot = tokio::select! {
        biased;
        _ = cancel.cancelled() => return Err(IMAGE_CANCELLED),
        _ = transport.cancelled() => return Err(IMAGE_CANCELLED),
        _ = lease.cancellation().cancelled() => return Err(IMAGE_CANCELLED),
        slot = slots.acquire() => slot.map_err(|_| IMAGE_CANCELLED)?,
    };
    lease.validate(timelines).await?;
    let media = client.media();
    let bytes = tokio::select! {
        biased;
        _ = cancel.cancelled() => return Err(IMAGE_CANCELLED),
        _ = transport.cancelled() => return Err(IMAGE_CANCELLED),
        _ = lease.cancellation().cancelled() => return Err(IMAGE_CANCELLED),
        result = tokio::time::timeout(
            Duration::from_secs(60),
            // SDK cache identities need not include the encryption metadata.
            // Our complete-source cache is authoritative; also avoid its Vec clone.
            media.get_media_content(&lease.request, false),
        ) => result.map_err(|_| IMAGE_TIMEOUT)?.map_err(|_| NETWORK)?,
    };
    check_image_live(lease, &cancel, &transport)?;
    lease.validate(timelines).await?;
    if !valid_image(&bytes) {
        return Err(INVALID_IMAGE);
    }
    let source_id = lease.source_id.clone();
    let cache_cancel = cancel.clone();
    let cache_transport = transport.clone();
    let subscription_cancel = lease.cancellation().clone();
    let path = tokio::task::spawn_blocking(move || {
        if cache_cancel.is_cancelled() || cache_transport.is_cancelled() || subscription_cancel.is_cancelled() {
            return Err(IMAGE_CANCELLED);
        }
        store.cache_image(&generation, &source_id, &bytes)
    }).await.map_err(|_| CACHE_WORKER)??;
    check_image_live(lease, &cancel, &transport)?;
    lease.validate(timelines).await?;
    check_image_live(lease, &cancel, &transport)?;
    Ok(path)
}

fn check_live(cancel: &CancellationToken, transport: &CancellationToken) -> Result<()> {
    if cancel.is_cancelled() || transport.is_cancelled() {
        Err(CANCELLED)
    } else {
        Ok(())
    }
}

// The caller reserves an account task token before releasing the account lock.
// Private file operations finish before that token is released; network reads
// have no account lock and are revoked by logout or transport shutdown.
pub async fn avatar(
    store: Arc<Store>,
    client: Client,
    generation: String,
    uri: OwnedMxcUri,
    cancel: CancellationToken,
    transport: CancellationToken,
    slots: &Semaphore,
) -> Result<PathBuf> {
    check_live(&cancel, &transport)?;
    let cache_store = store.clone();
    let cache_generation = generation.clone();
    let cache_uri = uri.clone();
    let cache_cancel = cancel.clone();
    let cache_transport = transport.clone();
    let cached = tokio::task::spawn_blocking(move || {
        check_live(&cache_cancel, &cache_transport)?;
        cache_store.cached_avatar(&cache_generation, cache_uri.as_str())
    })
    .await
    .map_err(|_| CACHE_WORKER)??;
    check_live(&cancel, &transport)?;
    if let Some(path) = cached {
        return Ok(path);
    }

    let _slot = tokio::select! {
        biased;
        _ = cancel.cancelled() => return Err(CANCELLED),
        _ = transport.cancelled() => return Err(CANCELLED),
        slot = slots.acquire() => slot.map_err(|_| CANCELLED)?,
    };

    let request = MediaRequestParameters {
        source: MediaSource::Plain(uri.clone()),
        format: MediaFormat::Thumbnail(MediaThumbnailSettings::with_method(
            Method::Crop,
            uint!(96),
            uint!(96),
        )),
    };
    let media = client.media();
    // SDK 0.19.1 owns authenticated endpoint selection and buffers the whole
    // response. Bound our wait and reject oversized/invalid bytes before the
    // private file cache; do not introduce an alternative HTTP/media transport.
    let bytes = tokio::select! {
        biased;
        _ = cancel.cancelled() => return Err(CANCELLED),
        _ = transport.cancelled() => return Err(CANCELLED),
        result = tokio::time::timeout(
            Duration::from_secs(30),
            media.get_media_content(&request, true),
        ) => result.map_err(|_| TIMEOUT)?.map_err(|_| NETWORK)?,
    };
    check_live(&cancel, &transport)?;
    if !valid_avatar(&bytes) {
        return Err(INVALID_AVATAR);
    }

    let cache_cancel = cancel.clone();
    let cache_transport = transport.clone();
    let path = tokio::task::spawn_blocking(move || {
        check_live(&cache_cancel, &cache_transport)?;
        store.cache_avatar(&generation, uri.as_str(), &bytes)
    })
    .await
    .map_err(|_| CACHE_WORKER)??;
    check_live(&cancel, &transport)?;
    Ok(path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn source_identity_includes_all_attachment_crypto_metadata() {
        let original = json!({"file":{
            "url":"mxc://example.org/image", "v":"v2",
            "key":{"kty":"oct", "alg":"A256CTR", "key_ops":["encrypt","decrypt"],
                "k":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", "ext":true},
            "iv":"AAAAAAAAAAAAAAAAAAAAAA",
            "hashes":{"sha256":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}
        }});
        let source: MediaSource = serde_json::from_value(original.clone()).unwrap();
        let identity = source_id(&source);
        assert!(crate::storage::valid_source_id(&identity));
        let plain: MediaSource = serde_json::from_value(json!({"url":"mxc://example.org/image"})).unwrap();
        assert_ne!(source_id(&plain), identity);
        for (pointer, replacement) in [
            ("/file/url", json!("mxc://example.org/replacement")),
            ("/file/key/k", json!("AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")),
            ("/file/iv", json!("AQAAAAAAAAAAAAAAAAAAAA")),
            ("/file/hashes/sha256", json!("AQAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")),
            ("/file/v", json!("custom-version")),
        ] {
            let mut changed = original.clone();
            *changed.pointer_mut(pointer).unwrap() = replacement;
            let changed: MediaSource = serde_json::from_value(changed).unwrap();
            assert_ne!(source_id(&changed), identity);
        }
        assert_eq!(source_id(&serde_json::from_value::<MediaSource>(original).unwrap()), identity);
    }
}
