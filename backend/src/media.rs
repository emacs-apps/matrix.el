use std::{path::PathBuf, sync::Arc, time::Duration};

use matrix_sdk::{
    Client,
    media::{MediaFormat, MediaRequestParameters, MediaThumbnailSettings},
    ruma::{
        OwnedMxcUri,
        api::client::media::get_content_thumbnail::v3::Method,
        events::room::MediaSource,
        uint,
    },
};
use tokio_util::sync::CancellationToken;
use tokio::sync::Semaphore;

use crate::{
    protocol::{NETWORK, Result, RpcError},
    storage::{INVALID_AVATAR, Store, valid_avatar},
};

const CANCELLED: RpcError = RpcError::new(
    "avatar_cancelled",
    "Avatar request was cancelled because its account or transport closed",
);
const CACHE_WORKER: RpcError = RpcError::new("store_io", "Unable to access private account storage");
const TIMEOUT: RpcError = RpcError::new("avatar_timeout", "Avatar thumbnail request timed out");

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
