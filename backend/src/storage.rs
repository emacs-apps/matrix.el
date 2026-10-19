use std::{
    fs::{self, DirBuilder, File, OpenOptions},
    io::{Read, Write},
    os::unix::fs::{DirBuilderExt, MetadataExt, OpenOptionsExt, PermissionsExt},
    path::{Path, PathBuf},
    sync::Mutex,
};

use fs2::FileExt;
use matrix_sdk::authentication::matrix::MatrixSession;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use uuid::Uuid;
use zeroize::Zeroizing;

use crate::protocol::{Result, RpcError};

const STORAGE: RpcError = RpcError::new("store_io", "Unable to access private account storage");
const CORRUPT: RpcError = RpcError::new(
    "session_corrupt",
    "Saved session or SDK store is corrupt; do not log in over this profile",
);
const PRIVATE: RpcError = RpcError::new(
    "store_permissions",
    "Account storage must be owned by this user, private, and contain no symbolic links",
);
pub const INVALID_AVATAR: RpcError = RpcError::new(
    "avatar_invalid",
    "Avatar thumbnail is absent, oversized, or not a supported image",
);
pub const MAX_AVATAR_BYTES: usize = 2 * 1024 * 1024;
pub const INVALID_IMAGE: RpcError = RpcError::new(
    "image_invalid",
    "Image is absent, oversized, or not a supported raster image",
);
pub const MAX_IMAGE_BYTES: usize = 32 * 1024 * 1024;

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct SavedSession {
    pub version: u32,
    pub homeserver: String,
    pub generation: String,
    pub fingerprint: String,
    pub session: MatrixSession,
}

pub struct Store {
    root: PathBuf,
    _lock: File,
    media_cache: Mutex<()>,
}

impl Store {
    pub fn open(root: PathBuf) -> Result<Self> {
        if !root.is_absolute() {
            return Err(RpcError::new("store_path", "Store path must be absolute"));
        }
        fs::create_dir_all(&root).map_err(|_| STORAGE)?;
        check_private(&root, true)?;
        let lock_path = root.join("process.lock");
        let lock = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .mode(0o600)
            .custom_flags(libc::O_NOFOLLOW)
            .open(&lock_path)
            .map_err(|_| PRIVATE)?;
        check_private(&lock_path, false)?;
        lock.try_lock_exclusive().map_err(|_| {
            RpcError::new(
                "store_locked",
                "Another Matrix backend is using this profile",
            )
        })?;
        check_tree(&root)?;
        Ok(Self { root, _lock: lock, media_cache: Mutex::new(()) })
    }

    pub fn root(&self) -> &Path {
        &self.root
    }

    pub fn may_login(&self) -> Result<()> {
        if self
            .root
            .join("session.json")
            .try_exists()
            .map_err(|_| STORAGE)?
        {
            return Err(RpcError::new(
                "session_exists",
                "This profile has a saved session; restore it instead",
            ));
        }
        if self
            .root
            .join("account.initialized")
            .try_exists()
            .map_err(|_| STORAGE)?
        {
            return Err(CORRUPT);
        }
        if self
            .root
            .join("login.pending")
            .try_exists()
            .map_err(|_| STORAGE)?
        {
            return Err(RpcError::new(
                "login_uncertain",
                "Previous login did not finish saving; use a new profile only after reviewing your devices",
            ));
        }
        Ok(())
    }

    pub fn begin_login(&self) -> Result<String> {
        self.may_login()?;
        let generation = Uuid::new_v4().to_string();
        self.atomic_write("login.pending", generation.as_bytes())?;
        let sdk = self.sdk_path(&generation)?;
        fs::create_dir(&sdk).map_err(|_| STORAGE)?;
        fs::set_permissions(&sdk, fs::Permissions::from_mode(0o700)).map_err(|_| STORAGE)?;
        self.sync_directory()?;
        Ok(generation)
    }

    pub fn login_rejected(&self) -> Result<()> {
        self.remove("login.pending")
    }

    pub fn sdk_path(&self, generation: &str) -> Result<PathBuf> {
        if Uuid::parse_str(generation).is_err() || generation.len() != 36 {
            return Err(CORRUPT);
        }
        Ok(self.root.join(format!("sdk-{generation}")))
    }

    pub fn load(&self) -> Result<SavedSession> {
        let path = self.root.join("session.json");
        if !path.try_exists().map_err(|_| STORAGE)? {
            self.may_login()?;
            return Err(RpcError::new(
                "session_missing",
                "No saved session exists for this profile",
            ));
        }
        check_private(&path, false)?;
        let mut file = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_NOFOLLOW)
            .open(path)
            .map_err(|_| STORAGE)?;
        if file.metadata().map_err(|_| STORAGE)?.len() > 64 * 1024 {
            return Err(CORRUPT);
        }
        let mut bytes = Zeroizing::new(Vec::new());
        file.read_to_end(&mut bytes).map_err(|_| STORAGE)?;
        let saved: SavedSession = serde_json::from_slice(&bytes).map_err(|_| CORRUPT)?;
        if saved.version != 1 {
            return Err(CORRUPT);
        }
        let sdk = self.sdk_path(&saved.generation)?;
        check_private(&sdk, true).map_err(|_| CORRUPT)?;
        // Opening an empty crypto store would generate another identity under the same device ID.
        if !sdk.join("matrix-sdk-crypto.sqlite3").is_file()
            || !sdk.join("matrix-sdk-state.sqlite3").is_file()
        {
            return Err(CORRUPT);
        }
        Ok(saved)
    }

    pub fn save(&self, saved: &SavedSession) -> Result<()> {
        let bytes = Zeroizing::new(serde_json::to_vec(saved).map_err(|_| STORAGE)?);
        self.atomic_write("session.json", &bytes)?;
        self.atomic_write("account.initialized", saved.generation.as_bytes())?;
        if self.root.join("login.pending").exists() {
            self.remove("login.pending")?;
        }
        Ok(())
    }

    pub fn logout(&self) -> Result<()> {
        self.remove("session.json")?;
        self.remove("account.initialized")
    }

    fn avatar_path(&self, generation: &str, uri: &str) -> Result<PathBuf> {
        self.sdk_path(generation)?;
        let avatars = self.root.join("avatars");
        private_cache_directory(&avatars)?;
        let directory = avatars.join(generation);
        private_cache_directory(&directory)?;
        Ok(directory.join(format!(
            "96-crop-{:x}.image",
            Sha256::digest(uri.as_bytes())
        )))
    }

    pub fn cached_avatar(&self, generation: &str, uri: &str) -> Result<Option<PathBuf>> {
        let path = self.avatar_path(generation, uri)?;
        match fs::symlink_metadata(&path) {
            Ok(_) => check_private(&path, false)?,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(None),
            Err(_) => return Err(STORAGE),
        }
        let mut file = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
            .open(&path)
            .map_err(|_| PRIVATE)?;
        let metadata = file.metadata().map_err(|_| STORAGE)?;
        if !metadata.is_file() || metadata.len() > MAX_AVATAR_BYTES as u64 {
            return Ok(None);
        }
        let mut header = [0u8; 24];
        let length = (metadata.len() as usize).min(header.len());
        file.read_exact(&mut header[..length])
            .map_err(|_| STORAGE)?;
        Ok(valid_avatar(&header[..length]).then_some(path))
    }

    pub fn cache_avatar(&self, generation: &str, uri: &str, bytes: &[u8]) -> Result<PathBuf> {
        if !valid_avatar(bytes) {
            return Err(INVALID_AVATAR);
        }
        // MXC content identities are immutable. A concurrent successful request
        // must not be replaced, and invalid downloads never reach publication.
        if let Some(path) = self.cached_avatar(generation, uri)? {
            return Ok(path);
        }
        let path = self.avatar_path(generation, uri)?;
        self.atomic_write_at(
            path.parent().ok_or(STORAGE)?,
            path.file_name()
                .and_then(|name| name.to_str())
                .ok_or(STORAGE)?,
            bytes,
        )?;
        Ok(path)
    }

    fn image_path(&self, generation: &str, source_id: &str) -> Result<PathBuf> {
        self.sdk_path(generation)?;
        if !valid_source_id(source_id) {
            return Err(INVALID_IMAGE);
        }
        let media = self.root.join("media");
        private_cache_directory(&media)?;
        let directory = media.join(generation);
        private_cache_directory(&directory)?;
        Ok(directory.join(format!("{source_id}.image")))
    }

    pub fn cached_image(&self, generation: &str, source_id: &str) -> Result<Option<PathBuf>> {
        let _cache = self.media_cache.lock().map_err(|_| STORAGE)?;
        let path = self.image_path(generation, source_id)?;
        Ok(cached_image_at(&path)?.then_some(path))
    }

    pub fn cache_image(&self, generation: &str, source_id: &str, bytes: &[u8]) -> Result<PathBuf> {
        if !valid_image(bytes) {
            return Err(INVALID_IMAGE);
        }
        // Store's exclusive process lock plus this file-operation-only mutex
        // orders the existence check and atomic publication. Concurrent downloads
        // never replace an already accepted complete-source identity.
        let _cache = self.media_cache.lock().map_err(|_| STORAGE)?;
        let path = self.image_path(generation, source_id)?;
        if cached_image_at(&path)? {
            return Ok(path);
        }
        self.atomic_write_at(
            path.parent().ok_or(STORAGE)?,
            path.file_name().and_then(|name| name.to_str()).ok_or(STORAGE)?,
            bytes,
        )?;
        Ok(path)
    }

    fn remove(&self, name: &str) -> Result<()> {
        fs::remove_file(self.root.join(name)).map_err(|_| STORAGE)?;
        self.sync_directory()
    }

    fn sync_directory(&self) -> Result<()> {
        File::open(&self.root)
            .and_then(|f| f.sync_all())
            .map_err(|_| STORAGE)
    }

    fn atomic_write(&self, name: &str, bytes: &[u8]) -> Result<()> {
        self.atomic_write_at(&self.root, name, bytes)
    }

    fn atomic_write_at(&self, directory: &Path, name: &str, bytes: &[u8]) -> Result<()> {
        let path = directory.join(format!(".write-{}", Uuid::new_v4()));
        let result = (|| {
            let mut file = OpenOptions::new()
                .write(true)
                .create_new(true)
                .mode(0o600)
                .custom_flags(libc::O_NOFOLLOW)
                .open(&path)
                .map_err(|_| STORAGE)?;
            file.write_all(bytes).map_err(|_| STORAGE)?;
            file.sync_all().map_err(|_| STORAGE)?;
            fs::rename(&path, directory.join(name)).map_err(|_| STORAGE)?;
            File::open(directory)
                .and_then(|file| file.sync_all())
                .map_err(|_| STORAGE)
        })();
        if result.is_err() {
            let _ = fs::remove_file(&path);
        }
        result
    }
}

fn private_cache_directory(path: &Path) -> Result<()> {
    match DirBuilder::new().mode(0o700).create(path) {
        Ok(()) => {
            File::open(path.parent().ok_or(STORAGE)?)
                .and_then(|file| file.sync_all())
                .map_err(|_| STORAGE)?;
        }
        Err(error) if error.kind() == std::io::ErrorKind::AlreadyExists => {}
        Err(_) => return Err(STORAGE),
    }
    check_private(path, true)
}

pub fn valid_avatar(bytes: &[u8]) -> bool {
    !bytes.is_empty() && bytes.len() <= MAX_AVATAR_BYTES && raster_signature(bytes)
}

pub fn valid_image(bytes: &[u8]) -> bool {
    !bytes.is_empty() && bytes.len() <= MAX_IMAGE_BYTES && raster_signature(bytes)
}

pub fn valid_source_id(source_id: &str) -> bool {
    source_id.len() == 64 && source_id.bytes().all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

fn cached_image_at(path: &Path) -> Result<bool> {
    match fs::symlink_metadata(path) {
        Ok(_) => check_private(path, false)?,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(_) => return Err(STORAGE),
    }
    let mut file = OpenOptions::new()
        .read(true)
        .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
        .open(path)
        .map_err(|_| PRIVATE)?;
    let metadata = file.metadata().map_err(|_| STORAGE)?;
    if !metadata.is_file() || metadata.len() > MAX_IMAGE_BYTES as u64 {
        return Err(INVALID_IMAGE);
    }
    let mut header = [0u8; 24];
    let length = (metadata.len() as usize).min(header.len());
    file.read_exact(&mut header[..length]).map_err(|_| STORAGE)?;
    if !valid_image(&header[..length]) {
        // Complete-source files are immutable, including on corruption: do not
        // silently replace an existing private path with different bytes.
        return Err(INVALID_IMAGE);
    }
    Ok(true)
}

fn raster_signature(bytes: &[u8]) -> bool {
    // The presentation layer owns decoding. Accept only conventional raster
    // signatures here, never an HTML/error body or executable SVG content.
    (bytes.len() >= 24 && bytes.starts_with(b"\x89PNG\r\n\x1a\n") && &bytes[12..16] == b"IHDR")
        || (bytes.len() >= 4 && bytes.starts_with(b"\xff\xd8\xff"))
        || (bytes.len() >= 13 && (bytes.starts_with(b"GIF87a") || bytes.starts_with(b"GIF89a")))
        || (bytes.len() >= 20
            && bytes.starts_with(b"RIFF")
            && &bytes[8..12] == b"WEBP"
            && matches!(&bytes[12..16], b"VP8 " | b"VP8L" | b"VP8X"))
}

/// A file staged beside a new destination outside account storage, published
/// atomically without ever replacing an existing path (including symlinks).
pub struct NewFile {
    _directory: tempfile::TempDir,
    staged: PathBuf,
    destination: PathBuf,
    parent: PathBuf,
    error: RpcError,
}

impl NewFile {
    /// Reserve absolute DESTINATION, reporting any failure as ERROR.
    pub fn reserve(root: &Path, destination: &Path, error: RpcError) -> Result<Self> {
        if !destination.is_absolute() {
            return Err(error);
        }
        let parent = destination.parent().ok_or(error)?.canonicalize().map_err(|_| error)?;
        if parent.starts_with(root.canonicalize().map_err(|_| error)?)
            || destination.symlink_metadata().is_ok()
        {
            return Err(error);
        }
        let directory = tempfile::Builder::new()
            .prefix(".matrix-")
            .tempdir_in(&parent)
            .map_err(|_| error)?;
        Ok(Self {
            staged: directory.path().join("staged"),
            _directory: directory,
            destination: destination.to_owned(),
            parent,
            error,
        })
    }

    pub fn staged(&self) -> &Path {
        &self.staged
    }

    pub fn publish(self) -> Result<()> {
        File::open(&self.staged).and_then(|file| file.sync_all()).map_err(|_| self.error)?;
        fs::hard_link(&self.staged, &self.destination).map_err(|_| self.error)?;
        File::open(&self.parent).and_then(|file| file.sync_all()).map_err(|_| self.error)
    }
}

fn check_private(path: &Path, directory: bool) -> Result<()> {
    let metadata = fs::symlink_metadata(path).map_err(|_| PRIVATE)?;
    if metadata.file_type().is_symlink()
        || metadata.uid() != unsafe { libc::geteuid() }
        || metadata.mode() & 0o077 != 0
        || (directory && !metadata.is_dir())
        || (!directory && (!metadata.is_file() || metadata.nlink() != 1))
    {
        return Err(PRIVATE);
    }
    Ok(())
}

fn check_tree(root: &Path) -> Result<()> {
    for entry in fs::read_dir(root).map_err(|_| STORAGE)? {
        let entry = entry.map_err(|_| STORAGE)?;
        let directory = entry.file_type().map_err(|_| STORAGE)?.is_dir();
        check_private(&entry.path(), directory)?;
        if directory {
            check_tree(&entry.path())?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn private_dir() -> tempfile::TempDir {
        let dir = tempfile::tempdir().unwrap();
        fs::set_permissions(dir.path(), fs::Permissions::from_mode(0o700)).unwrap();
        dir
    }

    const AVATAR_PNG: &[u8] = b"\x89PNG\r\n\x1a\n\
        \x00\x00\x00\x0dIHDR\x00\x00\x00\x01\x00\x00\x00\x01\
        \x08\x04\x00\x00\x00\xb5\x1c\x0c\x02\x00\x00\x00\x0bIDAT\
        \x78\xda\x63\xfc\xff\x1f\x00\x03\x03\x02\x00\xef\xa3\xf0\xad\
        \x00\x00\x00\x00IEND\xae\x42\x60\x82";

    #[test]
    fn image_cache_is_private_immutable_and_complete_identity_scoped() {
        let dir = private_dir();
        let store = Store::open(dir.path().into()).unwrap();
        let generation = Uuid::new_v4().to_string();
        let source = format!("{:x}", Sha256::digest(b"complete source"));
        let path = store.cache_image(&generation, &source, AVATAR_PNG).unwrap();
        assert!(path.is_absolute());
        assert_eq!(path, dir.path().join("media").join(&generation).join(format!("{source}.image")));
        assert_eq!(fs::metadata(&path).unwrap().mode() & 0o777, 0o600);
        for directory in [dir.path().join("media"), path.parent().unwrap().into()] {
            assert_eq!(fs::metadata(directory).unwrap().mode() & 0o777, 0o700);
        }
        let inode = fs::metadata(&path).unwrap().ino();
        assert_eq!(store.cached_image(&generation, &source).unwrap(), Some(path.clone()));
        let different_raster = b"GIF89a\x01\x00\x01\x00\x00\x00\x00";
        assert_eq!(store.cache_image(&generation, &source, different_raster).unwrap(), path);
        assert_eq!(fs::metadata(&path).unwrap().ino(), inode);
        assert_eq!(fs::read(&path).unwrap(), AVATAR_PNG);
        let replacement = format!("{:x}", Sha256::digest(b"replacement crypto"));
        assert_ne!(store.cache_image(&generation, &replacement, AVATAR_PNG).unwrap(), path);
        let next_generation = Uuid::new_v4().to_string();
        assert_eq!(store.cached_image(&next_generation, &source).unwrap(), None);
        assert_ne!(store.cache_image(&next_generation, &source, AVATAR_PNG).unwrap(), path);
        let mut oversized = AVATAR_PNG.to_vec();
        oversized.resize(MAX_IMAGE_BYTES + 1, 0);
        assert!(!valid_image(&oversized));
        oversized.truncate(MAX_IMAGE_BYTES);
        assert!(valid_image(&oversized));
        for bytes in [b"".as_slice(), b"<svg>executable</svg>".as_slice(), b"<html>error</html>".as_slice()] {
            assert_eq!(store.cache_image(&generation, &source, bytes).unwrap_err().code, "image_invalid");
        }
        for invalid in ["../outside", "", "abc", &"A".repeat(64)] {
            assert!(store.cached_image(&generation, invalid).is_err());
        }
        assert!(store.cached_image("../outside", &source).is_err());
    }

    #[test]
    fn image_cache_rejects_unsafe_and_corrupted_paths_without_replacement() {
        let dir = private_dir();
        let store = Store::open(dir.path().into()).unwrap();
        let generation = Uuid::new_v4().to_string();
        let source = "0".repeat(64);
        let outside_directory = private_dir();
        let media = dir.path().join("media");
        std::os::unix::fs::symlink(outside_directory.path(), &media).unwrap();
        assert_eq!(store.cached_image(&generation, &source).unwrap_err().code, "store_permissions");
        fs::remove_file(&media).unwrap();
        let path = store.image_path(&generation, &source).unwrap();
        let outside = tempfile::NamedTempFile::new().unwrap();
        fs::write(outside.path(), AVATAR_PNG).unwrap();
        std::os::unix::fs::symlink(outside.path(), &path).unwrap();
        assert_eq!(store.cache_image(&generation, &source, AVATAR_PNG).unwrap_err().code, "store_permissions");
        assert_eq!(fs::read(outside.path()).unwrap(), AVATAR_PNG);
        fs::remove_file(&path).unwrap();
        fs::hard_link(outside.path(), &path).unwrap();
        assert_eq!(store.cached_image(&generation, &source).unwrap_err().code, "store_permissions");
        fs::remove_file(&path).unwrap();
        fs::write(&path, b"<html>failed media").unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o644)).unwrap();
        assert_eq!(store.cached_image(&generation, &source).unwrap_err().code, "store_permissions");
        fs::set_permissions(&path, fs::Permissions::from_mode(0o600)).unwrap();
        let inode = fs::metadata(&path).unwrap().ino();
        assert_eq!(store.cache_image(&generation, &source, AVATAR_PNG).unwrap_err().code, "image_invalid");
        assert_eq!(fs::metadata(&path).unwrap().ino(), inode);
        assert_eq!(fs::read(&path).unwrap(), b"<html>failed media");
    }

    #[test]
    fn concurrent_image_downloads_publish_only_one_immutable_file() {
        let dir = private_dir();
        let store = std::sync::Arc::new(Store::open(dir.path().into()).unwrap());
        let generation = Uuid::new_v4().to_string();
        let source = "1".repeat(64);
        let workers: Vec<_> = (0..8).map(|_| {
            let store = store.clone();
            let generation = generation.clone();
            let source = source.clone();
            std::thread::spawn(move || {
                let path = store.cache_image(&generation, &source, AVATAR_PNG).unwrap();
                fs::metadata(path).unwrap().ino()
            })
        }).collect();
        let inodes: Vec<_> = workers.into_iter().map(|worker| worker.join().unwrap()).collect();
        assert!(inodes.iter().all(|inode| *inode == inodes[0]));
    }

    #[test]
    fn avatar_cache_is_private_immutable_and_identity_scoped() {
        let dir = private_dir();
        let store = Store::open(dir.path().into()).unwrap();
        let generation = Uuid::new_v4().to_string();
        let uri = "mxc://example.org/avatar";
        let path = store.cache_avatar(&generation, uri, AVATAR_PNG).unwrap();
        assert!(path.is_absolute());
        assert!(path.starts_with(dir.path().join("avatars").join(&generation)));
        assert_eq!(fs::metadata(&path).unwrap().mode() & 0o777, 0o600);
        for directory in [dir.path().join("avatars"), path.parent().unwrap().into()] {
            assert_eq!(fs::metadata(directory).unwrap().mode() & 0o777, 0o700);
        }
        let inode = fs::metadata(&path).unwrap().ino();
        assert_eq!(
            store.cached_avatar(&generation, uri).unwrap(),
            Some(path.clone())
        );
        assert_eq!(
            store.cache_avatar(&generation, uri, AVATAR_PNG).unwrap(),
            path
        );
        assert_eq!(fs::metadata(&path).unwrap().ino(), inode);

        let mut oversized = AVATAR_PNG.to_vec();
        oversized.resize(MAX_AVATAR_BYTES + 1, 0);
        for bytes in [b"".as_slice(), b"<html>not an image".as_slice(), &oversized] {
            assert_eq!(
                store
                    .cache_avatar(&generation, uri, bytes)
                    .unwrap_err()
                    .code,
                "avatar_invalid"
            );
            assert_eq!(fs::read(&path).unwrap(), AVATAR_PNG);
        }
        assert_ne!(
            store
                .avatar_path(&generation, "mxc://example.org/new")
                .unwrap(),
            path
        );
        let next_generation = Uuid::new_v4().to_string();
        assert_eq!(store.cached_avatar(&next_generation, uri).unwrap(), None);
        assert_ne!(
            store
                .cache_avatar(&next_generation, uri, AVATAR_PNG)
                .unwrap(),
            path
        );
        assert!(!fs::read_dir(path.parent().unwrap()).unwrap().any(|entry| {
            entry
                .unwrap()
                .file_name()
                .to_string_lossy()
                .starts_with(".write-")
        }));
    }

    #[test]
    fn avatar_cache_rejects_unsafe_paths_and_repairs_invalid_private_bytes() {
        let dir = private_dir();
        let store = Store::open(dir.path().into()).unwrap();
        let generation = Uuid::new_v4().to_string();
        let uri = "mxc://example.org/avatar";
        let outside_directory = private_dir();
        let avatars = dir.path().join("avatars");
        std::os::unix::fs::symlink(outside_directory.path(), &avatars).unwrap();
        assert_eq!(
            store.cached_avatar(&generation, uri).unwrap_err().code,
            "store_permissions"
        );
        fs::remove_file(&avatars).unwrap();

        let path = store.avatar_path(&generation, uri).unwrap();
        let outside = tempfile::NamedTempFile::new().unwrap();
        fs::write(outside.path(), AVATAR_PNG).unwrap();
        std::os::unix::fs::symlink(outside.path(), &path).unwrap();
        assert_eq!(
            store
                .cache_avatar(&generation, uri, AVATAR_PNG)
                .unwrap_err()
                .code,
            "store_permissions"
        );
        assert_eq!(fs::read(outside.path()).unwrap(), AVATAR_PNG);
        fs::remove_file(&path).unwrap();

        fs::hard_link(outside.path(), &path).unwrap();
        assert_eq!(
            store.cached_avatar(&generation, uri).unwrap_err().code,
            "store_permissions"
        );
        fs::remove_file(&path).unwrap();
        fs::write(&path, b"<html>failed thumbnail").unwrap();
        fs::set_permissions(&path, fs::Permissions::from_mode(0o644)).unwrap();
        assert_eq!(
            store.cached_avatar(&generation, uri).unwrap_err().code,
            "store_permissions"
        );
        fs::set_permissions(&path, fs::Permissions::from_mode(0o600)).unwrap();
        assert_eq!(store.cached_avatar(&generation, uri).unwrap(), None);
        assert_eq!(
            store.cache_avatar(&generation, uri, AVATAR_PNG).unwrap(),
            path
        );
        assert_eq!(fs::read(&path).unwrap(), AVATAR_PNG);

        OpenOptions::new()
            .write(true)
            .open(&path)
            .unwrap()
            .set_len(MAX_AVATAR_BYTES as u64 + 1)
            .unwrap();
        assert_eq!(store.cached_avatar(&generation, uri).unwrap(), None);
        store.cache_avatar(&generation, uri, AVATAR_PNG).unwrap();
        assert_eq!(fs::read(&path).unwrap(), AVATAR_PNG);
        assert!(store.avatar_path("../outside", uri).is_err());
    }

    #[test]
    fn lock_prevents_two_accounts_writing_one_store() {
        let dir = private_dir();
        let first = Store::open(dir.path().into()).unwrap();
        assert_eq!(
            Store::open(dir.path().into()).err().unwrap().code,
            "store_locked"
        );
        drop(first);
        assert!(Store::open(dir.path().into()).is_ok());
    }

    #[test]
    fn atomic_replace_is_private_and_truncates_previous_authority() {
        let dir = private_dir();
        let store = Store::open(dir.path().into()).unwrap();
        store
            .atomic_write(
                "session.json",
                b"{\"access_token\":\"previous-long-token\"}",
            )
            .unwrap();
        store
            .atomic_write("session.json", b"{\"access_token\":\"new\"}")
            .unwrap();
        let path = dir.path().join("session.json");
        assert_eq!(fs::read(&path).unwrap(), b"{\"access_token\":\"new\"}");
        assert_eq!(
            fs::metadata(&path).unwrap().permissions().mode() & 0o777,
            0o600
        );
        assert!(!fs::read_dir(dir.path()).unwrap().any(|entry| {
            entry
                .unwrap()
                .file_name()
                .to_string_lossy()
                .starts_with(".write-")
        }));
    }

    #[test]
    fn partial_login_never_implicitly_creates_another_device() {
        let dir = private_dir();
        let store = Store::open(dir.path().into()).unwrap();
        assert_eq!(store.load().err().unwrap().code, "session_missing");
        store.begin_login().unwrap();
        assert_eq!(store.may_login().unwrap_err().code, "login_uncertain");
        assert_eq!(store.load().err().unwrap().code, "login_uncertain");
    }

    #[test]
    fn lost_session_authority_cannot_start_another_device() {
        let dir = private_dir();
        let store = Store::open(dir.path().into()).unwrap();
        store
            .atomic_write("account.initialized", b"saved identity")
            .unwrap();
        assert_eq!(store.may_login().unwrap_err().code, "session_corrupt");
        assert_eq!(store.load().err().unwrap().code, "session_corrupt");
    }

    #[test]
    fn rejects_symlinks_public_files_and_path_traversal() {
        let dir = private_dir();
        let store = Store::open(dir.path().into()).unwrap();
        assert!(store.sdk_path("../outside").is_err());
        let outside = tempfile::NamedTempFile::new().unwrap();
        std::os::unix::fs::symlink(outside.path(), dir.path().join("session.json")).unwrap();
        assert!(store.load().is_err());
        fs::remove_file(dir.path().join("session.json")).unwrap();
        store
            .atomic_write("session.json", b"not a session")
            .unwrap();
        assert_eq!(store.load().err().unwrap().code, "session_corrupt");
        fs::set_permissions(
            dir.path().join("session.json"),
            fs::Permissions::from_mode(0o644),
        )
        .unwrap();
        assert_eq!(store.load().err().unwrap().code, "store_permissions");
    }
}
