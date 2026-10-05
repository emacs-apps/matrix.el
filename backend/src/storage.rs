use std::{
    fs::{self, File, OpenOptions},
    io::{Read, Write},
    os::unix::fs::{MetadataExt, OpenOptionsExt, PermissionsExt},
    path::{Path, PathBuf},
};

use fs2::FileExt;
use matrix_sdk::authentication::matrix::MatrixSession;
use serde::{Deserialize, Serialize};
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
        Ok(Self { root, _lock: lock })
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
        let path = self.root.join(format!(".write-{}", Uuid::new_v4()));
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
            fs::rename(&path, self.root.join(name)).map_err(|_| STORAGE)?;
            self.sync_directory()
        })();
        if result.is_err() {
            let _ = fs::remove_file(&path);
        }
        result
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
