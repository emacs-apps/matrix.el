use std::{
    path::{Path, PathBuf},
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    time::Duration,
};

use matrix_sdk::{
    Client, RoomState,
    config::{RequestConfig, SyncSettings},
    encryption::{BackupDownloadStrategy, EncryptionSettings},
    latest_events::LatestEventValue,
    ruma::{
        OwnedDeviceId, OwnedEventId, OwnedMxcUri, OwnedRoomId, OwnedRoomOrAliasId, OwnedUserId,
        api::{
            client::{room::create_room, session::get_login_types::v3::LoginType},
            error::ErrorKind,
        },
        events::{
            AnyMessageLikeEventContent, AnySyncMessageLikeEvent, AnySyncTimelineEvent,
            InitialStateEvent, SyncMessageLikeEvent,
            room::{encryption::RoomEncryptionEventContent, message::Relation},
        },
    },
};
use serde::Deserialize;
use serde_json::{Value, json};
use tokio::sync::{Mutex, RwLock, Semaphore};
use tokio_util::{sync::CancellationToken, task::TaskTracker};
use url::Url;
use zeroize::Zeroizing;

use crate::{
    media,
    protocol::{INVALID, NETWORK, Output, Result, RpcError, params},
    storage::{SavedSession, Store},
    timeline::{Attachment, Timelines},
    verification::Verifications,
};

const EXPIRED: RpcError = RpcError::new(
    "session_expired",
    "Saved session has expired or been revoked; no new device was created",
);
const CORRUPT: RpcError = RpcError::new(
    "session_corrupt",
    "Saved session or SDK store is corrupt; no new device was created",
);

pub struct Backend {
    store: Arc<Store>,
    output: Output,
    timeline_cancel: CancellationToken,
    avatar_slots: Semaphore,
    account: RwLock<Option<Account>>,
}

struct Account {
    client: Client,
    generation: String,
    timelines: Arc<Timelines>,
    verifications: Arc<Verifications>,
    status: Arc<RwLock<&'static str>>,
    expired: Arc<AtomicBool>,
    cancel: CancellationToken,
    tasks: TaskTracker,
    // Serialize expensive key mutations, not sync or ordinary requests.
    keys: Mutex<()>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Login {
    user_id: OwnedUserId,
    password: String,
    homeserver: Option<String>,
    device_name: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RoomParam {
    room_id: OwnedRoomId,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct JoinRoomParam {
    room_id: OwnedRoomOrAliasId,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SubscriptionParam {
    room_id: OwnedRoomId,
    subscription_id: String,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PaginateParam {
    room_id: OwnedRoomId,
    subscription_id: String,
    limit: u16,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct AvatarParam {
    uri: OwnedMxcUri,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ImageParam {
    room_id: OwnedRoomId,
    subscription_id: String,
    event_id: OwnedEventId,
    source_id: String,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct SendParam {
    room_id: OwnedRoomId,
    subscription_id: String,
    body: String,
    #[serde(default)]
    attachments: Vec<Attachment>,
    reply_to: Option<OwnedEventId>,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct EditParam {
    room_id: OwnedRoomId,
    subscription_id: String,
    event_id: OwnedEventId,
    body: String,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RedactParam {
    room_id: OwnedRoomId,
    subscription_id: String,
    event_id: OwnedEventId,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct CreateParam {
    name: String,
    #[serde(default)]
    invite: Vec<OwnedUserId>,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct DeviceParam {
    user_id: OwnedUserId,
    device_id: OwnedDeviceId,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct FlowParam {
    user_id: OwnedUserId,
    flow_id: String,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct KeyFileParam {
    path: PathBuf,
    passphrase: String,
}
#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RecoverParam {
    recovery_key: String,
}

impl Backend {
    pub fn new(store: Store, output: Output, timeline_cancel: CancellationToken) -> Arc<Self> {
        Arc::new(Self {
            store: Arc::new(store),
            output,
            timeline_cancel,
            avatar_slots: Semaphore::new(4),
            account: RwLock::new(None),
        })
    }

    pub async fn dispatch(&self, method: &str, value: Value) -> Result<Value> {
        match method {
            "session.login" | "session.restore" => {
                let mut slot = self.account.write().await;
                if slot.is_some() {
                    return Err(RpcError::new(
                        "session_active",
                        "An account session is already active",
                    ));
                }
                let login = if method == "session.login" {
                    Some(params::<Login>(value)?)
                } else {
                    empty(value)?;
                    None
                };
                self.output
                    .event(json!({"type":"status", "state":"connecting"}))
                    .await;
                let result = if let Some(login) = login {
                    self.login(login).await
                } else {
                    self.restore().await
                };
                match result {
                    Ok(account) => {
                        let info = account.info().await;
                        let mut event = info.clone();
                        event["type"] = json!("session");
                        self.output.event(event).await;
                        *slot = Some(account);
                        Ok(info)
                    }
                    Err(error) => {
                        self.output
                            .event(
                                json!({"type":"status", "state":"error", "message":error.message}),
                            )
                            .await;
                        Err(error)
                    }
                }
            }
            "session.logout" => {
                empty(value)?;
                let mut slot = self.account.write().await;
                let account = slot.as_ref().ok_or(no_session())?;
                if let Err(error) = account.client.matrix_auth().logout().await {
                    if !unknown_token(error.client_api_error_kind()) {
                        return Err(NETWORK);
                    }
                }
                account.shutdown().await;
                *slot = None;
                self.store.logout()?;
                self.output
                    .event(json!({"type":"status", "state":"offline", "message":"Logged out"}))
                    .await;
                Ok(json!({}))
            }
            _ => {
                // Durable sends/key operations keep an identity lease. View reads
                // have subscription lifetimes, so logout can revoke them.
                let slot = self.account.read().await;
                let account = slot.as_ref().ok_or(no_session())?;
                if method == "session.info" {
                    empty(value)?;
                    return Ok(account.info().await);
                }
                if account.expired.load(Ordering::Acquire) {
                    return Err(EXPIRED);
                }
                if matches!(method, "timeline.subscribe" | "timeline.unsubscribe") {
                    let args: SubscriptionParam = params(value)?;
                    if args.subscription_id.is_empty() || args.subscription_id.len() > 128 {
                        return Err(INVALID);
                    }
                    let client = (method == "timeline.subscribe").then(|| account.client.clone());
                    let timelines = account.timelines.clone();
                    drop(slot);
                    if let Some(client) = client {
                        timelines
                            .subscribe(
                                &client,
                                self.output.clone(),
                                args.room_id,
                                args.subscription_id,
                            )
                            .await?;
                    } else {
                        timelines
                            .unsubscribe(&args.room_id, &args.subscription_id)
                            .await;
                    }
                    return Ok(json!({}));
                }
                if method == "timeline.read" {
                    let args: RedactParam = params(value)?;
                    let timelines = account.timelines.clone();
                    drop(slot);
                    timelines.read_through(&args.room_id, &args.subscription_id, &args.event_id)
                        .await?;
                    return Ok(json!({}));
                }
                if method == "timeline.paginate" {
                    let args: PaginateParam = params(value)?;
                    let timelines = account.timelines.clone();
                    drop(slot);
                    let hit_start = timelines
                        .paginate(&args.room_id, &args.subscription_id, args.limit)
                        .await?;
                    return Ok(json!({"hit_start":hit_start}));
                }
                if method == "media.avatar" {
                    let args: AvatarParam = params(value)?;
                    if args.uri.as_str().len() > 4096 || !args.uri.is_valid() {
                        return Err(INVALID);
                    }
                    let client = account.client.clone();
                    let generation = account.generation.clone();
                    let cancel = account.cancel.clone();
                    let _operation = account.tasks.token();
                    drop(slot);
                    let path = media::avatar(
                        self.store.clone(),
                        client,
                        generation,
                        args.uri,
                        cancel,
                        self.timeline_cancel.clone(),
                        &self.avatar_slots,
                    )
                    .await?;
                    return Ok(json!({"path":path}));
                }
                if method == "media.image" {
                    let args: ImageParam = params(value)?;
                    let client = account.client.clone();
                    let generation = account.generation.clone();
                    let cancel = account.cancel.clone();
                    let timelines = account.timelines.clone();
                    let _operation = account.tasks.token();
                    drop(slot);
                    let lease = timelines.image_lease(
                        &args.room_id, &args.subscription_id, &args.event_id, args.source_id,
                    ).await?;
                    let path = media::image(
                        self.store.clone(),
                        client,
                        generation,
                        &timelines,
                        &lease,
                        cancel,
                        self.timeline_cancel.clone(),
                        &self.avatar_slots,
                    ).await?;
                    return Ok(json!({"path":path}));
                }
                account.dispatch(&self.store, method, value).await
            }
        }
    }

    async fn login(&self, login: Login) -> Result<Account> {
        self.store.may_login()?;
        let password = Zeroizing::new(login.password);
        if password.is_empty()
            || password.len() > 16384
            || login.device_name.as_ref().is_some_and(|s| s.len() > 128)
        {
            return Err(INVALID);
        }
        let mut builder = client_builder()?;
        if let Some(homeserver) = login.homeserver {
            builder = builder.homeserver_url(validate_homeserver(&homeserver)?);
        } else {
            builder = builder.server_name(login.user_id.server_name());
        }
        // Discover and inspect supported native authentication before creating any store.
        let discovery = builder.build().await.map_err(|_| {
            RpcError::new(
                "discovery_failed",
                "Unable to discover a supported Matrix homeserver",
            )
        })?;
        let homeserver = validate_homeserver(discovery.homeserver().as_str())?;
        let flows = discovery
            .matrix_auth()
            .get_login_types()
            .await
            .map_err(|_| NETWORK)?;
        if !flows
            .flows
            .iter()
            .any(|flow| matches!(flow, LoginType::Password(_)))
        {
            return Err(RpcError::new(
                "unsupported_login",
                "This homeserver does not offer native password login",
            ));
        }
        drop(discovery);
        let generation = self.store.begin_login()?;
        let client = client_builder()?
            .homeserver_url(homeserver)
            .sqlite_store(self.store.sdk_path(&generation)?, None)
            .build()
            .await
            .map_err(|_| CORRUPT)?;
        let auth = client.matrix_auth();
        let request = auth
            .login_username(login.user_id.as_str(), &password)
            .device_id(&generation)
            .initial_device_display_name(login.device_name.as_deref().unwrap_or("Matrix AppKit"));
        if let Err(error) = request.send().await {
            if client.matrix_auth().session().is_none()
                && matches!(
                    error.client_api_error_kind(),
                    Some(
                        ErrorKind::Forbidden
                            | ErrorKind::UserDeactivated
                            | ErrorKind::UserLocked
                            | ErrorKind::UserSuspended
                    )
                )
            {
                self.store.login_rejected()?;
                return Err(RpcError::new(
                    "login_rejected",
                    "Homeserver rejected password login",
                ));
            }
            return Err(RpcError::new(
                "login_uncertain",
                "Login response was not received; review your devices before trying a new profile",
            ));
        }
        drop(password);
        let session = client.matrix_auth().session().ok_or(CORRUPT)?;
        if session.meta.user_id != login.user_id || session.meta.device_id.as_str() != generation {
            return Err(RpcError::new(
                "identity_mismatch",
                "Homeserver returned a different account or device identity",
            ));
        }
        let fingerprint = client.encryption().ed25519_key().await.ok_or(CORRUPT)?;
        self.store.save(&SavedSession {
            version: 1,
            homeserver: client.homeserver().to_string(),
            generation: generation.clone(),
            fingerprint,
            session,
        })?;
        Account::start(
            client,
            generation,
            self.output.clone(),
            self.timeline_cancel.child_token(),
        )
        .await
    }

    async fn restore(&self) -> Result<Account> {
        let saved = self.store.load()?;
        let expected_user = saved.session.meta.user_id.clone();
        let expected_device = saved.session.meta.device_id.clone();
        let client = client_builder()?
            .homeserver_url(validate_homeserver(&saved.homeserver)?)
            .sqlite_store(self.store.sdk_path(&saved.generation)?, None)
            .build()
            .await
            .map_err(|_| CORRUPT)?;
        client
            .restore_session(saved.session)
            .await
            .map_err(|_| CORRUPT)?;
        if client.encryption().ed25519_key().await.as_deref() != Some(saved.fingerprint.as_str()) {
            return Err(CORRUPT);
        }
        match client.whoami().await {
            Ok(identity) => {
                if identity.user_id != expected_user
                    || identity.device_id.as_ref() != Some(&expected_device)
                {
                    return Err(RpcError::new(
                        "identity_mismatch",
                        "Saved session does not match its server device identity",
                    ));
                }
            }
            Err(error) if unknown_token(error.client_api_error_kind()) => return Err(EXPIRED),
            Err(_) => {
                return Err(RpcError::new(
                    "restore_offline",
                    "Could not validate saved session with homeserver; retry restore when connected",
                ));
            }
        }
        // Finish a session-save interrupted after atomic publication but before
        // its authority marker/pending marker cleanup.
        self.store.save(&SavedSession {
            version: 1,
            homeserver: saved.homeserver,
            generation: saved.generation.clone(),
            fingerprint: saved.fingerprint,
            session: client.matrix_auth().session().ok_or(CORRUPT)?,
        })?;
        Account::start(
            client,
            saved.generation,
            self.output.clone(),
            self.timeline_cancel.child_token(),
        )
        .await
    }

    pub async fn shutdown(&self) {
        let mut account = self.account.write().await;
        if let Some(account) = account.take() {
            account.shutdown().await;
        }
    }
}

impl Account {
    async fn start(
        client: Client,
        generation: String,
        output: Output,
        timeline_cancel: CancellationToken,
    ) -> Result<Self> {
        client.event_cache().subscribe().map_err(|_| CORRUPT)?;
        client
            .send_queue()
            .respawn_tasks_for_rooms_with_unsent_requests()
            .await;
        let cancel = CancellationToken::new();
        let status = Arc::new(RwLock::new("syncing"));
        let expired = Arc::new(AtomicBool::new(false));
        let tasks = TaskTracker::new();
        let verifications =
            Verifications::new(client.clone(), output.clone(), cancel.child_token());
        let sync_client = client.clone();
        let sync_cancel = cancel.clone();
        let sync_status = status.clone();
        let sync_expired = expired.clone();
        tasks.spawn(async move {
            output.event(json!({"type":"status", "state":"syncing"})).await;
            let mut backoff = 1u64;
            let mut previous_rooms = Value::Null;
            loop {
                let result = tokio::select! {
                    _ = sync_cancel.cancelled() => break,
                    result = sync_client.sync_once(SyncSettings::default()) => result,
                };
                match result {
                    Ok(_) => {
                        backoff = 1;
                        let mut state = sync_status.write().await;
                        if *state != "online" {
                            *state = "online";
                            output.event(json!({"type":"status", "state":"online"})).await;
                        }
                        drop(state);
                        let rooms = room_summaries(&sync_client).await;
                        if rooms != previous_rooms {
                            output.event(json!({"type":"rooms", "rooms":rooms})).await;
                            previous_rooms = rooms;
                        }
                    }
                    Err(error) if unknown_token(error.client_api_error_kind()) => {
                        sync_expired.store(true, Ordering::Release);
                        *sync_status.write().await = "error";
                        output.event(json!({"type":"status", "state":"error", "message":EXPIRED.message})).await;
                        break;
                    }
                    Err(_) => {
                        *sync_status.write().await = "offline";
                        output.event(json!({"type":"status", "state":"offline", "message":"Sync failed; retrying with bounded backoff"})).await;
                        tokio::select! { _ = sync_cancel.cancelled() => break, _ = tokio::time::sleep(Duration::from_secs(backoff)) => {} }
                        backoff = (backoff * 2).min(60);
                    }
                }
            }
        });
        Ok(Self {
            client,
            generation,
            timelines: Arc::new(Timelines::new(timeline_cancel)),
            verifications,
            status,
            expired,
            cancel,
            tasks,
            keys: Mutex::new(()),
        })
    }

    async fn info(&self) -> Value {
        json!({"user_id":self.client.user_id(), "device_id":self.client.device_id(),
            "homeserver":self.client.homeserver().as_str(), "connected":*self.status.read().await})
    }

    async fn dispatch(&self, store: &Store, method: &str, value: Value) -> Result<Value> {
        match method {
            "rooms.list" => {
                empty(value)?;
                Ok(json!({"rooms":room_summaries(&self.client).await}))
            }
            "room.create" => {
                let args: CreateParam = params(value)?;
                if args.name.trim().is_empty() || args.name.len() > 255 || args.invite.len() > 32 {
                    return Err(INVALID);
                }
                let mut request = create_room::v3::Request::new();
                request.name = Some(args.name);
                request.invite = args.invite;
                request.preset = Some(create_room::v3::RoomPreset::PrivateChat);
                request.initial_state.push(
                    InitialStateEvent::with_empty_state_key(
                        RoomEncryptionEventContent::with_recommended_defaults(),
                    )
                    .to_raw_any(),
                );
                let room = self
                    .client
                    .create_room(request)
                    .await
                    .map_err(|_| NETWORK)?;
                Ok(json!({"room_id":room.room_id()}))
            }
            "room.join" => {
                let args: JoinRoomParam = params(value)?;
                let room = self
                    .client
                    .join_room_by_id_or_alias(&args.room_id, &[])
                    .await
                    .map_err(|_| NETWORK)?;
                Ok(json!({"room_id":room.room_id()}))
            }
            "room.leave" => {
                let args: RoomParam = params(value)?;
                let room = self
                    .client
                    .get_room(&args.room_id)
                    .ok_or(RpcError::new("room_missing", "Room is not known"))?;
                room.leave().await.map_err(|_| NETWORK)?;
                Ok(json!({}))
            }
            "message.send" => {
                let args: SendParam = params(value)?;
                self.timelines
                    .send(&args.room_id, &args.subscription_id, args.body,
                        args.attachments, args.reply_to)
                    .await
            }
            "message.edit" => {
                let args: EditParam = params(value)?;
                self.timelines
                    .edit(&args.room_id, &args.subscription_id, &args.event_id, args.body)
                    .await?;
                Ok(json!({}))
            }
            "message.redact" => {
                let args: RedactParam = params(value)?;
                self.timelines
                    .redact(&args.room_id, &args.subscription_id, &args.event_id)
                    .await?;
                Ok(json!({}))
            }
            "devices.list" => {
                empty(value)?;
                let devices = self.client.devices().await.map_err(|_| NETWORK)?;
                let own = self.client.user_id().ok_or(no_session())?;
                let mut result = Vec::with_capacity(devices.devices.len());
                for device in devices.devices {
                    let crypto = self
                        .client
                        .encryption()
                        .get_device(own, &device.device_id)
                        .await
                        .map_err(|_| NETWORK)?;
                    result.push(json!({"device_id":device.device_id, "display_name":device.display_name,
                        "verified":crypto.is_some_and(|d| d.is_verified()), "is_self":self.client.device_id() == Some(device.device_id.as_ref())}));
                }
                Ok(json!({"devices":result}))
            }
            "verification.request" => {
                let args: DeviceParam = params(value)?;
                self.verifications
                    .request(&args.user_id, &args.device_id)
                    .await
            }
            "verification.accept" | "verification.confirm" | "verification.cancel" => {
                let args: FlowParam = params(value)?;
                if args.flow_id.is_empty() || args.flow_id.len() > 1024 {
                    return Err(INVALID);
                }
                match method {
                    "verification.accept" => {
                        self.verifications
                            .accept(&args.user_id, &args.flow_id)
                            .await?
                    }
                    "verification.confirm" => {
                        self.verifications
                            .confirm(&args.user_id, &args.flow_id)
                            .await?
                    }
                    _ => {
                        self.verifications
                            .cancel(&args.user_id, &args.flow_id)
                            .await?
                    }
                }
                Ok(json!({}))
            }
            "keys.export" | "keys.import" => {
                let args: KeyFileParam = params(value)?;
                let passphrase = Zeroizing::new(args.passphrase);
                if passphrase.is_empty() || passphrase.len() > 16384 || !args.path.is_absolute() {
                    return Err(INVALID);
                }
                let _guard = self.keys.lock().await;
                if method == "keys.export" {
                    self.export_keys(store, &args.path, &passphrase).await
                } else {
                    let import = snapshot_key_import(store.root().to_owned(), args.path).await?;
                    let result = self
                        .client
                        .encryption()
                        .import_room_keys(import.path().join("encrypted.keys"), &passphrase)
                        .await
                        .map_err(|_| {
                            RpcError::new(
                                "key_import_failed",
                                "Cannot decrypt or import the encrypted key file",
                            )
                        })?;
                    let sessions: Vec<String> = result
                        .keys
                        .values()
                        .flat_map(|users| users.values())
                        .flatten()
                        .cloned()
                        .collect();
                    self.timelines.retry_decryption(&sessions).await;
                    Ok(json!({"imported":result.imported_count, "total":result.total_count}))
                }
            }
            "keys.recover" => {
                let args: RecoverParam = params(value)?;
                let recovery_key = Zeroizing::new(args.recovery_key);
                if recovery_key.is_empty() || recovery_key.len() > 16384 {
                    return Err(INVALID);
                }
                let _guard = self.keys.lock().await;
                self.client.encryption().recovery().recover(&recovery_key).await
                    .map_err(|_| RpcError::new("key_recovery_failed", "Recovery failed; check the recovery key and existing server recovery setup"))?;
                Ok(json!({}))
            }
            _ => Err(RpcError::new(
                "method_not_found",
                "Unknown Matrix RPC method",
            )),
        }
    }

    async fn export_keys(&self, store: &Store, path: &Path, passphrase: &str) -> Result<Value> {
        let error = RpcError::new(
            "key_export_failed",
            "Cannot export keys; select a new file outside account storage",
        );
        let parent = path
            .parent()
            .ok_or(INVALID)?
            .canonicalize()
            .map_err(|_| error)?;
        if parent.starts_with(store.root().canonicalize().map_err(|_| error)?) || path.exists() {
            return Err(error);
        }
        let temp = tempfile::Builder::new()
            .prefix(".matrix-keys-")
            .tempdir_in(&parent)
            .map_err(|_| error)?;
        let encrypted = temp.path().join("encrypted.keys");
        self.client
            .encryption()
            .export_room_keys(encrypted.clone(), passphrase, |_| true)
            .await
            .map_err(|_| error)?;
        std::fs::File::open(&encrypted)
            .and_then(|file| file.sync_all())
            .map_err(|_| error)?;
        // Atomic, no-clobber publication: existing destinations (including symlinks) are never replaced.
        std::fs::hard_link(&encrypted, path).map_err(|_| error)?;
        std::fs::File::open(&parent)
            .and_then(|file| file.sync_all())
            .map_err(|_| error)?;
        Ok(json!({}))
    }

    async fn shutdown(&self) {
        self.cancel.cancel();
        self.timelines.shutdown().await;
        self.verifications.shutdown().await;
        self.tasks.close();
        self.tasks.wait().await;
        if self.client.pause().await.is_err() {
            eprintln!("Matrix account stores could not close cleanly");
        }
    }
}

async fn snapshot_key_import(root: PathBuf, source: PathBuf) -> Result<tempfile::TempDir> {
    let error = RpcError::new(
        "key_import_file",
        "Key import requires a regular encrypted file no larger than 128 MiB",
    );
    tokio::task::spawn_blocking(move || {
        use std::{fs::OpenOptions, io::Read, os::unix::fs::OpenOptionsExt};
        let file = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
            .open(source)
            .map_err(|_| error)?;
        let metadata = file.metadata().map_err(|_| error)?;
        const MAX_IMPORT: u64 = 128 * 1024 * 1024;
        if !metadata.is_file() || metadata.len() > MAX_IMPORT {
            return Err(error);
        }
        let temporary = tempfile::Builder::new()
            .prefix(".key-import-")
            .tempdir_in(root)
            .map_err(|_| error)?;
        let mut target = OpenOptions::new()
            .write(true)
            .create_new(true)
            .mode(0o600)
            .open(temporary.path().join("encrypted.keys"))
            .map_err(|_| error)?;
        if std::io::copy(&mut file.take(MAX_IMPORT + 1), &mut target).map_err(|_| error)?
            > MAX_IMPORT
        {
            return Err(error);
        }
        Ok(temporary)
    })
    .await
    .map_err(|_| error)?
}

fn summary_text(text: &str) -> &str {
    &text[..text
        .char_indices()
        .nth(512)
        .map_or(text.len(), |(index, _)| index)]
}

async fn room_summaries(client: &Client) -> Value {
    let latest_events = client.latest_events().await;
    let known_rooms = client.rooms();
    let mut rooms = Vec::with_capacity(known_rooms.len());
    for room in known_rooms {
        let membership = match room.state() {
            RoomState::Joined => "join",
            RoomState::Invited => "invite",
            RoomState::Left => "leave",
            _ => continue,
        };
        let name = room
            .name()
            .or_else(|| room.cached_display_name().map(|n| n.to_string()))
            .unwrap_or_else(|| room.room_id().to_string());
        let mut summary = json!({
            "room_id":room.room_id(), "name":summary_text(&name),
            "membership":membership, "encrypted":room.encryption_state().is_encrypted(),
            "unread":room.num_unread_notifications()
        });
        // The SDK room avatar wins. For a direct room with one other hero,
        // use that member's SDK profile without fetching membership or media.
        let mut avatar_url = room.avatar_url();
        if avatar_url.is_none() && room.is_direct().await.unwrap_or(false) {
            let heroes = room.heroes().await;
            let mut others = heroes
                .iter()
                .filter(|hero| hero.user_id != room.own_user_id());
            if let Some(hero) = others.next()
                && others.next().is_none()
            {
                avatar_url = hero.avatar_url.clone();
            }
        }
        if let Some(uri) = avatar_url {
            summary["avatar_url"] = json!(uri);
        }
        // The SDK computes these lazily from its event cache/send queue. Automatic
        // backfill stays disabled: an absent/unavailable preview is simply omitted.
        // Initial computation can complete after this snapshot; normal room-summary
        // updates will pick it up, without maintaining another history cache.
        if latest_events.listen_to_room(room.room_id()).await.is_ok() {
            let latest = room.latest_event();
            if let Some(timestamp) = latest.timestamp() {
                summary["timestamp"] = json!(timestamp);
            }
            let message = match latest {
                LatestEventValue::Remote(event) => {
                    if let Ok(AnySyncTimelineEvent::MessageLike(
                        AnySyncMessageLikeEvent::RoomMessage(SyncMessageLikeEvent::Original(event)),
                    )) = event.raw().deserialize()
                    {
                        summary["preview_sender"] = json!(event.sender);
                        Some(event.content)
                    } else {
                        None
                    }
                }
                LatestEventValue::LocalIsSending(value)
                | LatestEventValue::LocalCannotBeSent(value)
                | LatestEventValue::LocalHasBeenSent { value, .. } => {
                    if let Ok(AnyMessageLikeEventContent::RoomMessage(content)) =
                        value.content.deserialize()
                    {
                        summary["preview_sender"] = json!(room.own_user_id());
                        Some(content)
                    } else {
                        None
                    }
                }
                _ => None,
            };
            if let Some(content) = message {
                let body = match &content.relates_to {
                    Some(Relation::Replacement(replacement)) => {
                        replacement.new_content.msgtype.body()
                    }
                    _ => content.msgtype.body(),
                };
                summary["preview"] = json!(summary_text(body));
            }
        }
        rooms.push(summary);
    }
    rooms.sort_by(|a, b| a["room_id"].as_str().cmp(&b["room_id"].as_str()));
    Value::Array(rooms)
}

fn client_builder() -> Result<matrix_sdk::ClientBuilder> {
    // Do not follow redirects carrying a password, token or key to another endpoint.
    let http = reqwest::Client::builder()
        .redirect(reqwest::redirect::Policy::none())
        .connect_timeout(Duration::from_secs(15))
        .timeout(Duration::from_secs(60))
        .user_agent("matrix-appkit/0.1")
        .build()
        .map_err(|_| NETWORK)?;
    Ok(Client::builder()
        .http_client(http)
        .respect_login_well_known(false)
        .with_enable_automatic_back_pagination(false)
        .request_config(
            RequestConfig::default()
                .retry_limit(2)
                .timeout(Duration::from_secs(60)),
        )
        .with_encryption_settings(EncryptionSettings {
            auto_enable_cross_signing: false,
            auto_enable_backups: false,
            backup_download_strategy: BackupDownloadStrategy::OneShot,
        }))
}

fn validate_homeserver(value: &str) -> Result<Url> {
    let url = Url::parse(value).map_err(|_| INVALID)?;
    let loopback = match url.host() {
        Some(url::Host::Domain(name)) => name == "localhost",
        Some(url::Host::Ipv4(ip)) => ip.is_loopback(),
        Some(url::Host::Ipv6(ip)) => ip.is_loopback(),
        None => false,
    };
    if !(url.scheme() == "https" || url.scheme() == "http" && loopback)
        || !url.username().is_empty()
        || url.password().is_some()
        || url.query().is_some()
        || url.fragment().is_some()
    {
        return Err(RpcError::new(
            "unsafe_homeserver",
            "Homeserver must use HTTPS (HTTP is allowed only for loopback) and contain no credentials",
        ));
    }
    Ok(url)
}

fn unknown_token(kind: Option<&ErrorKind>) -> bool {
    matches!(kind, Some(ErrorKind::UnknownToken(_)))
}
fn no_session() -> RpcError {
    RpcError::new(
        "session_required",
        "Log in or restore an account session first",
    )
}
fn empty(value: Value) -> Result<()> {
    if value.as_object().is_some_and(|v| v.is_empty()) {
        Ok(())
    } else {
        Err(INVALID)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn image_rpc_accepts_only_timeline_scoped_identity_not_media_urls_or_keys() {
        let value = json!({
            "room_id":"!room:example.org", "subscription_id":"active",
            "event_id":"$image", "source_id":"0".repeat(64)
        });
        let args: ImageParam = params(value.clone()).unwrap();
        assert_eq!(args.event_id.as_str(), "$image");
        for (field, content) in [
            ("uri", json!("mxc://example.org/image")),
            ("url", json!("https://example.org/image")),
            ("file", json!({"key":"secret"})),
        ] {
            let mut extra = value.clone();
            extra[field] = content;
            assert!(params::<ImageParam>(extra).is_err());
        }
        for field in ["room_id", "subscription_id", "event_id", "source_id"] {
            let mut missing = value.clone();
            missing.as_object_mut().unwrap().remove(field);
            assert!(params::<ImageParam>(missing).is_err());
        }
    }

    #[tokio::test]
    async fn import_snapshot_rejects_symlinks_and_nonregular_files() {
        let directory = tempfile::tempdir().unwrap();
        let source = tempfile::NamedTempFile::new().unwrap();
        let link = directory.path().join("linked.keys");
        std::os::unix::fs::symlink(source.path(), &link).unwrap();
        assert!(
            snapshot_key_import(directory.path().into(), link)
                .await
                .is_err()
        );
        assert!(
            snapshot_key_import(directory.path().into(), directory.path().into())
                .await
                .is_err()
        );
        assert!(
            snapshot_key_import(directory.path().into(), source.path().into())
                .await
                .is_ok()
        );
    }

    #[test]
    fn homeserver_policy_rejects_credentials_and_remote_plaintext() {
        for url in [
            "https://matrix.org",
            "http://127.0.0.1:8008",
            "http://[::1]:8008",
            "http://localhost:8008",
        ] {
            assert!(validate_homeserver(url).is_ok());
        }
        for url in [
            "http://matrix.org",
            "http://localhost.evil.test",
            "https://token@matrix.org",
            "https://matrix.org/?access_token=secret",
            "file:///tmp/server",
        ] {
            assert!(validate_homeserver(url).is_err());
        }
    }
}
