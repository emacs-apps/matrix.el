use std::{collections::{HashMap, HashSet}, path::PathBuf, sync::Arc};

use eyeball_im::VectorDiff;
use futures_util::{StreamExt, stream::FuturesUnordered};
use matrix_sdk::{
    Client, RoomState,
    attachment::{AttachmentConfig, AttachmentInfo, BaseAudioInfo, BaseFileInfo, BaseImageInfo, BaseVideoInfo},
    room::Receipts,
    room::reply::{EnforceThread, Reply},
    room::edit::{EditError as RoomEditError, EditedContent},
    ruma::{
        EventId, OwnedEventId, OwnedRoomId, OwnedUserId, UInt, UserId,
        api::error::ErrorKind,
        events::{
            MessageLikeEventType, Mentions,
            room::{
                MediaSource,
                message::{AddMentions, MessageType, RoomMessageEventContent, RoomMessageEventContentWithoutRelation, TextMessageEventContent},
                power_levels::RoomPowerLevels,
            },
        },
    },
};
use matrix_sdk_ui::timeline::{
    EditError, Error as TimelineError, EventSendState, EventTimelineItem, Message, RedactError,
    InReplyToDetails, RoomExt, Timeline, TimelineDetails, TimelineItem, VirtualTimelineItem,
};
use serde_json::{Value, json};
use tokio::{io::AsyncReadExt, sync::{Mutex, OnceCell}};
use tokio_util::{
    sync::CancellationToken,
    task::{TaskTracker, task_tracker::TaskTrackerToken},
};

use crate::describe;
use crate::protocol::{INVALID, NETWORK, Output, Result, RpcError};
use crate::{media, storage::valid_source_id};

const MAX_SUBSCRIPTIONS: usize = 32;
const MAX_REPLY_FETCHES: usize = 4;
const ATTACHMENT_READ_FAILED: RpcError = RpcError::new(
    "attachment_read_failed", "Cannot read an attachment; select a readable regular local file",
);
const SEND_FAILED: RpcError = RpcError::new(
    "send_failed", "SDK could not queue this message; check room access and device verification",
);
const REPLY_UNAVAILABLE: RpcError = RpcError::new(
    "reply_unavailable", "This event is no longer available for replies in this timeline",
);

/// A run of composer text; a mention names the user it pills.
#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Segment {
    text: String,
    user_id: Option<OwnedUserId>,
}

fn escape_html(text: &str) -> String {
    let mut escaped = String::with_capacity(text.len());
    for character in text.chars() {
        match character {
            '&' => escaped.push_str("&amp;"),
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '"' => escaped.push_str("&quot;"),
            '\n' => escaped.push_str("<br>"),
            _ => escaped.push(character),
        }
    }
    escaped
}

/// Message text for BODY: the composer SEGMENTS' mentions become pills in an
/// HTML body and intentional m.mentions.  Mentions are always explicit, so a
/// name typed as plain text never pings anyone through legacy push rules.
fn compose_text(body: String, segments: &[Segment]) -> Result<(TextMessageEventContent, Mentions)> {
    if !segments.iter().any(|segment| segment.user_id.is_some()) {
        return Ok((TextMessageEventContent::plain(body), Mentions::new()));
    }
    if segments.len() > 1024
        || segments.iter().map(|segment| segment.text.as_str()).collect::<String>() != body
    {
        return Err(INVALID);
    }
    let mut html = String::new();
    let mut users = Vec::new();
    for segment in segments {
        match &segment.user_id {
            Some(user) if !segment.text.is_empty() => {
                html.push_str(&format!(
                    "<a href=\"https://matrix.to/#/{}\">{}</a>",
                    escape_html(user.as_str()), escape_html(&segment.text)
                ));
                users.push(user.clone());
            }
            Some(_) => return Err(INVALID),
            None => html.push_str(&escape_html(&segment.text)),
        }
    }
    Ok((TextMessageEventContent::html(body, html), Mentions::with_user_ids(users)))
}

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Attachment {
    path: PathBuf,
    filename: String,
    mime_type: String,
    width: Option<u64>,
    height: Option<u64>,
}

struct PreparedAttachment {
    file: tokio::fs::File,
    filename: String,
    mime: mime::Mime,
    width: Option<UInt>,
    height: Option<UInt>,
    capacity: usize,
}

impl Attachment {
    async fn prepare(self) -> Result<PreparedAttachment> {
        if !self.path.is_absolute() || self.filename.trim().is_empty()
            || self.filename.contains(['/', '\\', '\0'])
        {
            return Err(INVALID);
        }
        let mime: mime::Mime = self.mime_type.parse().map_err(|_| INVALID)?;
        if mime.type_() == mime::STAR || mime.subtype() == mime::STAR {
            return Err(INVALID);
        }
        let dimension = |value: u64| {
            if value == 0 { return Err(INVALID); }
            UInt::try_from(value).map_err(|_| INVALID)
        };
        let width = self.width.map(dimension).transpose()?;
        let height = self.height.map(dimension).transpose()?;
        // Open each source before queue admission, but retain handles, not every
        // file's bytes. O_NONBLOCK prevents a replaced FIFO from hanging admission.
        let file = tokio::fs::OpenOptions::new().read(true).custom_flags(libc::O_NONBLOCK)
            .open(&self.path).await.map_err(|_| ATTACHMENT_READ_FAILED)?;
        let metadata = file.metadata().await.map_err(|_| ATTACHMENT_READ_FAILED)?;
        if !metadata.is_file() {
            return Err(ATTACHMENT_READ_FAILED);
        }
        UInt::try_from(metadata.len()).map_err(|_| INVALID)?;
        let capacity = usize::try_from(metadata.len()).map_err(|_| INVALID)?;
        Ok(PreparedAttachment { file, filename: self.filename, mime, width, height, capacity })
    }
}

impl PreparedAttachment {
    fn info(&self, size: UInt) -> AttachmentInfo {
        match self.mime.type_() {
            mime::IMAGE => AttachmentInfo::Image(BaseImageInfo {
                size: Some(size), width: self.width, height: self.height, ..Default::default()
            }),
            mime::VIDEO => AttachmentInfo::Video(BaseVideoInfo {
                size: Some(size), width: self.width, height: self.height, ..Default::default()
            }),
            mime::AUDIO => AttachmentInfo::Audio(BaseAudioInfo {
                size: Some(size), ..Default::default()
            }),
            _ => AttachmentInfo::File(BaseFileInfo { size: Some(size) }),
        }
    }
}

fn send_outcome(accepted: usize, total: usize, error: Option<RpcError>) -> Result<Value> {
    if let Some(error) = error {
        if accepted == 0 { return Err(error); }
        return Ok(json!({"accepted":accepted, "total":total,
            "error":{"code":error.code, "message":error.message}}));
    }
    Ok(json!({"accepted":accepted, "total":total}))
}
const STALE_SUBSCRIPTION: RpcError = RpcError::new(
    "stale_subscription",
    "This timeline subscription is no longer active",
);
const PAGINATION_CANCELLED: RpcError = RpcError::new(
    "pagination_cancelled",
    "Timeline pagination was cancelled because its subscription closed",
);

const PERMISSIONS_UNAVAILABLE: RpcError = RpcError::new(
    "permissions_unavailable",
    "Room permissions are not available; wait for sync before changing messages",
);
const EVENT_MISSING: RpcError =
    RpcError::new("event_missing", "The target event is not in this room timeline");
const EDIT_FORBIDDEN: RpcError =
    RpcError::new("edit_forbidden", "You may only edit your own messages with room send permission");
const EDIT_UNAVAILABLE: RpcError = RpcError::new(
    "edit_unavailable",
    "This event has no editable message content; plaintext must be available",
);
const REDACT_FORBIDDEN: RpcError = RpcError::new(
    "redaction_forbidden",
    "Room permissions do not allow you to redact this event",
);
const ALREADY_REDACTED: RpcError =
    RpcError::new("already_redacted", "This event is already redacted");
const REACT_FORBIDDEN: RpcError =
    RpcError::new("reaction_forbidden", "Room permissions do not allow reacting to this event");
// Keys are arbitrary text; bound what one event can project.
const MAX_REACTION_KEY: usize = 256;
const MAX_REACTION_KEYS: usize = 32;

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct MutationPermissions {
    send_message: bool,
    react: bool,
    redact_own: bool,
    redact_other: bool,
}

impl MutationPermissions {
    fn from_power_levels(levels: &RoomPowerLevels, user: &UserId) -> Self {
        Self {
            send_message: levels.user_can_send_message(user, MessageLikeEventType::RoomMessage),
            react: levels.user_can_send_message(user, MessageLikeEventType::Reaction),
            redact_own: levels.user_can_redact_own_event(user),
            redact_other: levels.user_can_redact_event_of_other(user),
        }
    }

    fn can_edit(self, event: &EventTimelineItem) -> bool {
        event.event_id().is_some()
            && event.is_own()
            && event.content().as_message().is_some_and(body_editable)
            && self.send_message
    }

    fn can_react(self, event: &EventTimelineItem) -> bool {
        event.event_id().is_some()
            && event.content().reactions().is_some()
            && !is_redacted(event)
            && self.react
    }

    fn can_redact(self, event: &EventTimelineItem) -> bool {
        event.event_id().is_some()
            && !is_redacted(event)
            && event.redaction_send_state().is_none()
            && if event.is_own() { self.redact_own } else { self.redact_other }
    }
}

async fn mutation_permissions(room: &matrix_sdk::Room) -> Result<MutationPermissions> {
    if room.state() != RoomState::Joined {
        return Err(RpcError::new("room_not_joined", "Join this room before changing messages"));
    }
    // Never grant moderator or sending privileges from guessed/default room state.
    let levels = room.power_levels().await.map_err(|_| PERMISSIONS_UNAVAILABLE)?;
    Ok(MutationPermissions::from_power_levels(&levels, room.own_user_id()))
}

fn body_editable(message: &Message) -> bool {
    matches!(
        message.msgtype(),
        MessageType::Text(_) | MessageType::Notice(_) | MessageType::Emote(_)
            | MessageType::Audio(_) | MessageType::File(_) | MessageType::Image(_)
            | MessageType::Video(_)
    )
}

fn is_redacted(event: &EventTimelineItem) -> bool {
    if event.content().is_redacted()
        || event.content().is_message()
        || event.content().is_unable_to_decrypt()
    {
        return event.content().is_redacted();
    }
    // Redacted state events retain their state-event variant in SDK content.
    // Inspect only the marker, without copying/deserializing event bodies.
    #[derive(serde::Deserialize)]
    struct EventMetadata {
        #[serde(default)]
        unsigned: UnsignedMetadata,
    }
    #[derive(Default, serde::Deserialize)]
    struct UnsignedMetadata {
        redacted_because: Option<serde::de::IgnoredAny>,
    }
    event.original_json().is_some_and(|raw| {
        serde_json::from_str::<EventMetadata>(raw.json().get())
            .is_ok_and(|metadata| metadata.unsigned.redacted_because.is_some())
    })
}

#[derive(Clone, Copy, PartialEq, Eq, serde::Serialize)]
struct ReadState {
    unread_messages: Option<u64>,
    all_read: Option<bool>,
    first_unread_index: Option<usize>,
}

async fn read_state<'a>(
    timeline: &Timeline,
    items: impl Iterator<Item = &'a Arc<TimelineItem>>,
) -> ReadState {
    // SDK counts cover cached events and expose no completeness bit.
    let room = timeline.room();
    let fully_read = room.fully_read_event_id();
    let receipt = timeline.latest_user_read_receipt(room.own_user_id()).await;
    let all_read = timeline.latest_event_id().await.is_some_and(|latest| {
        fully_read.as_ref() == Some(&latest)
            || receipt.as_ref().is_some_and(|(event, _)| *event == latest)
    });
    let receipt_position = if receipt.is_some() {
        timeline.latest_user_read_receipt_timeline_event_id(room.own_user_id()).await
    } else {
        None
    };
    // The SDK maps receipts on hidden events to their visible predecessor.
    let fully_read_position = if receipt.as_ref().is_some_and(|(event, _)| {
        fully_read.as_ref() == Some(event)
    }) {
        receipt_position.as_ref().or(fully_read.as_ref())
    } else {
        fully_read.as_ref()
    };
    let mut found_fully_read = fully_read.is_none();
    let mut found_receipt = receipt.is_none();
    let mut passed_frontier = false;
    let mut first_unread_index = None;
    if !all_read {
        for (index, item) in items.enumerate() {
            let Some(event) = item.as_event() else { continue };
            let Some(event_id) = event.event_id() else { continue };
            let fully_here = fully_read_position.is_some_and(|id| id.as_str() == event_id.as_str());
            let receipt_here = receipt.as_ref().is_some_and(|(id, _)| id.as_str() == event_id.as_str())
                || receipt_position.as_ref().is_some_and(|id| id.as_str() == event_id.as_str());
            if fully_here || receipt_here {
                found_fully_read |= fully_here;
                found_receipt |= receipt_here;
                passed_frontier = true;
                first_unread_index = None;
            } else if passed_frontier && first_unread_index.is_none() && read_capable(event) {
                first_unread_index = Some(index);
            }
        }
    }
    ReadState {
        unread_messages: all_read.then_some(0),
        all_read: all_read.then_some(true),
        // Indices belong to the exact subscriber snapshot emitted in this revision.
        // An unresolved frontier never falls back to an older fully-read marker.
        first_unread_index: if found_fully_read && found_receipt {
            first_unread_index
        } else {
            None
        },
    }
}
struct Subscription {
    id: String,
    timeline: OnceCell<Result<Arc<Timeline>>>,
    cancel: CancellationToken,
    tasks: TaskTracker,
    // Admit mutations and subscription transitions in a single per-room order.
    // Closing/replacing a view waits for already-admitted SDK queue operations,
    // without holding the room map or blocking unrelated rooms.
    mutations: Mutex<()>,
    // Read receipts serialize independently of composer/durable message mutations.
    receipts: Mutex<()>,
}

impl Subscription {
    fn cancel(&self) {
        self.cancel.cancel();
        self.tasks.close();
    }
}

pub struct MediaLease {
    subscription: Arc<Subscription>,
    room_id: OwnedRoomId,
    event_id: OwnedEventId,
    pub source_id: String,
    pub request: matrix_sdk::media::MediaRequestParameters,
    /// Declared byte size, when the event states one.
    pub size: Option<u64>,
    _operation: TaskTrackerToken,
    _subscription_operation: TaskTrackerToken,
}

impl MediaLease {
    pub fn cancellation(&self) -> &CancellationToken {
        &self.subscription.cancel
    }

    pub async fn validate(&self, timelines: &Timelines) -> Result<()> {
        if !timelines.is_current(&self.room_id, &self.subscription).await {
            return Err(STALE_SUBSCRIPTION);
        }
        let timeline = mutation_timeline(&self.subscription)?;
        let items = timeline.items().await;
        let event = items.iter().filter_map(|item| item.as_event())
            .find(|event| event.event_id() == Some(self.event_id.as_ref())).ok_or(EVENT_MISSING)?;
        let (source, _) = event_media(event).ok_or(media::STALE_MEDIA)?;
        if media::source_id(source) != self.source_id {
            return Err(media::STALE_MEDIA);
        }
        if !timelines.is_current(&self.room_id, &self.subscription).await {
            return Err(STALE_SUBSCRIPTION);
        }
        Ok(())
    }
}

pub struct Timelines {
    rooms: Mutex<HashMap<OwnedRoomId, Arc<Subscription>>>,
    cancel: CancellationToken,
    tasks: TaskTracker,
}

impl Timelines {
    pub fn new(cancel: CancellationToken) -> Self {
        Self {
            rooms: Mutex::new(HashMap::new()),
            cancel,
            tasks: TaskTracker::new(),
        }
    }

    // Reserve the identity before any SDK or output await. Only this short critical
    // section is shared across rooms, and in-flight initializations count toward the cap.
    async fn reserve(
        &self,
        room_id: OwnedRoomId,
        id: String,
    ) -> Result<(
        Arc<Subscription>,
        Option<Arc<Subscription>>,
        TaskTrackerToken,
    )> {
        loop {
            let mut rooms = self.rooms.lock().await;
            if self.cancel.is_cancelled() {
                return Err(STALE_SUBSCRIPTION);
            }
            let current = rooms.get(&room_id).cloned();
            if let Some(current) = &current
                && current.id == id && !current.cancel.is_cancelled() {
                    return Ok((current.clone(), None, current.tasks.token()));
                }
            let _transition = match &current {
                Some(current) => match current.mutations.try_lock() {
                    Ok(guard) => Some(guard),
                    Err(_) => {
                        drop(rooms);
                        let _guard = current.mutations.lock().await;
                        continue;
                    }
                },
                None => None,
            };
            if current.is_none() && rooms.len() >= MAX_SUBSCRIPTIONS {
                return Err(RpcError::new(
                    "subscription_limit",
                    "At most 32 room timelines may be open",
                ));
            }
            let subscription = Arc::new(Subscription {
                id,
                timeline: OnceCell::new(),
                cancel: self.cancel.child_token(),
                tasks: TaskTracker::new(),
                mutations: Mutex::new(()),
                receipts: Mutex::new(()),
            });
            let operation = subscription.tasks.token();
            let old = rooms.insert(room_id, subscription.clone());
            if let Some(old) = &old {
                old.cancel();
            }
            return Ok((subscription, old, operation));
        }
    }

    async fn is_current(&self, room_id: &OwnedRoomId, subscription: &Arc<Subscription>) -> bool {
        let rooms = self.rooms.lock().await;
        !subscription.cancel.is_cancelled()
            && rooms
                .get(room_id)
                .is_some_and(|current| Arc::ptr_eq(current, subscription))
    }

    pub async fn subscribe(
        &self,
        client: &Client,
        output: Output,
        room_id: OwnedRoomId,
        id: String,
    ) -> Result<()> {
        let _operation = self.tasks.token();
        let (subscription, old, _subscription_operation) =
            self.reserve(room_id.clone(), id).await?;
        let initialize = async {
            let reusable = if let Some(old) = old {
                old.tasks.wait().await;
                let candidate = old.timeline.get()
                    .and_then(|result| result.as_ref().ok()).cloned();
                if let Some(timeline) = candidate {
                    // SDK fetch cancellation can leave its details Pending. Never
                    // transfer that abandoned waiter to a replacement view.
                    let pending = timeline.items().await.iter().filter_map(|item| item.as_event())
                        .any(|event| event.content().in_reply_to()
                            .is_some_and(|reply| matches!(reply.event, TimelineDetails::Pending)));
                    (!pending).then_some(timeline)
                } else {
                    None
                }
            } else {
                None
            };
            subscription
                .timeline
                .get_or_init(|| async {
                    let timeline = match reusable {
                        Some(timeline) => timeline,
                        None => Arc::new(
                            joined_room(client, &room_id)?
                                .timeline()
                                .await
                                .map_err(|_| NETWORK)?,
                        ),
                    };
                    let task_sdk_room = timeline.room().clone();
                    let mut room_updates = task_sdk_room.subscribe_to_updates();
                    let mut room_info = task_sdk_room.subscribe_info();
                    let receipt_updates = timeline.subscribe_own_user_read_receipts_changed().await;
                    let mut permissions = mutation_permissions(&task_sdk_room).await.unwrap_or_default();
                    let own_user = task_sdk_room.own_user_id().to_owned();
                    let own = &*own_user;
                    let (mut items, stream) = timeline.subscribe().await;
                    let mut reading = read_state(&timeline, items.iter()).await;
                    if !output
                        .event(json!({
                            "type":"timeline_reset", "room_id":room_id,
                            "subscription_id":subscription.id, "revision":0,
                            "items":items.iter().map(|i| item(i, permissions, own)).collect::<Vec<_>>(),
                            "read_state":reading
                        }))
                        .await
                    {
                        return Err(RpcError::new("transport_closed", "Protocol output closed"));
                    }
                    let task_cancel = subscription.cancel.clone();
                    let task_room = room_id.clone();
                    let task_id = subscription.id.clone();
                    let task_timeline = timeline.clone();
                    let (_typing_guard, mut typing_updates) =
                        task_sdk_room.subscribe_to_typing_notifications();
                    self.tasks.spawn(subscription.tasks.track_future(async move {
                        let _typing_guard = _typing_guard;
                        let mut typing_open = true;
                        let own = &*own_user;
                        futures_util::pin_mut!(stream);
                        futures_util::pin_mut!(receipt_updates);
                        let mut revision = 0u64;
                        let mut room_updates_open = true;
                        let mut reply_fetches = FuturesUnordered::new();
                        let mut attempted_replies = HashSet::new();
                        loop {
                            // Only SDK Unavailable details are admitted, once per
                            // source event in this subscription. No HTTP or reply
                            // content index: the SDK vector remains authoritative.
                            for event in items.iter().filter_map(|item| item.as_event()) {
                                if reply_fetches.len() == MAX_REPLY_FETCHES { break; }
                                let Some(event_id) = event.event_id() else { continue; };
                                if !event.content().in_reply_to().is_some_and(|reply|
                                    matches!(reply.event, TimelineDetails::Unavailable))
                                    || attempted_replies.contains(event_id)
                                {
                                    continue;
                                }
                                attempted_replies.insert(event_id.to_owned());
                                let timeline = task_timeline.clone();
                                let event_id = event_id.to_owned();
                                reply_fetches.push(async move {
                                    // SDK publishes Pending/Ready/Error through this
                                    // same vector stream; Error is never auto-retried.
                                    let _ = timeline.fetch_details_for_event(&event_id).await;
                                });
                            }
                            tokio::select! {
                                biased;
                                _ = task_cancel.cancelled() => break,
                                _ = reply_fetches.next(), if !reply_fetches.is_empty() => {}
                                read_update = async {
                                    tokio::select! {
                                        info = room_info.next() => info.is_some(),
                                        receipt = receipt_updates.next() => receipt.is_some(),
                                    }
                                } => {
                                    if !read_update { break; }
                                    let next = read_state(&task_timeline, items.iter()).await;
                                    if next == reading { continue; }
                                    reading = next;
                                    revision += 1;
                                    let event = json!({
                                        "type":"timeline_diff", "room_id":task_room,
                                        "subscription_id":task_id, "revision":revision,
                                        "changes":[], "read_state":reading
                                    });
                                    tokio::select! {
                                        biased;
                                        _ = task_cancel.cancelled() => break,
                                        sent = output.event(event) => if !sent { break; },
                                    }
                                }
                                typing = typing_updates.recv(), if typing_open => {
                                    let users = match typing {
                                        Ok(users) => users,
                                        Err(tokio::sync::broadcast::error::RecvError::Lagged(_)) => continue,
                                        Err(tokio::sync::broadcast::error::RecvError::Closed) => {
                                            typing_open = false;
                                            continue;
                                        }
                                    };
                                    let mut names = Vec::new();
                                    for user in users.iter().filter(|user| *user != own).take(8) {
                                        let member = task_sdk_room.get_member_no_sync(user).await.ok().flatten();
                                        names.push(member.and_then(|member| member.display_name().map(str::to_owned))
                                            .unwrap_or_else(|| user.to_string()));
                                    }
                                    let event = json!({
                                        "type":"typing", "room_id":task_room,
                                        "subscription_id":task_id, "users":names,
                                    });
                                    tokio::select! {
                                        biased;
                                        _ = task_cancel.cancelled() => break,
                                        sent = output.event(event) => if !sent { break; },
                                    }
                                }
                                update = room_updates.recv(), if room_updates_open => {
                                    if matches!(update, Err(tokio::sync::broadcast::error::RecvError::Closed)) {
                                        room_updates_open = false;
                                        continue;
                                    }
                                    let next = mutation_permissions(&task_sdk_room).await.unwrap_or_default();
                                    if next == permissions { continue; }
                                    permissions = next;
                                    revision += 1;
                                    reading = read_state(&task_timeline, items.iter()).await;
                                    // Reserialize our subscriber's SDK snapshot, not a separately
                                    // fetched list that could overtake queued SDK vector diffs.
                                    let event = json!({
                                        "type":"timeline_diff", "room_id":task_room,
                                        "subscription_id":task_id, "revision":revision,
                                        "read_state":reading,
                                        "changes":[{"op":"reset", "items":items.iter()
                                            .map(|i| item(i, permissions, own)).collect::<Vec<_>>()}]
                                    });
                                    tokio::select! {
                                        biased;
                                        _ = task_cancel.cancelled() => break,
                                        sent = output.event(event) => if !sent { break; },
                                    }
                                }
                                diffs = stream.next() => {
                                    let Some(diffs) = diffs else {
                                        tokio::select! {
                                            biased;
                                            _ = task_cancel.cancelled() => {},
                                            _ = output.event(json!({"type":"status", "state":"error", "message":"Timeline stream ended; close and reopen the room"})) => {},
                                        }
                                        break;
                                    };
                                    if diffs.is_empty() { continue; }
                                    revision += 1;
                                    let mut event = json!({
                                        "type":"timeline_diff", "room_id":task_room,
                                        "subscription_id":task_id, "revision":revision,
                                        "changes":diffs.iter().map(|diff| change(diff, permissions, own)).collect::<Vec<_>>()
                                    });
                                    for diff in diffs {
                                        diff.apply(&mut items);
                                    }
                                    reading = read_state(&task_timeline, items.iter()).await;
                                    event["read_state"] = json!(reading);
                                    tokio::select! {
                                        biased;
                                        _ = task_cancel.cancelled() => break,
                                        sent = output.event(event) => if !sent { break; },
                                    }
                                }
                            }
                        }
                        task_cancel.cancel();
                    }));
                    Ok(timeline)
                })
                .await
                .as_ref()
                .map(|_| ())
                .map_err(|error| *error)
        };
        let result = tokio::select! {
            biased;
            _ = subscription.cancel.cancelled() => Err(STALE_SUBSCRIPTION),
            result = initialize => result,
        };
        if result.is_err() {
            // A failed old initializer must never remove its replacement.
            let mut rooms = self.rooms.lock().await;
            if rooms
                .get(&room_id)
                .is_some_and(|current| Arc::ptr_eq(current, &subscription))
            {
                rooms.remove(&room_id);
            }
            subscription.cancel();
        }
        if result.is_ok() && !self.is_current(&room_id, &subscription).await {
            return Err(STALE_SUBSCRIPTION);
        }
        result
    }

    pub async fn unsubscribe(&self, room_id: &OwnedRoomId, id: &str) {
        let _operation = self.tasks.token();
        let old = loop {
            let mut rooms = self.rooms.lock().await;
            let Some(current) = rooms.get(room_id).filter(|s| s.id == id).cloned() else {
                break None;
            };
            let _transition = match current.mutations.try_lock() {
                Ok(guard) => guard,
                Err(_) => {
                    drop(rooms);
                    let _guard = current.mutations.lock().await;
                    continue;
                }
            };
            let old = rooms.remove(room_id).expect("checked subscription");
            old.cancel();
            break Some(old);
        };
        if let Some(old) = old {
            old.tasks.wait().await;
        }
    }

    pub async fn paginate(&self, room_id: &OwnedRoomId, id: &str, limit: u16) -> Result<bool> {
        if id.is_empty() || id.len() > 128 || !(1..=100).contains(&limit) {
            return Err(INVALID);
        }
        let _operation = self.tasks.token();
        let (subscription, _subscription_operation) = {
            let rooms = self.rooms.lock().await;
            let subscription = rooms
                .get(room_id)
                .filter(|s| s.id == id && !s.cancel.is_cancelled())
                .ok_or(STALE_SUBSCRIPTION)?;
            (subscription.clone(), subscription.tasks.token())
        };
        let timeline = subscription.timeline.get().ok_or(RpcError::new(
            "subscription_not_ready",
            "Wait for timeline subscription to complete before requesting history",
        ))?;
        let timeline = timeline.as_ref().map_err(|error| *error)?;
        // SDK 0.19.1 coalesces reads in an account-owned event-cache task. Dropping
        // this future revokes our waiter, not that task's HTTP/cache work. Its
        // authoritative updates may still reach a successor through the SDK stream;
        // this RPC must never settle the successor's loading/exhaustion state.
        let result = tokio::select! {
            biased;
            _ = subscription.cancel.cancelled() => Err(PAGINATION_CANCELLED),
            result = timeline.paginate_backwards(limit) => result.map_err(|_| RpcError::new(
                "pagination_failed",
                "Cannot load older room history; check connection and room access",
            )),
        };
        if !self.is_current(room_id, &subscription).await {
            return Err(PAGINATION_CANCELLED);
        }
        result
    }

    pub async fn send(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
        body: String,
        segments: Vec<Segment>,
        attachments: Vec<Attachment>,
        reply_to: Option<OwnedEventId>,
    ) -> Result<Value> {
        if body.len() > 32 * 1024 || (body.trim().is_empty() && attachments.is_empty()) {
            return Err(INVALID);
        }
        let caption_empty = body.is_empty();
        let (text, mentions) = compose_text(body, &segments)?;
        let _operation = self.tasks.token();
        let (subscription, _subscription_operation) = self.mutation_subscription(room_id, id).await?;
        let mut prepared = Vec::with_capacity(attachments.len());
        for attachment in attachments {
            prepared.push(attachment.prepare().await?);
        }
        let _mutation = subscription.mutations.lock().await;
        if !self.is_current(room_id, &subscription).await {
            return Err(STALE_SUBSCRIPTION);
        }
        let timeline = mutation_timeline(&subscription)?;
        if timeline.room().state() != RoomState::Joined {
            return Err(RpcError::new("room_not_joined", "Join this room before sending messages"));
        }
        if let Some(target) = &reply_to {
            let items = timeline.items().await;
            if !items.iter().filter_map(|item| item.as_event())
                .any(|event| event.event_id() == Some(target.as_ref()) && event.can_be_replied_to())
            {
                return Err(REPLY_UNAVAILABLE);
            }
        }
        // Once admitted, await SDK durable acceptance, rather than cancelling an
        // enqueue mid-persist and incorrectly reporting that nothing was accepted.
        // View transitions wait on this lock; the account dispatch holds its lease.
        if prepared.is_empty() {
            let result = match reply_to {
                Some(target) => timeline.send_reply(
                    RoomMessageEventContentWithoutRelation::new(MessageType::Text(text))
                        .add_mentions(mentions),
                    target,
                ).await,
                None => timeline.send(
                    RoomMessageEventContent::new(MessageType::Text(text)).add_mentions(mentions).into(),
                ).await,
            };
            return send_outcome(usize::from(result.is_ok()), 1, result.err().map(|_| SEND_FAILED));
        }
        let total = prepared.len();
        let mut accepted = 0;
        let mut caption = (!caption_empty).then_some(text);
        let mut caption_mentions = Some(mentions);
        for mut attachment in prepared {
            if !self.is_current(room_id, &subscription).await {
                return send_outcome(accepted, total, Some(STALE_SUBSCRIPTION));
            }
            let mut data = Vec::with_capacity(attachment.capacity);
            if attachment.file.read_to_end(&mut data).await.is_err() {
                return send_outcome(accepted, total, Some(ATTACHMENT_READ_FAILED));
            }
            let size = match UInt::try_from(data.len()) {
                Ok(size) => size,
                Err(_) => return send_outcome(accepted, total, Some(INVALID)),
            };
            let config = AttachmentConfig::new().info(attachment.info(size))
                .caption(caption.take())
                .mentions(Some(caption_mentions.take().unwrap_or_default()))
                .reply(reply_to.as_ref().map(|target| Reply {
                    event_id: target.clone(), enforce_thread: EnforceThread::MaybeThreaded,
                    add_mentions: AddMentions::Yes,
                }));
            // SDK owns media-cache persistence, upload/encryption, transaction IDs
            // and retries. A successful return releases this local file's ownership.
            if timeline.room().send_queue().send_attachment(
                attachment.filename, attachment.mime, data, config,
            ).await.is_err() {
                return send_outcome(accepted, total, Some(SEND_FAILED));
            }
            accepted += 1;
        }
        send_outcome(accepted, total, None)
    }

    async fn mutation_subscription(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
    ) -> Result<(Arc<Subscription>, TaskTrackerToken)> {
        if id.is_empty() || id.len() > 128 {
            return Err(INVALID);
        }
        let rooms = self.rooms.lock().await;
        let subscription = rooms
            .get(room_id)
            .filter(|s| s.id == id && !s.cancel.is_cancelled())
            .ok_or(STALE_SUBSCRIPTION)?;
        Ok((subscription.clone(), subscription.tasks.token()))
    }

    pub async fn media_lease(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
        event_id: &EventId,
        source_id: String,
    ) -> Result<MediaLease> {
        if !valid_source_id(&source_id) {
            return Err(INVALID);
        }
        let operation = self.tasks.token();
        let (subscription, subscription_operation) = self.mutation_subscription(room_id, id).await?;
        let (request, size) = {
            let timeline = mutation_timeline(&subscription)?;
            let items = timeline.items().await;
            let event = find_event(&items, event_id)?;
            let (source, size) = event_media(event).ok_or(media::STALE_MEDIA)?;
            if media::source_id(source) != source_id {
                return Err(media::STALE_MEDIA);
            }
            (matrix_sdk::media::MediaRequestParameters {
                source: source.clone(),
                format: matrix_sdk::media::MediaFormat::File,
            }, size.map(u64::from))
        };
        let lease = MediaLease {
            subscription,
            room_id: room_id.clone(),
            event_id: event_id.to_owned(),
            source_id,
            request,
            size,
            _operation: operation,
            _subscription_operation: subscription_operation,
        };
        lease.validate(self).await?;
        Ok(lease)
    }

    pub async fn read_through(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
        event_id: &EventId,
    ) -> Result<()> {
        let _operation = self.tasks.token();
        let (subscription, _subscription_operation) = self.mutation_subscription(room_id, id).await?;
        let operation = async {
            let _receipt = subscription.receipts.lock().await;
            if !self.is_current(room_id, &subscription).await {
                return Err(STALE_SUBSCRIPTION);
            }
            let timeline = mutation_timeline(&subscription)?;
            let items = timeline.items().await;
            if !items.iter().filter_map(|item| item.as_event())
                .any(|event| event.event_id() == Some(event_id) && read_capable(event))
            {
                return Err(EVENT_MISSING);
            }
            drop(items);
            // The SDK compares timeline positions and coalesces older receipts.
            // Never use mark_as_read: its account cache may include unseen events.
            timeline.send_multiple_receipts(
                Receipts::new()
                    .fully_read_marker(event_id.to_owned())
                    .private_read_receipt(event_id.to_owned()),
            ).await.map_err(|_| NETWORK)?;
            if !self.is_current(room_id, &subscription).await {
                return Err(STALE_SUBSCRIPTION);
            }
            Ok(())
        };
        tokio::select! {
            biased;
            _ = subscription.cancel.cancelled() => Err(STALE_SUBSCRIPTION),
            result = operation => result,
        }
    }

    /// Run OPERATION on the subscription's timeline in this room's mutation order.
    async fn mutate<T>(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
        operation: impl AsyncFnOnce(&Timeline) -> Result<T>,
    ) -> Result<T> {
        let _operation = self.tasks.token();
        let (subscription, _subscription_operation) = self.mutation_subscription(room_id, id).await?;
        let _mutation = subscription.mutations.lock().await;
        if !self.is_current(room_id, &subscription).await {
            return Err(STALE_SUBSCRIPTION);
        }
        let timeline = mutation_timeline(&subscription)?;
        tokio::select! {
            biased;
            _ = subscription.cancel.cancelled() => Err(STALE_SUBSCRIPTION),
            result = operation(timeline) => result,
        }
    }

    pub async fn edit(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
        event_id: &EventId,
        body: String,
    ) -> Result<()> {
        if body.trim().is_empty() || body.len() > 32 * 1024 {
            return Err(INVALID);
        }
        self.mutate(room_id, id, async |timeline| {
            let items = timeline.items().await;
            let event = find_event(&items, event_id)?;
            if !event.is_own() {
                return Err(EDIT_FORBIDDEN);
            }
            let message = event.content().as_message().filter(|message| body_editable(message))
                .ok_or(EDIT_UNAVAILABLE)?;
            let permissions = mutation_permissions(timeline.room()).await?;
            if !permissions.can_edit(event) {
                return Err(EDIT_FORBIDDEN);
            }
            timeline.edit(&event.identifier(), edited_content(message, body)?).await
                .map_err(edit_error)
        }).await
    }

    pub async fn redact(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
        event_id: &EventId,
    ) -> Result<()> {
        self.mutate(room_id, id, async |timeline| {
            let items = timeline.items().await;
            let event = find_event(&items, event_id)?;
            if is_redacted(event) || event.redaction_send_state().is_some() {
                return Err(ALREADY_REDACTED);
            }
            if !mutation_permissions(timeline.room()).await?.can_redact(event) {
                return Err(REDACT_FORBIDDEN);
            }
            timeline.redact(&event.identifier(), None).await.map_err(redact_error)
        }).await
    }

    /// Add KEY to the event's reactions, or remove our own; return whether added.
    pub async fn react(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
        event_id: &EventId,
        key: &str,
    ) -> Result<bool> {
        if key.trim().is_empty() || key.len() > MAX_REACTION_KEY {
            return Err(INVALID);
        }
        self.mutate(room_id, id, async |timeline| {
            let items = timeline.items().await;
            let event = find_event(&items, event_id)?;
            if !mutation_permissions(timeline.room()).await?.can_react(event) {
                return Err(REACT_FORBIDDEN);
            }
            timeline.toggle_reaction(&event.identifier(), key).await.map_err(|error| match error {
                TimelineError::EventNotInTimeline(_) => EVENT_MISSING,
                _ => RpcError::new(
                    "reaction_failed",
                    "SDK could not queue this reaction; check room access and connection",
                ),
            })
        }).await
    }

    /// Resend a local echo the SDK parked after an unrecoverable failure.
    pub async fn retry_send(&self, room_id: &OwnedRoomId, id: &str, transaction_id: &str) -> Result<()> {
        self.mutate(room_id, id, async |timeline| {
            let items = timeline.items().await;
            let event = local_echo(&items, transaction_id)?;
            if !matches!(event.send_state(), Some(EventSendState::SendingFailed { .. })) {
                return Err(RpcError::new("not_failed", "Only a failed message can be retried"));
            }
            let handle = event.local_echo_send_handle().ok_or(EVENT_MISSING)?;
            drop(items);
            handle.unwedge().await.map_err(|_| {
                RpcError::new("retry_failed", "SDK could not requeue this message")
            })?;
            timeline.room().send_queue().set_enabled(true);
            Ok(())
        }).await
    }

    /// Withdraw a local echo that is still unsent; false if it was already sent.
    pub async fn discard_send(&self, room_id: &OwnedRoomId, id: &str, transaction_id: &str) -> Result<bool> {
        self.mutate(room_id, id, async |timeline| {
            let items = timeline.items().await;
            let handle = local_echo(&items, transaction_id)?.local_echo_send_handle()
                .ok_or(EVENT_MISSING)?;
            drop(items);
            handle.abort().await.map_err(|_| {
                RpcError::new("discard_failed", "SDK could not withdraw this message")
            })
        }).await
    }

    pub async fn retry_decryption(&self, sessions: &[String]) {
        let timelines: Vec<_> = self
            .rooms
            .lock()
            .await
            .values()
            .filter_map(|s| {
                s.timeline
                    .get()
                    .and_then(|result| result.as_ref().ok())
                    .cloned()
            })
            .collect();
        for timeline in timelines {
            timeline
                .retry_decryption(sessions.iter().map(String::as_str))
                .await;
        }
    }

    pub async fn shutdown(&self) {
        self.cancel.cancel();
        let rooms = std::mem::take(&mut *self.rooms.lock().await);
        for subscription in rooms.values() {
            subscription.cancel();
        }
        self.tasks.close();
        self.tasks.wait().await;
    }
}

fn find_event<'a>(
    items: impl IntoIterator<Item = &'a Arc<TimelineItem>>,
    event_id: &EventId,
) -> Result<&'a EventTimelineItem> {
    items.into_iter().filter_map(|item| item.as_event())
        .find(|event| event.event_id() == Some(event_id)).ok_or(EVENT_MISSING)
}

/// Explain why the SDK stopped sending a local echo.
fn send_failure(error: &matrix_sdk::Error, recoverable: bool) -> &'static str {
    use matrix_sdk::QueueWedgeError as Wedge;
    if recoverable {
        return "Not sent yet; sending resumes when the connection recovers";
    }
    match error {
        matrix_sdk::Error::SendQueueWedgeError(wedge) => match wedge.as_ref() {
            Wedge::InsecureDevices { .. } => "Not sent: some recipients have unverified devices",
            Wedge::IdentityViolations { .. } => {
                "Not sent: a recipient's identity changed; verify them before retrying"
            }
            Wedge::CrossVerificationRequired => "Not sent: verify this device before retrying",
            Wedge::MissingMediaContent => "Not sent: the attachment data is missing",
            Wedge::InvalidMimeType { .. } => "Not sent: the attachment type is invalid",
            Wedge::GenericApiError { .. } => "Not sent: the server rejected this message",
        },
        _ => "Not sent: the server rejected this message",
    }
}

fn local_echo<'a>(
    items: impl IntoIterator<Item = &'a Arc<TimelineItem>>,
    transaction_id: &str,
) -> Result<&'a EventTimelineItem> {
    items.into_iter().filter_map(|item| item.as_event())
        .find(|event| event.transaction_id().is_some_and(|id| id.as_str() == transaction_id))
        .filter(|event| event.local_echo_send_handle().is_some())
        .ok_or(RpcError::new("unsent_missing", "The message is no longer waiting to be sent"))
}

fn mutation_timeline(subscription: &Subscription) -> Result<&Arc<Timeline>> {
    subscription.timeline.get().ok_or(RpcError::new(
        "subscription_not_ready",
        "Wait for timeline subscription to complete before changing messages",
    ))?.as_ref().map_err(|error| *error)
}

fn edited_content(message: &Message, body: String) -> Result<EditedContent> {
    let mut content = match message.msgtype() {
        // A body-only edit is plain text: retain the original subtype, not stale
        // HTML or URL previews whose content described the preceding body.
        MessageType::Text(_) => RoomMessageEventContentWithoutRelation::text_plain(body),
        MessageType::Notice(_) => RoomMessageEventContentWithoutRelation::notice_plain(body),
        MessageType::Emote(_) => RoomMessageEventContentWithoutRelation::emote_plain(body),
        MessageType::Audio(_) | MessageType::File(_) | MessageType::Image(_) | MessageType::Video(_) => {
            // SDK caption edits preserve the attachment/filename/source, including
            // encrypted media, instead of replacing a media message with m.text.
            return Ok(EditedContent::MediaCaption {
                caption: Some(body),
                formatted_caption: None,
                mentions: message.mentions().cloned(),
            });
        }
        _ => return Err(EDIT_UNAVAILABLE),
    };
    content.mentions = message.mentions().cloned();
    Ok(EditedContent::RoomMessage(content))
}

fn edit_error(error: TimelineError) -> RpcError {
    match error {
        TimelineError::EventNotInTimeline(_) => EVENT_MISSING,
        TimelineError::EditError(EditError::RoomError(RoomEditError::NotAuthor)) => EDIT_FORBIDDEN,
        TimelineError::EditError(EditError::RoomError(
            RoomEditError::StateEvent | RoomEditError::IncompatibleEditType { .. }
                | RoomEditError::Deserialize(_),
        )) | TimelineError::UnsupportedEvent => EDIT_UNAVAILABLE,
        TimelineError::EditError(EditError::RoomError(RoomEditError::Fetch(error)))
            if error.client_api_error_kind().is_some_and(|kind| matches!(kind, ErrorKind::Forbidden)) =>
        {
            EDIT_FORBIDDEN
        }
        TimelineError::EditError(EditError::RoomError(RoomEditError::Fetch(error)))
            if error.client_api_error_kind() == Some(&ErrorKind::NotFound) =>
        {
            EVENT_MISSING
        }
        TimelineError::Sdk(error)
            if error.client_api_error_kind().is_some_and(|kind| matches!(kind, ErrorKind::Forbidden)) =>
        {
            EDIT_FORBIDDEN
        }
        _ => RpcError::new(
            "edit_failed",
            "SDK could not queue this edit; check connection, room access and device verification",
        ),
    }
}

fn redact_error(error: TimelineError) -> RpcError {
    match error {
        TimelineError::EventNotInTimeline(_)
            | TimelineError::RedactError(RedactError::ItemNotFound(_)) => EVENT_MISSING,
        TimelineError::Sdk(error)
            if error.client_api_error_kind().is_some_and(|kind| matches!(kind, ErrorKind::Forbidden)) =>
        {
            REDACT_FORBIDDEN
        }
        _ => RpcError::new(
            "redaction_failed",
            "SDK could not queue this redaction; check room access and connection",
        ),
    }
}

fn joined_room(client: &Client, room_id: &OwnedRoomId) -> Result<matrix_sdk::Room> {
    let room = client.get_room(room_id).ok_or(RpcError::new(
        "room_missing",
        "Room is not known; wait for sync or join it first",
    ))?;
    if room.state() != RoomState::Joined {
        return Err(RpcError::new(
            "room_not_joined",
            "Join this room before opening its timeline or sending messages",
        ));
    }
    Ok(room)
}


/// The source and declared size of EVENT's image, file, audio or video.
fn event_media(event: &EventTimelineItem) -> Option<(&MediaSource, Option<UInt>)> {
    match event.content().as_message()?.msgtype() {
        MessageType::Image(image) => Some((&image.source, image.info.as_ref().and_then(|i| i.size))),
        MessageType::File(file) => Some((&file.source, file.info.as_ref().and_then(|i| i.size))),
        MessageType::Audio(audio) => Some((&audio.source, audio.info.as_ref().and_then(|i| i.size))),
        MessageType::Video(video) => Some((&video.source, video.info.as_ref().and_then(|i| i.size))),
        _ => None,
    }
}

/// Describe EVENT's file, audio or video attachment for presentation.
fn attachment_metadata(event: &EventTimelineItem) -> Option<Value> {
    let (kind, source, filename, mime, size, duration) =
        match event.content().as_message()?.msgtype() {
            MessageType::File(file) => {
                let info = file.info.as_deref();
                ("file", &file.source, file.filename(), info.and_then(|i| i.mimetype.as_ref()),
                    info.and_then(|i| i.size), None)
            }
            MessageType::Audio(audio) => {
                let info = audio.info.as_deref();
                ("audio", &audio.source, audio.filename(), info.and_then(|i| i.mimetype.as_ref()),
                    info.and_then(|i| i.size), info.and_then(|i| i.duration))
            }
            MessageType::Video(video) => {
                let info = video.info.as_deref();
                ("video", &video.source, video.filename(), info.and_then(|i| i.mimetype.as_ref()),
                    info.and_then(|i| i.size), info.and_then(|i| i.duration))
            }
            _ => return None,
        };
    let mut value = json!({"kind":kind, "source_id":media::source_id(source), "filename":filename});
    if let Some(mime) = mime {
        value["mime_type"] = json!(mime);
    }
    if let Some(size) = size {
        value["size"] = json!(size);
    }
    if let Some(duration) = duration {
        value["duration_ms"] = json!(duration.as_millis() as u64);
    }
    Some(value)
}

fn event_image(event: &EventTimelineItem) -> Option<&matrix_sdk::ruma::events::room::message::ImageMessageEventContent> {
    if is_redacted(event) {
        return None;
    }
    match event.content().as_message()?.msgtype() {
        MessageType::Image(image) => Some(image),
        _ => None,
    }
}

fn image_metadata(image: &matrix_sdk::ruma::events::room::message::ImageMessageEventContent) -> Value {
    let mut value = json!({
        "source_id":media::source_id(&image.source),
        "filename":image.filename(),
    });
    if let Some(info) = &image.info {
        if let Some(mime) = &info.mimetype {
            value["mime_type"] = json!(mime);
        }
        if let Some(width) = info.width.filter(|width| u64::from(*width) > 0) {
            value["width"] = json!(width);
        }
        if let Some(height) = info.height.filter(|height| u64::from(*height) > 0) {
            value["height"] = json!(height);
        }
        if let Some(size) = info.size {
            value["size"] = json!(size);
        }
    }
    value
}
fn reply_metadata(reply: &InReplyToDetails) -> Value {
    let mut value = json!({"event_id":reply.event_id});
    match &reply.event {
        TimelineDetails::Unavailable => value["status"] = json!("unavailable"),
        TimelineDetails::Pending => value["status"] = json!("pending"),
        TimelineDetails::Error(_) => value["status"] = json!("error"),
        TimelineDetails::Ready(event) => {
            value["status"] = json!("ready");
            value["sender"] = json!(event.sender);
            if let TimelineDetails::Ready(profile) = &event.sender_profile
                && let Some(name) = &profile.display_name {
                    value["sender_name"] = json!(name);
                }
            if let Some(message) = event.content.as_message() {
                value["kind"] = json!("message");
                value["body"] = json!(message.body());
            } else if event.content.is_redacted() {
                value["kind"] = json!("redacted");
                value["body"] = json!("Message deleted");
            } else if event.content.is_unable_to_decrypt() {
                value["kind"] = json!("encrypted");
                value["body"] = json!("Encrypted message — keys unavailable");
            } else {
                value["kind"] = json!("state");
                value["body"] = json!(event.content.event_type_str()
                    .unwrap_or_else(|| "Redacted event".into()));
            }
        }
    }
    value
}

fn read_capable(event: &EventTimelineItem) -> bool {
    event.is_remote_event()
        && event.event_id().is_some()
        && (event.content().as_message().is_some()
            || event.content().is_unable_to_decrypt()
            || event.content().is_redacted())
}

fn reactions(event: &EventTimelineItem, own: &UserId) -> Option<Value> {
    let reactions = event.content().reactions().filter(|reactions| !reactions.is_empty())?;
    Some(reactions.iter()
        .filter(|(key, senders)| key.len() <= MAX_REACTION_KEY && !senders.is_empty())
        .take(MAX_REACTION_KEYS)
        .map(|(key, senders)| json!({
            "key":key, "count":senders.len(), "own":senders.contains_key(own),
        }))
        .collect())
}

fn item(item: &TimelineItem, permissions: MutationPermissions, own: &UserId) -> Value {
    let mut value = json!({"id": item.unique_id().0, "can_edit":false, "can_redact":false, "can_reply":false, "can_read":false});
    if let Some(event) = item.as_event() {
        value["event_id"] = json!(event.event_id());
        value["transaction_id"] = json!(event.transaction_id());
        value["sender"] = json!(event.sender());
        if let TimelineDetails::Ready(profile) = event.sender_profile() {
            if let Some(name) = &profile.display_name {
                value["sender_name"] = json!(name);
            }
            if let Some(uri) = &profile.avatar_url {
                value["avatar_url"] = json!(uri);
            }
        }
        value["timestamp"] = json!(event.timestamp());
        value["own"] = json!(event.is_own());
        value["can_edit"] = json!(permissions.can_edit(event));
        value["can_redact"] = json!(permissions.can_redact(event));
        value["can_react"] = json!(permissions.can_react(event));
        if event.is_highlighted() {
            value["highlighted"] = json!(true);
        }
        if let Some(reactions) = reactions(event, own) {
            value["reactions"] = reactions;
        }
        value["can_reply"] = json!(event.can_be_replied_to());
        value["can_read"] = json!(read_capable(event));
        if let Some(reply) = event.content().in_reply_to() {
            value["reply"] = reply_metadata(&reply);
        }
        let content = event.content();
        if let Some(message) = content.as_message() {
            value["kind"] = json!("message");
            value["body"] = json!(message.body());
            if let Some(image) = event_image(event) {
                value["image"] = image_metadata(image);
            } else if let Some(attachment) = attachment_metadata(event) {
                value["attachment"] = attachment;
            }
        } else if is_redacted(event) {
            value["kind"] = json!("redacted");
            value["body"] = json!("Message deleted");
        } else if content.is_unable_to_decrypt() {
            value["kind"] = json!("encrypted");
            value["body"] =
                json!("Encrypted message — keys unavailable or sender trust requirements not met");
        } else if let Some((state_type, text)) = describe::state(event) {
            value["kind"] = json!("state");
            value["state_type"] = json!(state_type);
            value["described"] = json!(true);
            value["body"] = json!(text);
        } else {
            value["kind"] = json!("state");
            value["body"] = json!(
                content
                    .event_type_str()
                    .unwrap_or_else(|| "Redacted event".into())
            );
        }
        let local = event.local_echo_send_handle().is_some();
        match event.send_state() {
            Some(EventSendState::NotSentYet { .. }) => {
                value["send_state"] = json!("pending");
                value["can_discard"] = json!(local);
            }
            Some(EventSendState::SendingFailed { error, is_recoverable }) => {
                value["send_state"] = json!("failed");
                value["error"] = json!(send_failure(error, *is_recoverable));
                value["can_retry"] = json!(local && !is_recoverable);
                value["can_discard"] = json!(local);
            }
            Some(EventSendState::Sent { .. }) => value["send_state"] = json!("sent"),
            None if event.is_own() => value["send_state"] = json!("sent"),
            None => {}
        }
    } else {
        match item.as_virtual() {
            Some(VirtualTimelineItem::DateDivider(timestamp)) => {
                value["kind"] = json!("day");
                value["body"] = json!("Day");
                value["timestamp"] = json!(timestamp);
            }
            Some(VirtualTimelineItem::ReadMarker) => {
                value["kind"] = json!("read_marker");
                value["body"] = json!("Read up to here");
            }
            _ => {
                value["kind"] = json!("state");
                value["body"] = json!("Start of timeline");
            }
        }
    }
    value
}

fn change(diff: &VectorDiff<Arc<TimelineItem>>, permissions: MutationPermissions, own: &UserId) -> Value {
    match diff {
        VectorDiff::Append { values } => {
            json!({"op":"append", "items":values.iter().map(|i| item(i, permissions, own)).collect::<Vec<_>>()})
        }
        VectorDiff::Clear => json!({"op":"clear"}),
        VectorDiff::PushFront { value } => json!({"op":"push_front", "item":item(value, permissions, own)}),
        VectorDiff::PushBack { value } => json!({"op":"push_back", "item":item(value, permissions, own)}),
        VectorDiff::PopFront => json!({"op":"pop_front"}),
        VectorDiff::PopBack => json!({"op":"pop_back"}),
        VectorDiff::Insert { index, value } => {
            json!({"op":"insert", "index":index, "item":item(value, permissions, own)})
        }
        VectorDiff::Set { index, value } => json!({"op":"set", "index":index, "item":item(value, permissions, own)}),
        VectorDiff::Remove { index } => json!({"op":"remove", "index":index}),
        VectorDiff::Truncate { length } => json!({"op":"truncate", "length":length}),
        VectorDiff::Reset { values } => {
            json!({"op":"reset", "items":values.iter().map(|i| item(i, permissions, own)).collect::<Vec<_>>()})
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::os::unix::fs::PermissionsExt;
    use std::time::Duration;
    use matrix_sdk::{
        authentication::matrix::MatrixSession,
        config::{RequestConfig, SyncSettings},
        ruma::{api::MatrixVersion, event_id, owned_room_id},
    };
    use tokio::sync::mpsc;
    use wiremock::{Mock, MockServer, ResponseTemplate, matchers::{method, path_regex}};

    #[tokio::test]
    async fn replacement_revokes_pending_initializer_without_blocking_other_rooms() {
        let timelines = Timelines::new(CancellationToken::new());
        let room = owned_room_id!("!room:example.org");
        let (old, _, old_operation) = timelines.reserve(room.clone(), "old".into()).await.unwrap();
        let (current, replaced, current_operation) = timelines
            .reserve(room.clone(), "current".into())
            .await
            .unwrap();
        assert!(Arc::ptr_eq(&old, &replaced.unwrap()));
        assert!(old.cancel.is_cancelled());
        assert!(!timelines.is_current(&room, &old).await);
        assert!(timelines.is_current(&room, &current).await);

        // The old initializer is deliberately still alive. It neither holds the
        // room map nor gives a stale close request authority over the replacement.
        timelines.unsubscribe(&room, "old").await;
        assert!(timelines.is_current(&room, &current).await);
        assert_eq!(
            timelines.paginate(&room, "old", 30).await.unwrap_err().code,
            "stale_subscription"
        );
        let (_, _, unrelated_operation) = timelines
            .reserve(owned_room_id!("!other:example.org"), "other".into())
            .await
            .unwrap();
        drop(unrelated_operation);
        drop(old_operation);
        old.tasks.wait().await;
        drop(current_operation);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn closed_generation_cannot_settle_a_reopened_subscription() {
        let transport = CancellationToken::new();
        let timelines = Timelines::new(transport.clone());
        let room = owned_room_id!("!room:example.org");
        let (old, _, operation) = timelines
            .reserve(room.clone(), "same-id".into())
            .await
            .unwrap();
        drop(operation);
        timelines.unsubscribe(&room, "same-id").await;
        let (current, _, operation) = timelines
            .reserve(room.clone(), "same-id".into())
            .await
            .unwrap();
        assert!(!timelines.is_current(&room, &old).await);
        assert!(timelines.is_current(&room, &current).await);

        transport.cancel();
        assert!(!timelines.is_current(&room, &current).await);
        assert_eq!(
            timelines
                .paginate(&room, "same-id", 30)
                .await
                .unwrap_err()
                .code,
            "stale_subscription"
        );
        assert_eq!(
            timelines
                .reserve(room, "after-close".into())
                .await
                .err()
                .unwrap()
                .code,
            "stale_subscription"
        );
        drop(operation);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn pagination_rejects_invalid_limits_before_touching_a_subscription() {
        let timelines = Timelines::new(CancellationToken::new());
        let room = owned_room_id!("!room:example.org");
        let (_, _, operation) = timelines
            .reserve(room.clone(), "active".into())
            .await
            .unwrap();
        for limit in [0, 101, u16::MAX] {
            assert_eq!(
                timelines
                    .paginate(&room, "active", limit)
                    .await
                    .unwrap_err()
                    .code,
                "invalid_params"
            );
        }
        for limit in [1, 100] {
            assert_eq!(
                timelines
                    .paginate(&room, "active", limit)
                    .await
                    .unwrap_err()
                    .code,
                "subscription_not_ready"
            );
        }
        drop(operation);
        timelines.shutdown().await;
    }

    fn message(event_id: &str, sender: &str, content: Value) -> Value {
        json!({
            "type":"m.room.message", "event_id":event_id, "sender":sender,
            "origin_server_ts":1000, "content":content
        })
    }

    fn power_levels(own_level: u8) -> Value {
        json!({"users":{"@me:example.org":own_level}, "redact":50})
    }

    async fn sdk_sync(server: &MockServer, client: &Client, levels: Option<Value>, events: Vec<Value>) {
        sdk_sync_with_receipts(server, client, levels, events, None, None).await;
    }

    async fn sdk_sync_with_receipts(
        server: &MockServer,
        client: &Client,
        levels: Option<Value>,
        events: Vec<Value>,
        receipt: Option<&str>,
        marker: Option<&str>,
    ) {
        let mut state = vec![
            json!({
                "type":"m.room.create", "state_key":"", "event_id":"$create",
                "sender":"@me:example.org", "origin_server_ts":1,
                "content":{"creator":"@me:example.org", "room_version":"11"}
            }),
            json!({
                "type":"m.room.member", "state_key":"@me:example.org", "event_id":"$member",
                "sender":"@me:example.org", "origin_server_ts":2, "content":{"membership":"join"}
            }),
            json!({
                "type":"m.room.member", "state_key":"@other:example.org", "event_id":"$other-member",
                "sender":"@other:example.org", "origin_server_ts":3, "content":{"membership":"join"}
            }),
        ];
        if let Some(content) = levels {
            state.push(json!({
                "type":"m.room.power_levels", "state_key":"",
                "event_id":format!("$powers-{}", uuid::Uuid::new_v4()),
                "sender":"@me:example.org", "origin_server_ts":4, "content":content
            }));
        }
        let response = json!({
            "next_batch":uuid::Uuid::new_v4().to_string(),
            "rooms":{"join":{"!room:example.org":{
                "state":{"events":state}, "timeline":{"events":events, "limited":false},
                "ephemeral":{"events":receipt.map(|event_id| json!({
                    "type":"m.receipt", "content":{event_id:{
                        "m.read.private":{"@me:example.org":{"ts":2000}}
                    }}
                })).into_iter().collect::<Vec<_>>()},
                "account_data":{"events":marker.map(|event_id| json!({
                    "type":"m.fully_read", "content":{"event_id":event_id}
                })).into_iter().collect::<Vec<_>>()},
                "unread_notifications":{"notification_count":0, "highlight_count":0}
            }}},
            "account_data":{"events":[]}, "to_device":{"events":[]},
            "device_lists":{"changed":[], "left":[]}, "device_one_time_keys_count":{}
        });
        let _sync = Mock::given(method("GET"))
            .and(path_regex(r"^/_matrix/client/(r0|v3)/sync$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(response))
            .mount_as_scoped(server).await;
        client.sync_once(SyncSettings::default()).await.unwrap();
    }

    async fn sdk_fixture(
        levels: Option<Value>,
        events: Vec<Value>,
    ) -> (MockServer, Client, Timelines, mpsc::Receiver<Vec<u8>>) {
        sdk_fixture_with_server(MockServer::start().await, levels, events).await
    }

    async fn sdk_fixture_with_server(
        server: MockServer,
        levels: Option<Value>,
        events: Vec<Value>,
    ) -> (MockServer, Client, Timelines, mpsc::Receiver<Vec<u8>>) {
        let client = Client::builder().homeserver_url(server.uri())
            .server_versions([MatrixVersion::V1_0])
            .request_config(RequestConfig::new().disable_retry().timeout(Duration::from_secs(5)))
            .build().await.unwrap();
        let session: MatrixSession = serde_json::from_value(json!({
            "user_id":"@me:example.org", "device_id":"DEVICE", "access_token":"test-token"
        })).unwrap();
        client.restore_session(session).await.unwrap();
        // Observe genuine SDK local queue updates, without a fake server echo
        // or background HTTP retry races. The durable queue remains authoritative.
        client.send_queue().set_enabled(false).await;
        client.event_cache().subscribe().unwrap();
        Mock::given(method("GET"))
            .and(path_regex(r"/state/m\.room\.encryption/?$"))
            .respond_with(ResponseTemplate::new(404)
                .set_body_json(json!({"errcode":"M_NOT_FOUND", "error":"Not encrypted"})))
            .mount(&server).await;
        sdk_sync(&server, &client, levels, events).await;
        let timelines = Timelines::new(CancellationToken::new());
        let (tx, rx) = mpsc::channel(128);
        timelines.subscribe(&client, Output::new(tx, CancellationToken::new()),
            owned_room_id!("!room:example.org"), "active".into()).await.unwrap();
        (server, client, timelines, rx)
    }

    async fn snapshot_row(timelines: &Timelines, event_id: &EventId) -> Value {
        let room = owned_room_id!("!room:example.org");
        let (subscription, operation) = timelines.mutation_subscription(&room, "active").await.unwrap();
        let timeline = mutation_timeline(&subscription).unwrap();
        let permissions = mutation_permissions(timeline.room()).await.unwrap_or_default();
        let items = timeline.items().await;
        let row = items.iter().find(|item| item.as_event()
            .is_some_and(|event| event.event_id() == Some(event_id))).unwrap();
        let value = item(row, permissions, timeline.room().own_user_id());
        drop(operation);
        value
    }

    async fn next_row(
        rx: &mut mpsc::Receiver<Vec<u8>>,
        event_id: &str,
        predicate: impl Fn(&Value) -> bool,
    ) -> Value {
        tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                let bytes = rx.recv().await.expect("timeline output ended");
                let event: Value = serde_json::from_slice(&bytes).unwrap();
                let params = &event["params"];
                let mut rows = Vec::new();
                if let Some(items) = params["items"].as_array() {
                    rows.extend(items);
                }
                if let Some(changes) = params["changes"].as_array() {
                    for change in changes {
                        if let Some(row) = change.get("item") {
                            rows.push(row);
                        }
                        if let Some(items) = change["items"].as_array() {
                            rows.extend(items);
                        }
                    }
                }
                if let Some(row) = rows.into_iter()
                    .find(|row| row["event_id"] == event_id && predicate(row)) {
                    return row.clone();
                }
            }
        }).await.expect("SDK timeline did not publish expected event update")
    }

    async fn next_read_state(rx: &mut mpsc::Receiver<Vec<u8>>, all_read: bool) -> Value {
        tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                let event: Value = serde_json::from_slice(&rx.recv().await.unwrap()).unwrap();
                let event = &event["params"];
                if event["read_state"].is_object()
                    && (event["read_state"]["all_read"] == true) == all_read
                {
                    return event.clone();
                }
            }
        }).await.expect("SDK timeline did not publish expected read state")
    }

    #[tokio::test]
    async fn sdk_read_state_requires_complete_frontier_not_notification_zero() {
        let (server, client, timelines, mut rx) = sdk_fixture(None, vec![
            message("$z", "@other:example.org", json!({"msgtype":"m.notice", "body":"first"})),
            message("$m", "@other:example.org", json!({"msgtype":"m.notice", "body":"middle"})),
            message("$a", "@other:example.org", json!({"msgtype":"m.notice", "body":"last"})),
        ]).await;
        let room_id = owned_room_id!("!room:example.org");
        let room = client.get_room(&room_id).unwrap();
        let initial = next_read_state(&mut rx, false).await;
        assert!(initial["read_state"]["unread_messages"].is_null());
        assert_eq!(room.num_unread_notifications(), 0);
        // Notices are unread messages, but the default suppress-notices push rule
        // gives them no notification badge. Neither number is the other's proxy.
        tokio::time::timeout(Duration::from_secs(10), async {
            while room.num_unread_messages() == 0 {
                tokio::task::yield_now().await;
            }
        }).await.unwrap();
        let (subscription, operation) = timelines.mutation_subscription(&room_id, "active").await.unwrap();
        let timeline = mutation_timeline(&subscription).unwrap();

        // An unresolved receipt and a zero notification badge do not prove read-all.
        sdk_sync_with_receipts(&server, &client, None, vec![], Some("$unloaded"), None).await;
        let unresolved = read_state(timeline, timeline.items().await.iter()).await;
        assert!(unresolved.all_read.is_none());
        assert!(unresolved.first_unread_index.is_none());

        // Opaque ids sort opposite to timeline order. A partial receipt stays partial.
        sdk_sync_with_receipts(&server, &client, None, vec![], Some("$m"), Some("$m")).await;
        tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                if timeline.latest_user_read_receipt(room.own_user_id()).await
                    .is_some_and(|(id, _)| id == event_id!("$m"))
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
        }).await.unwrap();
        let partial = read_state(timeline, timeline.items().await.iter()).await;
        assert!(partial.all_read.is_none());
        assert!(partial.unread_messages.is_none());
        assert_eq!(room.num_unread_notifications(), 0);

        // Receipt-only sync from another client must publish, even though own
        // receipts don't change visible rows and the notification count stays zero.
        sdk_sync_with_receipts(&server, &client, None, vec![], Some("$a"), None).await;
        let complete = next_read_state(&mut rx, true).await;
        assert_eq!(complete["subscription_id"], "active");
        assert_eq!(complete["read_state"]["unread_messages"], 0);
        assert_eq!(room.num_unread_notifications(), 0);

        // A new remote message revokes completeness; don't retain an old zero.
        sdk_sync(&server, &client, None, vec![
            message("$0", "@other:example.org", json!({"msgtype":"m.notice", "body":"new"})),
        ]).await;
        let unread = next_read_state(&mut rx, false).await;
        assert!(unread["read_state"]["unread_messages"].is_null());
        assert!(unread["revision"].as_u64().unwrap() > complete["revision"].as_u64().unwrap());
        // The old fully-read marker must not recreate a boundary before $m/$a.
        tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                let items = timeline.items().await;
                let reading = read_state(timeline, items.iter()).await;
                if reading.first_unread_index.and_then(|index| items.get(index))
                    .and_then(|item| item.as_event()).and_then(|event| event.event_id())
                    == Some(event_id!("$0"))
                {
                    break;
                }
                tokio::task::yield_now().await;
            }
        }).await.unwrap();
        drop(operation);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_read_boundary_resolves_receipts_loaded_before_timeline_creation() {
        let (server, client, timelines, _rx) = sdk_fixture(None, vec![
            message("$before", "@other:example.org", json!({"msgtype":"m.text", "body":"read"})),
            message("$after", "@other:example.org", json!({"msgtype":"m.text", "body":"unread"})),
        ]).await;
        sdk_sync_with_receipts(&server, &client, None, vec![],
            Some("$before"), Some("$before")).await;
        let timeline = client.get_room(&owned_room_id!("!room:example.org")).unwrap()
            .timeline().await.unwrap();
        let items = timeline.items().await;
        let reading = read_state(&timeline, items.iter()).await;
        assert!(!reading.all_read.unwrap_or(false));
        assert_eq!(reading.first_unread_index.and_then(|index| items.get(index))
            .and_then(|item| item.as_event()).and_then(|event| event.event_id()),
            Some(event_id!("$after")));
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_read_state_empty_timeline_is_unknown_and_fully_read_can_confirm() {
        let (server, client, timelines, mut rx) = sdk_fixture(None, vec![]).await;
        let initial = next_read_state(&mut rx, false).await;
        assert!(initial["read_state"]["unread_messages"].is_null());
        sdk_sync_with_receipts(&server, &client, None, vec![
            message("$latest", "@other:example.org", json!({"msgtype":"m.text", "body":"last"})),
        ], None, Some("$latest")).await;
        let complete = next_read_state(&mut rx, true).await;
        assert_eq!(complete["read_state"]["unread_messages"], 0);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_read_through_replacement_revokes_inflight_receipt() {
        let (server, _client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$target", "@other:example.org", json!({"msgtype":"m.text", "body":"selected"})),
        ]).await;
        Mock::given(method("POST"))
            .and(path_regex(r"/read_markers$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({}))
                .set_delay(Duration::from_secs(4)))
            .mount(&server).await;
        let timelines = Arc::new(timelines);
        let reading = timelines.clone();
        let request = tokio::spawn(async move {
            reading.read_through(&owned_room_id!("!room:example.org"), "active",
                event_id!("$target")).await
        });
        tokio::time::timeout(Duration::from_secs(5), async {
            loop {
                if server.received_requests().await.unwrap().iter()
                    .any(|request| request.url.path().ends_with("/read_markers")) {
                    break;
                }
                tokio::task::yield_now().await;
            }
        }).await.unwrap();
        let (_, _, replacement_operation) = timelines
            .reserve(owned_room_id!("!room:example.org"), "replacement".into()).await.unwrap();
        assert_eq!(tokio::time::timeout(Duration::from_secs(1), request)
            .await.unwrap().unwrap().unwrap_err().code, "stale_subscription");
        drop(replacement_operation);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_read_through_uses_exact_remote_event_and_independent_receipt_lease() {
        let (server, _client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$z", "@other:example.org", json!({"msgtype":"m.text", "body":"selected"})),
            message("$a", "@other:example.org", json!({"msgtype":"m.text", "body":"unseen"})),
        ]).await;
        let room = owned_room_id!("!room:example.org");
        Mock::given(method("POST"))
            .and(path_regex(r"/read_markers$"))
            .and(wiremock::matchers::body_json(json!({
                "m.fully_read":"$z", "m.read.private":"$z"
            })))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({})))
            .expect(1)
            .mount(&server).await;
        assert_eq!(snapshot_row(&timelines, event_id!("$z")).await["can_read"], true);
        assert_eq!(timelines.read_through(&room, "active", event_id!("$missing"))
            .await.unwrap_err().code, "event_missing");
        assert_eq!(timelines.read_through(&room, "replaced", event_id!("$z"))
            .await.unwrap_err().code, "stale_subscription");
        let (subscription, operation) = timelines.mutation_subscription(&room, "active").await.unwrap();
        let mutation = subscription.mutations.lock().await;
        tokio::time::timeout(Duration::from_secs(5),
            timelines.read_through(&room, "active", event_id!("$z"))).await.unwrap().unwrap();
        // Successful partial acknowledgement must not clear the live boundary.
        let timeline = mutation_timeline(&subscription).unwrap();
        assert!(read_state(timeline, timeline.items().await.iter()).await.all_read.is_none());
        drop(mutation);
        drop(operation);
        timelines.unsubscribe(&room, "active").await;
        assert_eq!(timelines.read_through(&room, "active", event_id!("$a"))
            .await.unwrap_err().code, "stale_subscription");
        timelines.shutdown().await;
    }

    fn reaction(event_id: &str, sender: &str, target: &str, key: &str) -> Value {
        json!({
            "type":"m.reaction", "event_id":event_id, "sender":sender, "origin_server_ts":1100,
            "content":{"m.relates_to":{"rel_type":"m.annotation", "event_id":target, "key":key}}
        })
    }

    #[tokio::test]
    async fn sdk_reactions_project_counts_and_own_keys_and_toggle_through_the_queue() {
        let (server, _client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$target", "@other:example.org", json!({"msgtype":"m.text", "body":"hi"})),
            reaction("$r1", "@other:example.org", "$target", "👍"),
            reaction("$r2", "@me:example.org", "$target", "👍"),
            reaction("$r3", "@other:example.org", "$target", "🎉"),
        ]).await;
        let room = owned_room_id!("!room:example.org");
        let row = snapshot_row(&timelines, event_id!("$target")).await;
        assert_eq!(row["can_react"], true);
        assert_eq!(row["reactions"], json!([
            {"key":"👍", "count":2, "own":true}, {"key":"🎉", "count":1, "own":false},
        ]));
        // Removing a remote reaction redacts it directly; adding one is queued.
        Mock::given(method("PUT"))
            .and(path_regex(r"/rooms/.*/redact/\$r2/"))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({"event_id":"$undo"})))
            .expect(1)
            .mount(&server).await;
        assert!(timelines.react(&room, "active", event_id!("$target"), "🎉").await.unwrap());
        assert!(!timelines.react(&room, "active", event_id!("$target"), "👍").await.unwrap());
        let row = snapshot_row(&timelines, event_id!("$target")).await;
        assert_eq!(row["reactions"], json!([
            {"key":"👍", "count":1, "own":false}, {"key":"🎉", "count":2, "own":true},
        ]));
        assert_eq!(timelines.react(&room, "active", event_id!("$target"), " ")
            .await.unwrap_err().code, "invalid_params");
        assert_eq!(timelines.react(&room, "active", event_id!("$missing"), "👍")
            .await.unwrap_err().code, "event_missing");
        timelines.shutdown().await;
    }

    fn state(event_id: &str, sender: &str, kind: &str, key: &str, content: Value) -> Value {
        json!({
            "type":kind, "event_id":event_id, "sender":sender, "state_key":key,
            "origin_server_ts":1200, "content":content
        })
    }

    #[tokio::test]
    async fn sdk_state_changes_project_readable_sentences_and_hide_configuration() {
        let mut renamed = state("$rename", "@other:example.org", "m.room.member",
            "@other:example.org", json!({"membership":"join", "displayname":"Robin"}));
        renamed["unsigned"] = json!({"prev_content":{"membership":"join", "displayname":"Rob"}});
        let mut kick = state("$kick", "@me:example.org", "m.room.member", "@new:example.org",
            json!({"membership":"leave", "reason":"spam"}));
        kick["unsigned"] = json!({"prev_content":{"membership":"join", "displayname":"Newcomer"}});
        let (_server, _client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![
            state("$join", "@new:example.org", "m.room.member", "@new:example.org",
                json!({"membership":"join", "displayname":"Newcomer"})),
            kick,
            renamed,
            state("$topic", "@other:example.org", "m.room.topic", "", json!({"topic":"Plans"})),
            state("$rules", "@other:example.org", "m.room.join_rules", "",
                json!({"join_rule":"public"})),
        ]).await;
        let row = snapshot_row(&timelines, event_id!("$join")).await;
        assert_eq!((&row["kind"], &row["described"], &row["state_type"], &row["body"]),
            (&json!("state"), &json!(true), &json!("m.room.member"), &json!("Newcomer joined")));
        assert_eq!(snapshot_row(&timelines, event_id!("$kick")).await["body"],
            "@me:example.org removed Newcomer: spam");
        assert_eq!(snapshot_row(&timelines, event_id!("$rename")).await["body"],
            "Rob changed their name to Robin");
        assert_eq!(snapshot_row(&timelines, event_id!("$topic")).await["body"],
            "Robin changed the topic: Plans");
        let rules = snapshot_row(&timelines, event_id!("$rules")).await;
        assert_eq!((&rules["kind"], &rules["described"]), (&json!("state"), &Value::Null));
        timelines.shutdown().await;
    }

    async fn local_row(timeline: &Timeline, predicate: impl Fn(&Value) -> bool) -> Option<Value> {
        let own = timeline.room().own_user_id().to_owned();
        timeline.items().await.iter()
            .map(|row| item(row, MutationPermissions::default(), &own))
            .find(|row| row["event_id"].is_null() && predicate(row))
    }

    async fn wait_local(timeline: &Timeline, predicate: impl Fn(&Value) -> bool) -> Value {
        tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                if let Some(row) = local_row(timeline, &predicate).await {
                    return row;
                }
                tokio::time::sleep(Duration::from_millis(20)).await;
            }
        }).await.expect("local echo did not reach the expected state")
    }

    #[tokio::test]
    async fn sdk_failed_sends_can_be_retried_and_unsent_ones_discarded() {
        let (server, client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![]).await;
        let room = owned_room_id!("!room:example.org");
        let timeline = {
            let (subscription, _operation) = timelines.mutation_subscription(&room, "active").await.unwrap();
            mutation_timeline(&subscription).unwrap().clone()
        };
        timeline.send(RoomMessageEventContent::text_plain("withdrawn").into()).await.unwrap();
        let queued = wait_local(&timeline, |row| row["body"] == "withdrawn").await;
        assert_eq!((&queued["send_state"], &queued["can_discard"]), (&json!("pending"), &json!(true)));
        let txn = queued["transaction_id"].as_str().unwrap().to_owned();
        assert!(timelines.discard_send(&room, "active", &txn).await.unwrap());
        tokio::time::timeout(Duration::from_secs(10), async {
            while local_row(&timeline, |row| row["transaction_id"] == txn.as_str()).await.is_some() {
                tokio::time::sleep(Duration::from_millis(20)).await;
            }
        }).await.unwrap();

        let rejection = Mock::given(method("PUT"))
            .and(path_regex(r"/rooms/.*/send/m\.room\.message/"))
            .respond_with(ResponseTemplate::new(403)
                .set_body_json(json!({"errcode":"M_FORBIDDEN", "error":"no"})))
            .mount_as_scoped(&server).await;
        timeline.send(RoomMessageEventContent::text_plain("parked").into()).await.unwrap();
        client.send_queue().set_enabled(true).await;
        let failed = wait_local(&timeline, |row| row["send_state"] == "failed").await;
        assert_eq!((&failed["can_retry"], &failed["error"]),
            (&json!(true), &json!("Not sent: the server rejected this message")));
        let txn = failed["transaction_id"].as_str().unwrap().to_owned();
        assert_eq!(timelines.discard_send(&room, "active", "unknown").await.unwrap_err().code,
            "unsent_missing");
        drop(rejection);
        Mock::given(method("PUT"))
            .and(path_regex(r"/rooms/.*/send/m\.room\.message/"))
            .respond_with(ResponseTemplate::new(200).set_body_json(json!({"event_id":"$sent"})))
            .mount(&server).await;
        timelines.retry_send(&room, "active", &txn).await.unwrap();
        tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                let row = timeline.items().await.iter().filter_map(|row| row.as_event())
                    .find(|event| event.transaction_id().is_some_and(|id| id.as_str() == txn))
                    .map(|event| matches!(event.send_state(), Some(EventSendState::Sent { .. })));
                if row == Some(true) {
                    break;
                }
                tokio::time::sleep(Duration::from_millis(20)).await;
            }
        }).await.expect("retried message was not sent");
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_typing_notifications_name_others_for_the_subscription() {
        let (server, client, timelines, mut rx) = sdk_fixture(Some(power_levels(0)), vec![]).await;
        let response = json!({
            "next_batch":"typing",
            "rooms":{"join":{"!room:example.org":{
                "timeline":{"events":[], "limited":false},
                "ephemeral":{"events":[{"type":"m.typing", "content":{
                    "user_ids":["@other:example.org", "@me:example.org"]
                }}]}
            }}}
        });
        let _sync = Mock::given(method("GET"))
            .and(path_regex(r"^/_matrix/client/(r0|v3)/sync$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(response))
            .mount_as_scoped(&server).await;
        client.sync_once(SyncSettings::default()).await.unwrap();
        let event = tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                let event: Value = serde_json::from_slice(&rx.recv().await.unwrap()).unwrap();
                if event["params"]["type"] == "typing" {
                    return event["params"].clone();
                }
            }
        }).await.expect("no typing event");
        assert_eq!((&event["subscription_id"], &event["users"]),
            (&json!("active"), &json!(["@other:example.org"])));
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_mutation_permissions_distinguish_author_moderator_and_missing_plaintext() {
        let mut gone = message("$gone", "@me:example.org", json!({}));
        gone["unsigned"] = json!({"redacted_because":{
            "type":"m.room.redaction", "event_id":"$redaction", "sender":"@me:example.org",
            "origin_server_ts":1001, "content":{"redacts":"$gone"}
        }});
        let encrypted = json!({
            "type":"m.room.encrypted", "event_id":"$encrypted", "sender":"@me:example.org",
            "origin_server_ts":1000, "content":{
                "algorithm":"m.megolm.v1.aes-sha2", "ciphertext":"aGVsbG8",
                "sender_key":"aGVsbG8", "device_id":"DEVICE", "session_id":"unavailable"
            }
        });
        let (server, client, timelines, mut rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$own", "@me:example.org", json!({"msgtype":"m.text", "body":"own"})),
            message("$other", "@other:example.org", json!({"msgtype":"m.text", "body":"other"})),
            gone, encrypted,
        ]).await;
        let room = owned_room_id!("!room:example.org");
        let own = snapshot_row(&timelines, event_id!("$own")).await;
        let other = snapshot_row(&timelines, event_id!("$other")).await;
        assert_eq!(own["can_edit"], true);
        assert_eq!(own["can_redact"], true);
        assert_eq!(other["can_edit"], false);
        assert_eq!(other["can_redact"], false);
        let encrypted = snapshot_row(&timelines, event_id!("$encrypted")).await;
        assert_eq!(encrypted["kind"], "encrypted");
        assert_eq!(encrypted["can_edit"], false);
        assert_eq!(encrypted["can_redact"], true);
        let gone = snapshot_row(&timelines, event_id!("$gone")).await;
        assert_eq!(gone["kind"], "redacted");
        assert_eq!(gone["can_edit"], false);
        assert_eq!(gone["can_redact"], false);
        assert_eq!(timelines.edit(&room, "active", event_id!("$other"), "illegal".into())
            .await.unwrap_err().code, "edit_forbidden");
        assert_eq!(timelines.redact(&room, "active", event_id!("$other"))
            .await.unwrap_err().code, "redaction_forbidden");
        assert_eq!(timelines.edit(&room, "active", event_id!("$encrypted"), "illegal".into())
            .await.unwrap_err().code, "edit_unavailable");
        assert_eq!(timelines.redact(&room, "active", event_id!("$gone"))
            .await.unwrap_err().code, "already_redacted");
        for id in ["active", "closed"] {
            let expected = if id == "active" { "event_missing" } else { "stale_subscription" };
            assert_eq!(timelines.edit(&room, id, event_id!("$missing"), "new".into())
                .await.unwrap_err().code, expected);
            assert_eq!(timelines.redact(&room, id, event_id!("$missing"))
                .await.unwrap_err().code, expected);
        }
        assert_eq!(snapshot_row(&timelines, event_id!("$other")).await["body"], "other");
        let (subscription, operation) = timelines.mutation_subscription(&room, "active").await.unwrap();
        let timeline = mutation_timeline(&subscription).unwrap();
        timeline.send(RoomMessageEventContent::text_plain("pending").into()).await.unwrap();
        let (mut local_items, local_updates) = timeline.subscribe().await;
        futures_util::pin_mut!(local_updates);
        tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                if let Some(local) = local_items.iter().find(|item| item.as_event()
                    .is_some_and(|event| event.event_id().is_none())) {
                    let flags = item(local, mutation_permissions(timeline.room()).await.unwrap(),
                        timeline.room().own_user_id());
                    assert_eq!(flags["can_edit"], false);
                    assert_eq!(flags["can_redact"], false);
                    assert_eq!(flags["can_read"], false);
                    break;
                }
                for diff in local_updates.next().await.unwrap() { diff.apply(&mut local_items); }
            }
        }).await.unwrap();
        drop(operation);

        // A PL-only sync changes capabilities on existing rows, even without
        // a message diff; a moderator still cannot edit another author's event.
        sdk_sync(&server, &client, Some(power_levels(50)), vec![]).await;
        let moderator_row = next_row(&mut rx, "$other", |row| row["can_redact"] == true).await;
        assert_eq!(moderator_row["id"], other["id"]);
        assert_eq!(moderator_row["can_edit"], false);
        assert_eq!(timelines.edit(&room, "active", event_id!("$other"), "illegal".into())
            .await.unwrap_err().code, "edit_forbidden");
        timelines.redact(&room, "active", event_id!("$other")).await.unwrap();
        let tombstone = next_row(&mut rx, "$other", |row| row["kind"] == "redacted").await;
        assert_eq!(tombstone["id"], other["id"]);
        assert_eq!(tombstone["event_id"], other["event_id"]);
        assert_ne!(tombstone["body"], other["body"]);
        assert_eq!(tombstone["can_redact"], false);
        timelines.shutdown().await;
    }

    const IMAGE_PNG: &[u8] = b"\x89PNG\r\n\x1a\n\
        \x00\x00\x00\x0dIHDR\x00\x00\x00\x01\x00\x00\x00\x01\
        \x08\x04\x00\x00\x00\xb5\x1c\x0c\x02\x00\x00\x00\x0bIDAT\
        \x78\xda\x63\xfc\xff\x1f\x00\x03\x03\x02\x00\xef\xa3\xf0\xad\
        \x00\x00\x00\x00IEND\xae\x42\x60\x82";

    fn encrypted_image_file() -> Value {
        json!({
            "url":"mxc://example.org/encrypted-image", "v":"v2",
            "key":{"kty":"oct", "alg":"A256CTR", "key_ops":["encrypt","decrypt"],
                "k":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", "ext":true},
            "iv":"AAAAAAAAAAAAAAAAAAAAAA",
            "hashes":{"sha256":"Q9pQ6XVX9EBh7tc2oNfdlmYmMA8SDoJsbzfhmqHZX7k"}
        })
    }

    #[tokio::test]
    async fn sdk_attachments_describe_and_save_to_new_files_only() {
        let (server, client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$file", "@other:example.org", json!({
                "msgtype":"m.file", "body":"notes", "filename":"notes.txt",
                "url":"mxc://example.org/notes", "info":{"mimetype":"text/plain", "size":5}
            })),
            message("$voice", "@other:example.org", json!({
                "msgtype":"m.audio", "body":"voice.ogg", "url":"mxc://example.org/voice",
                "info":{"duration":1500}
            })),
        ]).await;
        Mock::given(method("GET"))
            .and(path_regex(r"^/_matrix/media/(r0|v3)/download/example\.org/notes$"))
            .respond_with(ResponseTemplate::new(200).set_body_bytes(b"hello".to_vec()))
            .mount(&server).await;
        let room = owned_room_id!("!room:example.org");
        let file = snapshot_row(&timelines, event_id!("$file")).await;
        assert_eq!((&file["attachment"]["kind"], &file["attachment"]["filename"],
                    &file["attachment"]["mime_type"], &file["attachment"]["size"]),
            (&json!("file"), &json!("notes.txt"), &json!("text/plain"), &json!(5)));
        let voice = snapshot_row(&timelines, event_id!("$voice")).await;
        assert_eq!((&voice["attachment"]["kind"], &voice["attachment"]["duration_ms"]),
            (&json!("audio"), &json!(1500)));
        let store = tempfile::tempdir().unwrap();
        let downloads = tempfile::tempdir().unwrap();
        let destination = downloads.path().join("notes.txt");
        let slots = tokio::sync::Semaphore::new(1);
        let source = file["attachment"]["source_id"].as_str().unwrap().to_owned();
        let save = async |path: std::path::PathBuf| {
            let lease = timelines.media_lease(&room, "active", event_id!("$file"), source.clone())
                .await.unwrap();
            media::save(store.path().to_owned(), path, download(&client, &timelines, &lease,
                CancellationToken::new(), CancellationToken::new(), &slots)).await
        };
        save(destination.clone()).await.unwrap();
        assert_eq!(std::fs::read(&destination).unwrap(), b"hello");
        assert_eq!(save(destination.clone()).await.unwrap_err().code, "media_save_failed");
        assert_eq!(save(store.path().join("inside")).await.unwrap_err().code, "media_save_failed");
        assert_eq!(std::fs::read_dir(downloads.path()).unwrap().count(), 1);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_images_download_original_decrypted_bytes_and_reuse_only_complete_sources() {
        let mut bad_file = encrypted_image_file();
        bad_file["hashes"]["sha256"] = json!("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
        let (server, client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$image", "@other:example.org", json!({
                "msgtype":"m.image", "body":"caption", "filename":"image.png",
                "url":"mxc://example.org/image",
                "info":{"mimetype":"image/png", "w":0, "h":1, "size":IMAGE_PNG.len()}
            })),
            message("$encrypted-image", "@other:example.org", json!({
                "msgtype":"m.image", "body":"encrypted.png", "file":encrypted_image_file()
            })),
            message("$bad-image", "@other:example.org", json!({
                "msgtype":"m.image", "body":"bad.png", "file":bad_file
            })),
            message("$text", "@other:example.org", json!({
                "msgtype":"m.text", "body":"image.png", "url":"mxc://example.org/image"
            })),
            message("$file", "@other:example.org", json!({
                "msgtype":"m.file", "body":"image.png", "url":"mxc://example.org/image",
                "info":{"mimetype":"image/png"}
            })),
        ]).await;
        let ciphertext = "55c58e3faf4a9383ad48a219dbcc64d5530f8afac74536b8a167b4f1c47e6f87cca7403d46292f2f53361fb0460c8218716301cad805dad9d1a2f58e3c437bca7308d1aa";
        let ciphertext: Vec<u8> = (0..ciphertext.len()).step_by(2)
            .map(|offset| u8::from_str_radix(&ciphertext[offset..offset + 2], 16).unwrap()).collect();
        Mock::given(method("GET"))
            .and(path_regex(r"^/_matrix/media/(r0|v3)/download/example\.org/image$"))
            .respond_with(ResponseTemplate::new(200).set_body_bytes(IMAGE_PNG))
            .expect(1).mount(&server).await;
        Mock::given(method("GET"))
            .and(path_regex(r"^/_matrix/media/(r0|v3)/download/example\.org/encrypted-image$"))
            .respond_with(ResponseTemplate::new(200).set_body_bytes(ciphertext))
            .expect(2).mount(&server).await;
        let directory = tempfile::tempdir().unwrap();
        std::fs::set_permissions(directory.path(), std::fs::Permissions::from_mode(0o700)).unwrap();
        let store = Arc::new(crate::storage::Store::open(directory.path().into()).unwrap());
        let generation = uuid::Uuid::new_v4().to_string();
        let slots = tokio::sync::Semaphore::new(2);
        let room = owned_room_id!("!room:example.org");
        let plain = snapshot_row(&timelines, event_id!("$image")).await;
        assert_eq!(plain["body"], "caption");
        assert_eq!(plain["image"]["filename"], "image.png");
        assert_eq!(plain["image"]["mime_type"], "image/png");
        assert_eq!(plain["image"]["height"], 1);
        assert_eq!(plain["image"]["size"], IMAGE_PNG.len());
        assert!(plain["image"].get("width").is_none());
        assert!(plain["image"].get("url").is_none());
        assert!(plain["image"].get("file").is_none());
        for id in ["$text", "$file"] {
            let event_id = EventId::parse(id).unwrap();
            assert!(snapshot_row(&timelines, &event_id).await.get("image").is_none());
        }
        for id in ["$image", "$encrypted-image"] {
            let event_id = EventId::parse(id).unwrap();
            let row = snapshot_row(&timelines, &event_id).await;
            let source = row["image"]["source_id"].as_str().unwrap();
            let lease = timelines.media_lease(&room, "active", &event_id, source.into()).await.unwrap();
            let first = media::image(store.clone(), generation.clone(), download(&client,
                &timelines, &lease, CancellationToken::new(), CancellationToken::new(), &slots)).await.unwrap();
            assert_eq!(std::fs::read(&first).unwrap(), IMAGE_PNG);
            assert!(first.starts_with(directory.path().join("media").join(&generation)));
            let cached = media::image(store.clone(), generation.clone(), download(&client,
                &timelines, &lease, CancellationToken::new(), CancellationToken::new(), &slots)).await.unwrap();
            assert_eq!(first, cached);
        }
        let bad = snapshot_row(&timelines, event_id!("$bad-image")).await;
        let good = snapshot_row(&timelines, event_id!("$encrypted-image")).await;
        assert_ne!(bad["image"]["source_id"], good["image"]["source_id"]);
        let source = bad["image"]["source_id"].as_str().unwrap();
        let lease = timelines.media_lease(&room, "active", event_id!("$bad-image"), source.into()).await.unwrap();
        assert!(media::image(store.clone(), generation.clone(), download(&client, &timelines, &lease,
            CancellationToken::new(), CancellationToken::new(), &slots)).await.is_err());
        assert_eq!(store.cached_image(&generation, source).unwrap(), None);
        drop(lease);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_media_leases_reject_source_replacement_redaction_and_reopened_subscription() {
        let (server, client, timelines, mut rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$image", "@other:example.org", json!({
                "msgtype":"m.image", "body":"image.png", "url":"mxc://example.org/image"
            })),
        ]).await;
        let room = owned_room_id!("!room:example.org");
        let row = snapshot_row(&timelines, event_id!("$image")).await;
        let source = row["image"]["source_id"].as_str().unwrap().to_owned();
        assert_eq!(timelines.media_lease(&room, "old", event_id!("$image"), source.clone())
            .await.err().unwrap().code, "stale_subscription");
        assert_eq!(timelines.media_lease(&room, "active", event_id!("$image"), "0".repeat(64))
            .await.err().unwrap().code, "stale_media");
        let lease = timelines.media_lease(&room, "active", event_id!("$image"), source).await.unwrap();
        let new_content = json!({
            "msgtype":"m.image", "body":"replacement.png", "url":"mxc://example.org/replacement"
        });
        sdk_sync(&server, &client, None, vec![
            message("$replacement", "@other:example.org", json!({
                "msgtype":"m.image", "body":"* replacement.png", "url":"mxc://example.org/replacement",
                "m.new_content":new_content, "m.relates_to":{"rel_type":"m.replace", "event_id":"$image"}
            })),
        ]).await;
        let replacement = next_row(&mut rx, "$image", |row| row["body"] == "replacement.png").await;
        assert_ne!(replacement["image"]["source_id"], row["image"]["source_id"]);
        assert_eq!(lease.validate(&timelines).await.unwrap_err().code, "stale_media");
        let source = replacement["image"]["source_id"].as_str().unwrap().to_owned();
        let replacement_lease = timelines.media_lease(&room, "active", event_id!("$image"), source).await.unwrap();
        sdk_sync(&server, &client, None, vec![json!({
            "type":"m.room.redaction", "event_id":"$redaction", "sender":"@other:example.org",
            "origin_server_ts":2000, "redacts":"$image", "content":{"redacts":"$image"}
        })]).await;
        let redacted = next_row(&mut rx, "$image", |row| row["kind"] == "redacted").await;
        assert!(redacted.get("image").is_none());
        assert_eq!(replacement_lease.validate(&timelines).await.unwrap_err().code, "stale_media");
        drop(replacement_lease);
        let (_, _, operation) = timelines.reserve(room.clone(), "replacement".into()).await.unwrap();
        drop(operation);
        assert_eq!(lease.validate(&timelines).await.unwrap_err().code, "stale_subscription");
        drop(lease);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_image_download_revokes_account_and_transport_before_publication() {
        for cancel_transport in [false, true] {
            let (server, client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![
                message("$image", "@other:example.org", json!({
                    "msgtype":"m.image", "body":"image.png", "url":"mxc://example.org/image"
                })),
            ]).await;
            let cancel = CancellationToken::new();
            let transport = CancellationToken::new();
            let revoke = if cancel_transport { transport.clone() } else { cancel.clone() };
            Mock::given(method("GET"))
                .and(path_regex(r"^/_matrix/media/(r0|v3)/download/example\.org/image$"))
                .respond_with(move |_: &wiremock::Request| {
                    revoke.cancel();
                    ResponseTemplate::new(200).set_body_bytes(IMAGE_PNG)
                }).expect(1).mount(&server).await;
            let directory = tempfile::tempdir().unwrap();
            std::fs::set_permissions(directory.path(), std::fs::Permissions::from_mode(0o700)).unwrap();
            let store = Arc::new(crate::storage::Store::open(directory.path().into()).unwrap());
            let generation = uuid::Uuid::new_v4().to_string();
            let row = snapshot_row(&timelines, event_id!("$image")).await;
            let source = row["image"]["source_id"].as_str().unwrap();
            let lease = timelines.media_lease(&owned_room_id!("!room:example.org"), "active",
                event_id!("$image"), source.into()).await.unwrap();
            let slots = tokio::sync::Semaphore::new(1);
            assert_eq!(media::image(store.clone(), generation.clone(), download(&client, &timelines,
                &lease, cancel, transport, &slots)).await.unwrap_err().code, "media_cancelled");
            assert_eq!(store.cached_image(&generation, source).unwrap(), None);
            drop(lease);
            timelines.shutdown().await;
        }
    }

    #[tokio::test]
    async fn sdk_edits_preserve_message_subtypes_reply_identity_and_media_source() {
        let encrypted_file = json!({
            "url":"mxc://example.org/encrypted-image", "v":"v2",
            "key":{"kty":"oct", "alg":"A256CTR", "key_ops":["encrypt","decrypt"],
                "k":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", "ext":true},
            "iv":"AAAAAAAAAAAAAAAAAAAAAA",
            "hashes":{"sha256":"AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"}
        });
        let (_server, _client, timelines, mut rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$text", "@me:example.org", json!({
                "msgtype":"m.text", "body":"original", "format":"org.matrix.custom.html",
                "formatted_body":"<b>original</b>",
                "m.relates_to":{"m.in_reply_to":{"event_id":"$notice"}},
                "m.mentions":{"user_ids":["@other:example.org"]}
            })),
            message("$notice", "@me:example.org", json!({"msgtype":"m.notice", "body":"notice"})),
            message("$emote", "@me:example.org", json!({"msgtype":"m.emote", "body":"emote"})),
            message("$image", "@me:example.org", json!({
                "msgtype":"m.image", "body":"image.png", "url":"mxc://example.org/image"
            })),
            message("$encrypted-image", "@me:example.org", json!({
                "msgtype":"m.image", "body":"encrypted.png", "file":encrypted_file
            })),
        ]).await;
        let room = owned_room_id!("!room:example.org");
        for id in ["$text", "$notice", "$emote", "$image", "$encrypted-image"] {
            let event_id = EventId::parse(id).unwrap();
            let original = snapshot_row(&timelines, &event_id).await;
            assert_eq!(original["can_edit"], true);
            timelines.edit(&room, "active", &event_id, "changed".into()).await.unwrap();
            let edited = next_row(&mut rx, id, |row| row["body"] == "changed").await;
            assert_eq!(edited["id"], original["id"]);
            assert_eq!(edited["event_id"], original["event_id"]);
            if original.get("image").is_some() {
                assert_eq!(edited["image"]["source_id"], original["image"]["source_id"]);
                assert_eq!(edited["image"]["filename"], original["image"]["filename"]);
            }
        }
        let (subscription, operation) = timelines.mutation_subscription(&room, "active").await.unwrap();
        let items = mutation_timeline(&subscription).unwrap().items().await;
        for (id, msgtype) in [
            ("$text", "m.text"), ("$notice", "m.notice"), ("$emote", "m.emote"),
            ("$image", "m.image"), ("$encrypted-image", "m.image"),
        ] {
            let event_id = EventId::parse(id).unwrap();
            let event = items.iter().filter_map(|item| item.as_event())
                .find(|event| event.event_id() == Some(event_id.as_ref())).unwrap();
            let message = event.content().as_message().unwrap();
            assert_eq!(message.msgtype().msgtype(), msgtype);
            assert!(message.is_edited());
            if id == "$text" {
                assert_eq!(event.content().in_reply_to().unwrap().event_id.as_str(), "$notice");
                assert!(message.mentions().unwrap().user_ids.contains(
                    matrix_sdk::ruma::user_id!("@other:example.org")));
                let MessageType::Text(text) = message.msgtype() else { panic!("text type changed") };
                assert!(text.formatted.is_none());
            }
            if id == "$image" || id == "$encrypted-image" {
                let MessageType::Image(image) = message.msgtype() else { panic!("media type changed") };
                let (filename, source) = if id == "$image" {
                    ("image.png", json!({"url":"mxc://example.org/image"}))
                } else {
                    ("encrypted.png", json!({"file":encrypted_file}))
                };
                assert_eq!(image.filename(), filename);
                let source: matrix_sdk::ruma::events::room::MediaSource =
                    serde_json::from_value(source).unwrap();
                assert_eq!(serde_json::to_value(&image.source).unwrap(),
                           serde_json::to_value(source).unwrap());
                assert_eq!(image.caption(), Some("changed"));
            }
        }
        drop(operation);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_unknown_and_revoked_power_levels_never_grant_mutation_privileges() {
        let (server, client, timelines, mut rx) = sdk_fixture(None, vec![
            message("$own", "@me:example.org", json!({"msgtype":"m.text", "body":"own"})),
        ]).await;
        let room = owned_room_id!("!room:example.org");
        let unknown = snapshot_row(&timelines, event_id!("$own")).await;
        assert_eq!(unknown["can_edit"], false);
        assert_eq!(unknown["can_redact"], false);
        assert_eq!(timelines.edit(&room, "active", event_id!("$own"), "new".into())
            .await.unwrap_err().code, "permissions_unavailable");
        assert_eq!(timelines.redact(&room, "active", event_id!("$own"))
            .await.unwrap_err().code, "permissions_unavailable");
        sdk_sync(&server, &client, Some(power_levels(0)), vec![]).await;
        next_row(&mut rx, "$own", |row| row["can_edit"] == true && row["can_redact"] == true).await;
        sdk_sync(&server, &client, Some(json!({
            "users":{"@me:example.org":0},
            "events":{"m.room.message":50, "m.room.redaction":50}
        })), vec![]).await;
        let revoked = next_row(&mut rx, "$own", |row| row["can_edit"] == false && row["can_redact"] == false).await;
        assert_eq!(revoked["id"], unknown["id"]);
        assert_eq!(timelines.edit(&room, "active", event_id!("$own"), "new".into())
            .await.unwrap_err().code, "edit_forbidden");
        assert_eq!(timelines.redact(&room, "active", event_id!("$own"))
            .await.unwrap_err().code, "redaction_forbidden");
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn mutation_lease_orders_replacement_without_blocking_other_rooms() {
        let timelines = Arc::new(Timelines::new(CancellationToken::new()));
        let room = owned_room_id!("!room:example.org");
        let (old, _, operation) = timelines.reserve(room.clone(), "old".into()).await.unwrap();
        drop(operation);
        let guard = old.mutations.lock().await;
        let pending = {
            let timelines = timelines.clone();
            let room = room.clone();
            tokio::spawn(async move { timelines.reserve(room, "replacement".into()).await })
        };
        let (_, _, unrelated_operation) = timelines.reserve(
            owned_room_id!("!unrelated:example.org"), "unrelated".into()).await.unwrap();
        drop(unrelated_operation);
        assert!(timelines.is_current(&room, &old).await);
        drop(guard);
        let (current, _, operation) = pending.await.unwrap().unwrap();
        drop(operation);
        assert!(old.cancel.is_cancelled());
        assert!(timelines.is_current(&room, &current).await);
        assert_eq!(timelines.edit(&room, "old", event_id!("$target"), "new".into())
            .await.unwrap_err().code, "stale_subscription");
        assert_eq!(timelines.redact(&room, "old", event_id!("$target"))
            .await.unwrap_err().code, "stale_subscription");
        timelines.unsubscribe(&room, "replacement").await;
        assert_eq!(timelines.redact(&room, "replacement", event_id!("$target"))
            .await.unwrap_err().code, "stale_subscription");
        timelines.shutdown().await;
    }

    fn attachment(path: &std::path::Path, filename: &str, mime_type: &str) -> Attachment {
        Attachment { path: path.into(), filename: filename.into(), mime_type: mime_type.into(),
            width: None, height: None }
    }

    async fn queued_content(client: &Client) -> Vec<Value> {
        let echoes = client.send_queue().local_echoes().await.unwrap();
        echoes.get(&owned_room_id!("!room:example.org")).into_iter().flatten()
            .filter_map(|echo| {
                let matrix_sdk::send_queue::LocalEchoContent::Event { serialized_event, .. } =
                    &echo.content else { return None; };
                Some(serde_json::from_str(serialized_event.raw().0.json().get()).unwrap())
            }).collect()
    }

    fn reply_message(event_id: &str, target: &str) -> Value {
        message(event_id, "@me:example.org", json!({
            "msgtype":"m.text", "body":"response",
            "m.relates_to":{"m.in_reply_to":{"event_id":target}}
        }))
    }

    #[tokio::test]
    async fn sdk_send_queues_typed_media_first_caption_reply_and_owned_bytes() {
        let (_server, client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$target", "@other:example.org", json!({"msgtype":"m.text", "body":"target"})),
        ]).await;
        let room = owned_room_id!("!room:example.org");
        let directory = tempfile::tempdir().unwrap();
        let image_path = directory.path().join("image.png");
        let file_path = directory.path().join("document.bin");
        std::fs::write(&image_path, IMAGE_PNG).unwrap();
        std::fs::write(&file_path, b"document").unwrap();
        let mut image = attachment(&image_path, "image.png", "image/png");
        image.width = Some(1);
        image.height = Some(1);
        assert_eq!(timelines.send(&room, "active", "caption".into(), vec![],
            vec![image, attachment(&file_path, "document.bin", "application/octet-stream")],
            Some(event_id!("$target").to_owned())).await.unwrap(),
            json!({"accepted":2, "total":2}));
        std::fs::remove_file(&image_path).unwrap();
        std::fs::remove_file(&file_path).unwrap();
        let content = queued_content(&client).await;
        assert_eq!(content.len(), 2);
        assert_eq!(content[0]["msgtype"], "m.image");
        assert_eq!(content[0]["body"], "caption");
        assert_eq!(content[0]["filename"], "image.png");
        assert_eq!(content[0]["info"]["size"], IMAGE_PNG.len());
        assert_eq!(content[0]["info"]["mimetype"], "image/png");
        assert_eq!(content[0]["info"]["w"], 1);
        assert_eq!(content[0]["info"]["h"], 1);
        assert_eq!(content[1]["msgtype"], "m.file");
        assert_eq!(content[1]["body"], "document.bin");
        assert_eq!(content[1]["info"]["size"], 8);
        for (content, bytes) in content.iter().zip([IMAGE_PNG, b"document".as_slice()]) {
            assert_eq!(content["m.relates_to"]["m.in_reply_to"]["event_id"], "$target");
            assert_eq!(content["m.mentions"]["user_ids"], json!(["@other:example.org"]));
            let source: matrix_sdk::ruma::events::room::MediaSource =
                serde_json::from_value(json!({"url":content["url"]})).unwrap();
            let request = matrix_sdk::media::MediaRequestParameters {
                source, format: matrix_sdk::media::MediaFormat::File,
            };
            assert_eq!(client.media().get_media_content(&request, true).await.unwrap(), bytes);
        }
        assert_eq!(timelines.send(&room, "active", "text reply".into(), vec![], vec![],
            Some(event_id!("$target").to_owned())).await.unwrap(),
            json!({"accepted":1, "total":1}));
        let content = queued_content(&client).await;
        let text = content.iter().find(|event| event["msgtype"] == "m.text").unwrap();
        assert_eq!(text["body"], "text reply");
        assert_eq!(text["m.relates_to"]["m.in_reply_to"]["event_id"], "$target");
        let source = directory.path().join("only.png");
        std::fs::write(&source, IMAGE_PNG).unwrap();
        assert_eq!(timelines.send(&room, "active", String::new(), vec![],
            vec![attachment(&source, "only.png", "application/octet-stream")], None)
            .await.unwrap(), json!({"accepted":1, "total":1}));
        let content = queued_content(&client).await;
        let file = content.iter().find(|event| event["body"] == "only.png").unwrap();
        assert_eq!(file["msgtype"], "m.file");
        assert!(file.get("filename").is_none());
        timelines.shutdown().await;
    }

    fn download<'a>(
        client: &Client,
        timelines: &'a Timelines,
        lease: &'a MediaLease,
        cancel: CancellationToken,
        transport: CancellationToken,
        slots: &'a tokio::sync::Semaphore,
    ) -> media::Download<'a> {
        media::Download { client: client.clone(), timelines, lease, cancel, transport, slots }
    }

    fn segment(text: &str, user_id: Option<&str>) -> Segment {
        Segment { text: text.into(), user_id: user_id.map(|id| UserId::parse(id).unwrap()) }
    }

    #[tokio::test]
    async fn sdk_mentions_become_pills_and_intentional_mentions() {
        let (_server, client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![]).await;
        let room = owned_room_id!("!room:example.org");
        timelines.send(&room, "active", "hi <Al>!\nok".into(), vec![
            segment("hi ", None), segment("<Al>", Some("@al:example.org")), segment("!\nok", None),
        ], vec![], None).await.unwrap();
        timelines.send(&room, "active", "plain Al".into(), vec![], vec![], None).await.unwrap();
        assert_eq!(timelines.send(&room, "active", "mismatch".into(), vec![
            segment("other", Some("@al:example.org")),
        ], vec![], None).await.unwrap_err().code, "invalid_params");
        let queued = queued_content(&client).await;
        assert_eq!(queued[0]["formatted_body"],
            "hi <a href=\"https://matrix.to/#/@al:example.org\">&lt;Al&gt;</a>!<br>ok");
        assert_eq!(queued[0]["format"], "org.matrix.custom.html");
        assert_eq!(queued[0]["m.mentions"], json!({"user_ids":["@al:example.org"]}));
        assert_eq!(queued[1]["m.mentions"], json!({}));
        assert!(queued[1].get("formatted_body").is_none());
        assert_eq!(queued.len(), 2);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_send_validation_and_revoked_admission_leave_queue_unchanged() {
        let (_server, client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![]).await;
        let room = owned_room_id!("!room:example.org");
        let source = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(source.path(), IMAGE_PNG).unwrap();
        for invalid in [
            attachment(source.path(), "bad.png", "not a mime type"),
            attachment(source.path(), "bad.png", "image/*"),
            attachment(std::path::Path::new("relative.png"), "bad.png", "image/png"),
            attachment(source.path().parent().unwrap(), "directory", "application/octet-stream"),
            Attachment { width: Some(0), ..attachment(source.path(), "bad.png", "image/png") },
        ] {
            assert!(timelines.send(&room, "active", "caption".into(), vec![],
                vec![attachment(source.path(), "good.png", "image/png"), invalid], None)
                .await.is_err());
            assert!(queued_content(&client).await.is_empty());
        }
        assert_eq!(timelines.send(&room, "active", String::new(), vec![], vec![], None)
            .await.unwrap_err().code, "invalid_params");
        assert_eq!(timelines.send(&room, "active", "x".repeat(32 * 1024 + 1), vec![], vec![], None)
            .await.unwrap_err().code, "invalid_params");
        assert_eq!(timelines.send(&room, "active", "reply".into(), vec![], vec![],
            Some(event_id!("$missing").to_owned())).await.unwrap_err().code, "reply_unavailable");
        let timelines = Arc::new(timelines);
        let (subscription, operation) = timelines.mutation_subscription(&room, "active").await.unwrap();
        drop(operation);
        let guard = subscription.mutations.lock().await;
        let send = {
            let timelines = timelines.clone();
            let room = room.clone();
            tokio::spawn(async move {
                timelines.send(&room, "active", "must not publish".into(), vec![], vec![], None).await
            })
        };
        // A stream/account cancellation revokes an admission waiting on the
        // mutation fence; it cannot enqueue using its captured generation.
        subscription.cancel.cancel();
        drop(guard);
        assert_eq!(send.await.unwrap().unwrap_err().code, "stale_subscription");
        assert!(queued_content(&client).await.is_empty());
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_send_partial_result_preserves_confirmed_prefix_and_zero_is_outer_error() {
        let (_server, client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![]).await;
        let room = owned_room_id!("!room:example.org");
        let source = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(source.path(), IMAGE_PNG).unwrap();
        // Linux exposes a regular, openable file whose read at offset zero
        // genuinely fails. No fake SDK forwarding or injected queue result.
        let unreadable = std::path::Path::new("/proc/self/mem");
        let result = timelines.send(&room, "active", "consumed caption".into(), vec![], vec![
            attachment(source.path(), "accepted.png", "image/png"),
            attachment(unreadable, "memory.bin", "application/octet-stream"),
        ], None).await.unwrap();
        assert_eq!(result["accepted"], 1);
        assert_eq!(result["total"], 2);
        assert_eq!(result["error"]["code"], "attachment_read_failed");
        let content = queued_content(&client).await;
        assert_eq!(content.len(), 1);
        assert_eq!(content[0]["body"], "consumed caption");
        assert_eq!(content[0]["filename"], "accepted.png");
        assert_eq!(timelines.send(&room, "active", String::new(), vec![],
            vec![attachment(unreadable, "memory.bin", "application/octet-stream")], None)
            .await.unwrap_err().code, "attachment_read_failed");
        assert_eq!(queued_content(&client).await.len(), 1);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_reply_projection_tracks_ready_edits_redaction_and_no_fallback_inference() {
        let (server, client, timelines, mut rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$target", "@other:example.org", json!({"msgtype":"m.text", "body":"original"})),
            reply_message("$reply", "$target"),
            json!({
                "type":"m.room.encrypted", "event_id":"$encrypted-target",
                "sender":"@other:example.org", "origin_server_ts":1000,
                "content":{"algorithm":"m.megolm.v1.aes-sha2", "ciphertext":"aGVsbG8",
                    "sender_key":"aGVsbG8", "device_id":"OTHER", "session_id":"unavailable"}
            }),
            reply_message("$encrypted-reply", "$encrypted-target"),
            json!({
                "type":"m.room.topic", "state_key":"", "event_id":"$state-target",
                "sender":"@other:example.org", "origin_server_ts":1000, "content":{"topic":"topic"}
            }),
            reply_message("$state-reply", "$state-target"),
            message("$fallback", "@other:example.org", json!({
                "msgtype":"m.text", "body":"> <@me:example.org> pretend quote\n\nplain",
                "format":"org.matrix.custom.html",
                "formatted_body":"<mx-reply><blockquote>not a relationship</blockquote></mx-reply>plain"
            })),
        ]).await;
        let original = snapshot_row(&timelines, event_id!("$reply")).await;
        assert_eq!(original["can_reply"], true);
        assert_eq!(original["reply"]["event_id"], "$target");
        assert_eq!(original["reply"]["status"], "ready");
        assert_eq!(original["reply"]["sender"], "@other:example.org");
        assert_eq!(original["reply"]["kind"], "message");
        assert_eq!(original["reply"]["body"], "original");
        assert!(snapshot_row(&timelines, event_id!("$fallback")).await.get("reply").is_none());
        let encrypted = snapshot_row(&timelines, event_id!("$encrypted-reply")).await;
        assert_eq!(encrypted["reply"]["status"], "ready");
        assert_eq!(encrypted["reply"]["kind"], "encrypted");
        assert_eq!(encrypted["reply"]["event_id"], "$encrypted-target");
        assert_ne!(encrypted["reply"]["body"], "aGVsbG8");
        let state = snapshot_row(&timelines, event_id!("$state-reply")).await;
        assert_eq!(state["reply"]["status"], "ready");
        assert_eq!(state["reply"]["kind"], "state");
        assert_eq!(state["reply"]["event_id"], "$state-target");
        sdk_sync(&server, &client, None, vec![
            message("$edit-target", "@other:example.org", json!({
                "msgtype":"m.text", "body":"* changed",
                "m.new_content":{"msgtype":"m.text", "body":"changed"},
                "m.relates_to":{"rel_type":"m.replace", "event_id":"$target"}
            })),
        ]).await;
        let edited = next_row(&mut rx, "$reply", |row| row["reply"]["body"] == "changed").await;
        assert_eq!(edited["id"], original["id"]);
        assert_eq!(edited["reply"]["event_id"], "$target");
        sdk_sync(&server, &client, None, vec![json!({
            "type":"m.room.redaction", "event_id":"$redact-target", "sender":"@other:example.org",
            "origin_server_ts":2000, "redacts":"$target", "content":{"redacts":"$target"}
        })]).await;
        let redacted = next_row(&mut rx, "$reply", |row| row["reply"]["kind"] == "redacted").await;
        assert_eq!(redacted["reply"]["status"], "ready");
        assert_eq!(redacted["reply"]["event_id"], "$target");
        assert_ne!(redacted["reply"]["body"], "changed");
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_unloaded_reply_fetch_publishes_ready_error_without_blocking_vector_updates() {
        let server = MockServer::start().await;
        let mut remote = message("$remote", "@other:example.org",
            json!({"msgtype":"m.text", "body":"remote preview"}));
        remote["room_id"] = json!("!room:example.org");
        Mock::given(method("GET")).and(path_regex(r"/event/.*remote$"))
            .respond_with(ResponseTemplate::new(200).set_body_json(remote)
                .set_delay(Duration::from_secs(2)))
            .expect(1).mount(&server).await;
        Mock::given(method("GET")).and(path_regex(r"/event/.*gone$"))
            .respond_with(ResponseTemplate::new(404)
                .set_body_json(json!({"errcode":"M_NOT_FOUND", "error":"gone"})))
            .expect(1).mount(&server).await;
        let (server, client, timelines, mut rx) = sdk_fixture_with_server(server,
            Some(power_levels(0)), vec![
                reply_message("$remote-reply", "$remote"), reply_message("$gone-reply", "$gone"),
            ]).await;
        next_row(&mut rx, "$remote-reply", |row| row["reply"]["status"] == "unavailable").await;
        next_row(&mut rx, "$remote-reply", |row| row["reply"]["status"] == "pending").await;
        sdk_sync(&server, &client, None, vec![
            message("$live", "@other:example.org", json!({"msgtype":"m.text", "body":"live"})),
        ]).await;
        tokio::time::timeout(Duration::from_secs(1),
            next_row(&mut rx, "$live", |row| row["body"] == "live")).await
            .expect("reply fetch blocked the live SDK vector stream");
        let ready = next_row(&mut rx, "$remote-reply", |row| row["reply"]["status"] == "ready").await;
        assert_eq!(ready["reply"]["event_id"], "$remote");
        assert_eq!(ready["reply"]["sender"], "@other:example.org");
        assert_eq!(ready["reply"]["body"], "remote preview");
        let error = snapshot_row(&timelines, event_id!("$gone-reply")).await;
        assert_eq!(error["reply"]["status"], "error");
        assert_eq!(error["reply"]["event_id"], "$gone");
        assert!(error["reply"].get("body").is_none());
        sdk_sync(&server, &client, Some(power_levels(50)), vec![]).await;
        next_row(&mut rx, "$gone-reply", |row| row["can_redact"] == true).await;
        assert_eq!(snapshot_row(&timelines, event_id!("$gone-reply")).await["reply"]["status"], "error");
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_reply_fetches_are_bounded_and_replacement_does_not_inherit_abandoned_pending() {
        use std::sync::atomic::{AtomicUsize, Ordering};
        let server = MockServer::start().await;
        let requests = Arc::new(AtomicUsize::new(0));
        let admitted = Arc::new(tokio::sync::Notify::new());
        let request_count = requests.clone();
        let request_admitted = admitted.clone();
        Mock::given(method("GET")).and(path_regex(r"/event/.*unloaded-[0-9]+$"))
            .respond_with(move |_: &wiremock::Request| {
                if request_count.fetch_add(1, Ordering::SeqCst) + 1 == MAX_REPLY_FETCHES {
                    request_admitted.notify_one();
                }
                ResponseTemplate::new(404)
                    .set_body_json(json!({"errcode":"M_NOT_FOUND", "error":"unavailable"}))
                    .set_delay(Duration::from_secs(2))
            }).mount(&server).await;
        let events = (0..8).map(|index|
            reply_message(&format!("$reply-{index}"), &format!("$unloaded-{index}"))).collect();
        let (server, client, timelines, mut rx) = sdk_fixture_with_server(server,
            Some(power_levels(0)), events).await;
        tokio::time::timeout(Duration::from_secs(1), admitted.notified()).await.unwrap();
        assert_eq!(requests.load(Ordering::SeqCst), MAX_REPLY_FETCHES);
        sdk_sync(&server, &client, None, vec![
            message("$still-live", "@other:example.org", json!({"msgtype":"m.text", "body":"live"})),
        ]).await;
        tokio::time::timeout(Duration::from_secs(1),
            next_row(&mut rx, "$still-live", |_| true)).await.unwrap();
        assert_eq!(requests.load(Ordering::SeqCst), MAX_REPLY_FETCHES);
        let (tx, mut replacement_rx) = mpsc::channel(128);
        timelines.subscribe(&client, Output::new(tx, CancellationToken::new()),
            owned_room_id!("!room:example.org"), "replacement".into()).await.unwrap();
        let reset: Value = serde_json::from_slice(&replacement_rx.recv().await.unwrap()).unwrap();
        assert_eq!(reset["params"]["subscription_id"], "replacement");
        assert!(reset["params"]["items"].as_array().unwrap().iter()
            .filter_map(|row| row.get("reply"))
            .all(|reply| reply["status"] != "pending"));
        assert_eq!(timelines.send(&owned_room_id!("!room:example.org"), "active",
            "stale".into(), vec![], vec![], None).await.unwrap_err().code, "stale_subscription");
        tokio::time::timeout(Duration::from_secs(10), async {
            let mut errors = HashSet::new();
            while errors.len() < 8 {
                let frame: Value = serde_json::from_slice(
                    &replacement_rx.recv().await.expect("replacement output ended")).unwrap();
                let params = &frame["params"];
                let mut rows = Vec::new();
                if let Some(items) = params["items"].as_array() { rows.extend(items); }
                if let Some(changes) = params["changes"].as_array() {
                    for change in changes {
                        if let Some(row) = change.get("item") { rows.push(row); }
                        if let Some(items) = change["items"].as_array() { rows.extend(items); }
                    }
                }
                for row in rows {
                    if row["reply"]["status"] == "error" {
                        errors.insert(row["event_id"].as_str().unwrap().to_owned());
                    }
                }
            }
        }).await.expect("replacement inherited abandoned SDK Pending replies");
        assert!(queued_content(&client).await.is_empty());
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_encrypted_media_upload_survives_source_release_and_owns_ciphertext_metadata() {
        let (server, client, timelines, _rx) = sdk_fixture(Some(power_levels(0)), vec![]).await;
        sdk_sync(&server, &client, None, vec![json!({
            "type":"m.room.encryption", "state_key":"", "event_id":"$encryption",
            "sender":"@me:example.org", "origin_server_ts":1,
            "content":{"algorithm":"m.megolm.v1.aes-sha2"}
        })]).await;
        let room_id = owned_room_id!("!room:example.org");
        let room = client.get_room(&room_id).unwrap();
        assert!(room.encryption_state().is_encrypted());
        let source = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(source.path(), IMAGE_PNG).unwrap();
        timelines.send(&room_id, "active", "encrypted caption".into(), vec![],
            vec![attachment(source.path(), "secret.png", "image/png")], None).await.unwrap();
        source.close().unwrap();
        let queue = room.send_queue();
        Mock::given(method("GET"))
            .and(path_regex(r"^/_matrix/(media/(r0|v3)|client/v1/media)/config$"))
            .respond_with(ResponseTemplate::new(200)
                .set_body_json(json!({"m.upload.size":32 * 1024 * 1024})))
            .mount(&server).await;
        Mock::given(method("POST")).and(path_regex(r"^/_matrix/media/(r0|v3)/upload$"))
            .respond_with(move |request: &wiremock::Request| {
                assert_eq!(request.body.len(), IMAGE_PNG.len());
                assert_ne!(request.body, IMAGE_PNG);
                ResponseTemplate::new(200)
                    .set_body_json(json!({"content_uri":"mxc://example.org/sdk-encrypted"}))
            }).expect(1).mount(&server).await;
        let (_, mut updates) = queue.subscribe().await.unwrap();
        client.send_queue().set_enabled(true).await;
        let source = tokio::time::timeout(Duration::from_secs(10), async {
            loop {
                match updates.recv().await.unwrap() {
                    matrix_sdk::send_queue::RoomSendQueueUpdate::MediaUpload {
                        file: Some(source), ..
                    } => break source,
                    matrix_sdk::send_queue::RoomSendQueueUpdate::SendError { error, .. } =>
                        panic!("SDK attachment upload failed: {error}"),
                    _ => {}
                }
            }
        }).await.unwrap();
        queue.set_enabled(false);
        let matrix_sdk::ruma::events::room::MediaSource::Encrypted(file) = &source
            else { panic!("SDK uploaded plaintext media to an encrypted room") };
        assert_eq!(file.url.as_str(), "mxc://example.org/sdk-encrypted");
        assert_eq!(file.info.version(), "v2");
        let request = matrix_sdk::media::MediaRequestParameters {
            source, format: matrix_sdk::media::MediaFormat::File,
        };
        assert_eq!(client.media().get_media_content(&request, true).await.unwrap(), IMAGE_PNG);
        assert_eq!(queued_content(&client).await.len(), 1);
        timelines.shutdown().await;
    }
}
