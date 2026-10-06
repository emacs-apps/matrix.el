use std::{collections::HashMap, sync::Arc};

use eyeball_im::VectorDiff;
use futures_util::StreamExt;
use matrix_sdk::{
    Client, RoomState,
    room::edit::{EditError as RoomEditError, EditedContent},
    ruma::{
        EventId, OwnedEventId, OwnedRoomId, UserId,
        api::error::ErrorKind,
        events::{
            MessageLikeEventType,
            room::{
                message::{MessageType, RoomMessageEventContent, RoomMessageEventContentWithoutRelation},
                power_levels::RoomPowerLevels,
            },
        },
    },
};
use matrix_sdk_ui::timeline::{
    EditError, Error as TimelineError, EventSendState, EventTimelineItem, Message, RedactError,
    RoomExt, Timeline, TimelineDetails, TimelineItem, VirtualTimelineItem,
};
use serde_json::{Value, json};
use tokio::sync::{Mutex, OnceCell};
use tokio_util::{
    sync::CancellationToken,
    task::{TaskTracker, task_tracker::TaskTrackerToken},
};

use crate::protocol::{INVALID, NETWORK, Output, Result, RpcError};
use crate::{media, storage::valid_source_id};

const MAX_SUBSCRIPTIONS: usize = 32;
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

#[derive(Clone, Copy, Default, PartialEq, Eq)]
struct MutationPermissions {
    send_message: bool,
    redact_own: bool,
    redact_other: bool,
}

impl MutationPermissions {
    fn from_power_levels(levels: &RoomPowerLevels, user: &UserId) -> Self {
        Self {
            send_message: levels.user_can_send_message(user, MessageLikeEventType::RoomMessage),
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
struct Subscription {
    id: String,
    timeline: OnceCell<Result<Arc<Timeline>>>,
    cancel: CancellationToken,
    tasks: TaskTracker,
    // Admit mutations and subscription transitions in a single per-room order.
    // Closing/replacing a view waits for already-admitted SDK queue operations,
    // without holding the room map or blocking unrelated rooms.
    mutations: Mutex<()>,
}

impl Subscription {
    fn cancel(&self) {
        self.cancel.cancel();
        self.tasks.close();
    }
}

pub struct ImageLease {
    subscription: Arc<Subscription>,
    room_id: OwnedRoomId,
    event_id: OwnedEventId,
    pub source_id: String,
    pub request: matrix_sdk::media::MediaRequestParameters,
    _operation: TaskTrackerToken,
    _subscription_operation: TaskTrackerToken,
}

impl ImageLease {
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
        let image = event_image(event).ok_or(media::STALE_IMAGE)?;
        if media::source_id(&image.source) != self.source_id {
            return Err(media::STALE_IMAGE);
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
            if let Some(current) = &current {
                if current.id == id && !current.cancel.is_cancelled() {
                    return Ok((current.clone(), None, current.tasks.token()));
                }
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
                old.timeline
                    .get()
                    .and_then(|result| result.as_ref().ok())
                    .cloned()
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
                    let mut permissions = mutation_permissions(&task_sdk_room).await.unwrap_or_default();
                    let (mut items, stream) = timeline.subscribe().await;
                    if !output
                        .event(json!({
                            "type":"timeline_reset", "room_id":room_id,
                            "subscription_id":subscription.id, "revision":0,
                            "items":items.iter().map(|i| item(i, permissions)).collect::<Vec<_>>()
                        }))
                        .await
                    {
                        return Err(RpcError::new("transport_closed", "Protocol output closed"));
                    }
                    let task_cancel = subscription.cancel.clone();
                    let task_room = room_id.clone();
                    let task_id = subscription.id.clone();
                    self.tasks.spawn(subscription.tasks.track_future(async move {
                        futures_util::pin_mut!(stream);
                        let mut revision = 0u64;
                        let mut room_updates_open = true;
                        loop {
                            tokio::select! {
                                biased;
                                _ = task_cancel.cancelled() => break,
                                update = room_updates.recv(), if room_updates_open => {
                                    if matches!(update, Err(tokio::sync::broadcast::error::RecvError::Closed)) {
                                        room_updates_open = false;
                                        continue;
                                    }
                                    let next = mutation_permissions(&task_sdk_room).await.unwrap_or_default();
                                    if next == permissions { continue; }
                                    permissions = next;
                                    revision += 1;
                                    // Reserialize our subscriber's SDK snapshot, not a separately
                                    // fetched list that could overtake queued SDK vector diffs.
                                    let event = json!({
                                        "type":"timeline_diff", "room_id":task_room,
                                        "subscription_id":task_id, "revision":revision,
                                        "changes":[{"op":"reset", "items":items.iter()
                                            .map(|i| item(i, permissions)).collect::<Vec<_>>()}]
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
                                    let event = json!({
                                        "type":"timeline_diff", "room_id":task_room,
                                        "subscription_id":task_id, "revision":revision,
                                        "changes":diffs.iter().map(|diff| change(diff, permissions)).collect::<Vec<_>>()
                                    });
                                    for diff in diffs {
                                        diff.apply(&mut items);
                                    }
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

    pub async fn send(&self, client: &Client, room_id: &OwnedRoomId, body: String) -> Result<()> {
        let room = joined_room(client, room_id)?;
        let timeline = self.rooms.lock().await.get(room_id).and_then(|s| {
            s.timeline
                .get()
                .and_then(|result| result.as_ref().ok())
                .cloned()
        });
        let timeline = match timeline {
            Some(timeline) => timeline,
            None => Arc::new(room.timeline().await.map_err(|_| NETWORK)?),
        };
        // The SDK persists the queue and owns retries/transaction IDs even when this view is gone.
        timeline
            .send(RoomMessageEventContent::text_plain(body).into())
            .await
            .map_err(|_| NETWORK)?;
        Ok(())
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

    pub async fn image_lease(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
        event_id: &EventId,
        source_id: String,
    ) -> Result<ImageLease> {
        if !valid_source_id(&source_id) {
            return Err(INVALID);
        }
        let operation = self.tasks.token();
        let (subscription, subscription_operation) = self.mutation_subscription(room_id, id).await?;
        let request = {
            let timeline = mutation_timeline(&subscription)?;
            let items = timeline.items().await;
            let event = items.iter().filter_map(|item| item.as_event())
                .find(|event| event.event_id() == Some(event_id)).ok_or(EVENT_MISSING)?;
            let image = event_image(event).ok_or(media::STALE_IMAGE)?;
            if media::source_id(&image.source) != source_id {
                return Err(media::STALE_IMAGE);
            }
            matrix_sdk::media::MediaRequestParameters {
                source: image.source.clone(),
                format: matrix_sdk::media::MediaFormat::File,
            }
        };
        let lease = ImageLease {
            subscription,
            room_id: room_id.clone(),
            event_id: event_id.to_owned(),
            source_id,
            request,
            _operation: operation,
            _subscription_operation: subscription_operation,
        };
        lease.validate(self).await?;
        Ok(lease)
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
        let _operation = self.tasks.token();
        let (subscription, _subscription_operation) = self.mutation_subscription(room_id, id).await?;
        let _mutation = subscription.mutations.lock().await;
        if !self.is_current(room_id, &subscription).await {
            return Err(STALE_SUBSCRIPTION);
        }
        let timeline = mutation_timeline(&subscription)?;
        let operation = async {
            let items = timeline.items().await;
            let event = items.iter().filter_map(|item| item.as_event())
                .find(|event| event.event_id() == Some(event_id)).ok_or(EVENT_MISSING)?;
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
        };
        tokio::select! {
            biased;
            _ = subscription.cancel.cancelled() => Err(STALE_SUBSCRIPTION),
            result = operation => result,
        }
    }

    pub async fn redact(
        &self,
        room_id: &OwnedRoomId,
        id: &str,
        event_id: &EventId,
    ) -> Result<()> {
        let _operation = self.tasks.token();
        let (subscription, _subscription_operation) = self.mutation_subscription(room_id, id).await?;
        let _mutation = subscription.mutations.lock().await;
        if !self.is_current(room_id, &subscription).await {
            return Err(STALE_SUBSCRIPTION);
        }
        let timeline = mutation_timeline(&subscription)?;
        let operation = async {
            let items = timeline.items().await;
            let event = items.iter().filter_map(|item| item.as_event())
                .find(|event| event.event_id() == Some(event_id)).ok_or(EVENT_MISSING)?;
            if is_redacted(event) || event.redaction_send_state().is_some() {
                return Err(ALREADY_REDACTED);
            }
            if !mutation_permissions(timeline.room()).await?.can_redact(event) {
                return Err(REDACT_FORBIDDEN);
            }
            timeline.redact(&event.identifier(), None).await.map_err(redact_error)
        };
        tokio::select! {
            biased;
            _ = subscription.cancel.cancelled() => Err(STALE_SUBSCRIPTION),
            result = operation => result,
        }
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
fn item(item: &TimelineItem, permissions: MutationPermissions) -> Value {
    let mut value = json!({"id": item.unique_id().0, "can_edit":false, "can_redact":false});
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
        let content = event.content();
        if let Some(message) = content.as_message() {
            value["kind"] = json!("message");
            value["body"] = json!(message.body());
            if let Some(image) = event_image(event) {
                value["image"] = image_metadata(image);
            }
        } else if is_redacted(event) {
            value["kind"] = json!("redacted");
            value["body"] = json!("Message deleted");
        } else if content.is_unable_to_decrypt() {
            value["kind"] = json!("encrypted");
            value["body"] =
                json!("Encrypted message — keys unavailable or sender trust requirements not met");
        } else {
            value["kind"] = json!("state");
            value["body"] = json!(
                content
                    .event_type_str()
                    .unwrap_or_else(|| "Redacted event".into())
            );
        }
        match event.send_state() {
            Some(EventSendState::NotSentYet { .. }) => value["send_state"] = json!("pending"),
            Some(EventSendState::SendingFailed { .. }) => {
                value["send_state"] = json!("failed");
                value["error"] = json!(
                    "SDK could not send this message; check connection and device verification"
                );
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

fn change(diff: &VectorDiff<Arc<TimelineItem>>, permissions: MutationPermissions) -> Value {
    match diff {
        VectorDiff::Append { values } => {
            json!({"op":"append", "items":values.iter().map(|i| item(i, permissions)).collect::<Vec<_>>()})
        }
        VectorDiff::Clear => json!({"op":"clear"}),
        VectorDiff::PushFront { value } => json!({"op":"push_front", "item":item(value, permissions)}),
        VectorDiff::PushBack { value } => json!({"op":"push_back", "item":item(value, permissions)}),
        VectorDiff::PopFront => json!({"op":"pop_front"}),
        VectorDiff::PopBack => json!({"op":"pop_back"}),
        VectorDiff::Insert { index, value } => {
            json!({"op":"insert", "index":index, "item":item(value, permissions)})
        }
        VectorDiff::Set { index, value } => json!({"op":"set", "index":index, "item":item(value, permissions)}),
        VectorDiff::Remove { index } => json!({"op":"remove", "index":index}),
        VectorDiff::Truncate { length } => json!({"op":"truncate", "length":length}),
        VectorDiff::Reset { values } => {
            json!({"op":"reset", "items":values.iter().map(|i| item(i, permissions)).collect::<Vec<_>>()})
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
                "ephemeral":{"events":[]}, "account_data":{"events":[]},
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
        let server = MockServer::start().await;
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
        let value = item(row, permissions);
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
                    let flags = item(local, mutation_permissions(timeline.room()).await.unwrap());
                    assert_eq!(flags["can_edit"], false);
                    assert_eq!(flags["can_redact"], false);
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
            let lease = timelines.image_lease(&room, "active", &event_id, source.into()).await.unwrap();
            let first = media::image(store.clone(), client.clone(), generation.clone(),
                &timelines, &lease, CancellationToken::new(), CancellationToken::new(), &slots).await.unwrap();
            assert_eq!(std::fs::read(&first).unwrap(), IMAGE_PNG);
            assert!(first.starts_with(directory.path().join("media").join(&generation)));
            let cached = media::image(store.clone(), client.clone(), generation.clone(),
                &timelines, &lease, CancellationToken::new(), CancellationToken::new(), &slots).await.unwrap();
            assert_eq!(first, cached);
        }
        let bad = snapshot_row(&timelines, event_id!("$bad-image")).await;
        let good = snapshot_row(&timelines, event_id!("$encrypted-image")).await;
        assert_ne!(bad["image"]["source_id"], good["image"]["source_id"]);
        let source = bad["image"]["source_id"].as_str().unwrap();
        let lease = timelines.image_lease(&room, "active", event_id!("$bad-image"), source.into()).await.unwrap();
        assert!(media::image(store.clone(), client, generation.clone(), &timelines, &lease,
            CancellationToken::new(), CancellationToken::new(), &slots).await.is_err());
        assert_eq!(store.cached_image(&generation, source).unwrap(), None);
        drop(lease);
        timelines.shutdown().await;
    }

    #[tokio::test]
    async fn sdk_image_leases_reject_source_replacement_redaction_and_reopened_subscription() {
        let (server, client, timelines, mut rx) = sdk_fixture(Some(power_levels(0)), vec![
            message("$image", "@other:example.org", json!({
                "msgtype":"m.image", "body":"image.png", "url":"mxc://example.org/image"
            })),
        ]).await;
        let room = owned_room_id!("!room:example.org");
        let row = snapshot_row(&timelines, event_id!("$image")).await;
        let source = row["image"]["source_id"].as_str().unwrap().to_owned();
        assert_eq!(timelines.image_lease(&room, "old", event_id!("$image"), source.clone())
            .await.err().unwrap().code, "stale_subscription");
        assert_eq!(timelines.image_lease(&room, "active", event_id!("$image"), "0".repeat(64))
            .await.err().unwrap().code, "stale_image");
        let lease = timelines.image_lease(&room, "active", event_id!("$image"), source).await.unwrap();
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
        assert_eq!(lease.validate(&timelines).await.unwrap_err().code, "stale_image");
        let source = replacement["image"]["source_id"].as_str().unwrap().to_owned();
        let replacement_lease = timelines.image_lease(&room, "active", event_id!("$image"), source).await.unwrap();
        sdk_sync(&server, &client, None, vec![json!({
            "type":"m.room.redaction", "event_id":"$redaction", "sender":"@other:example.org",
            "origin_server_ts":2000, "redacts":"$image", "content":{"redacts":"$image"}
        })]).await;
        let redacted = next_row(&mut rx, "$image", |row| row["kind"] == "redacted").await;
        assert!(redacted.get("image").is_none());
        assert_eq!(replacement_lease.validate(&timelines).await.unwrap_err().code, "stale_image");
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
            let lease = timelines.image_lease(&owned_room_id!("!room:example.org"), "active",
                event_id!("$image"), source.into()).await.unwrap();
            let slots = tokio::sync::Semaphore::new(1);
            assert_eq!(media::image(store.clone(), client, generation.clone(), &timelines, &lease,
                cancel, transport, &slots).await.unwrap_err().code, "image_cancelled");
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
}
