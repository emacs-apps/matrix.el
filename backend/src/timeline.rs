use std::{collections::HashMap, sync::Arc};

use eyeball_im::VectorDiff;
use futures_util::StreamExt;
use matrix_sdk::{
    Client, RoomState,
    ruma::{OwnedRoomId, events::room::message::RoomMessageEventContent},
};
use matrix_sdk_ui::timeline::{
    EventSendState, RoomExt, Timeline, TimelineItem, VirtualTimelineItem,
};
use serde_json::{Value, json};
use tokio::{sync::Mutex, task::JoinHandle};
use tokio_util::sync::CancellationToken;

use crate::protocol::{NETWORK, Output, Result, RpcError};

const MAX_SUBSCRIPTIONS: usize = 32;
struct Subscription {
    id: String,
    timeline: Arc<Timeline>,
    cancel: CancellationToken,
    task: JoinHandle<()>,
}

#[derive(Default)]
pub struct Timelines {
    rooms: Mutex<HashMap<OwnedRoomId, Subscription>>,
}

impl Timelines {
    pub async fn subscribe(
        &self,
        client: &Client,
        output: Output,
        room_id: OwnedRoomId,
        id: String,
    ) -> Result<()> {
        let mut rooms = self.rooms.lock().await;
        if rooms.get(&room_id).is_some_and(|s| s.id == id) {
            return Ok(());
        }
        if !rooms.contains_key(&room_id) && rooms.len() >= MAX_SUBSCRIPTIONS {
            return Err(RpcError::new(
                "subscription_limit",
                "At most 32 room timelines may be open",
            ));
        }
        let timeline = if let Some(old) = rooms.remove(&room_id) {
            old.cancel.cancel();
            let _ = old.task.await;
            old.timeline
        } else {
            Arc::new(
                joined_room(client, &room_id)?
                    .timeline()
                    .await
                    .map_err(|_| NETWORK)?,
            )
        };
        let (items, stream) = timeline.subscribe().await;
        if !output
            .event(
                json!({"type":"timeline_reset", "room_id":room_id, "subscription_id":id,
            "revision":0, "items":items.iter().map(|i| item(i)).collect::<Vec<_>>()}),
            )
            .await
        {
            return Err(RpcError::new("transport_closed", "Protocol output closed"));
        }
        let cancel = CancellationToken::new();
        let task_cancel = cancel.clone();
        let task_room = room_id.clone();
        let task_id = id.clone();
        let task = tokio::spawn(async move {
            futures_util::pin_mut!(stream);
            let mut revision = 0u64;
            loop {
                tokio::select! {
                    _ = task_cancel.cancelled() => break,
                    diffs = stream.next() => {
                        let Some(diffs) = diffs else {
                            output.event(json!({"type":"status", "state":"error", "message":"Timeline stream ended; close and reopen the room"})).await;
                            break;
                        };
                        if diffs.is_empty() { continue; }
                        revision += 1;
                        let event = json!({"type":"timeline_diff", "room_id":task_room, "subscription_id":task_id,
                            "revision":revision, "changes":diffs.into_iter().map(change).collect::<Vec<_>>()});
                        tokio::select! {
                            _ = task_cancel.cancelled() => break,
                            sent = output.event(event) => if !sent { break; },
                        }
                    }
                }
            }
        });
        rooms.insert(
            room_id,
            Subscription {
                id,
                timeline,
                cancel,
                task,
            },
        );
        Ok(())
    }

    pub async fn unsubscribe(&self, room_id: &OwnedRoomId, id: &str) {
        let mut rooms = self.rooms.lock().await;
        if rooms.get(room_id).is_some_and(|s| s.id == id) {
            let old = rooms.remove(room_id).expect("checked subscription");
            old.cancel.cancel();
            let _ = old.task.await;
        }
    }

    pub async fn send(&self, client: &Client, room_id: &OwnedRoomId, body: String) -> Result<()> {
        let room = joined_room(client, room_id)?;
        let timeline = self
            .rooms
            .lock()
            .await
            .get(room_id)
            .map(|s| s.timeline.clone());
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

    pub async fn retry_decryption(&self, sessions: &[String]) {
        let timelines: Vec<_> = self
            .rooms
            .lock()
            .await
            .values()
            .map(|s| s.timeline.clone())
            .collect();
        for timeline in timelines {
            timeline
                .retry_decryption(sessions.iter().map(String::as_str))
                .await;
        }
    }

    pub async fn shutdown(&self) {
        let rooms = std::mem::take(&mut *self.rooms.lock().await);
        for subscription in rooms.values() {
            subscription.cancel.cancel();
        }
        for (_, subscription) in rooms {
            let _ = subscription.task.await;
        }
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

fn item(item: &TimelineItem) -> Value {
    let mut value = json!({"id": item.unique_id().0});
    if let Some(event) = item.as_event() {
        value["event_id"] = json!(event.event_id());
        value["transaction_id"] = json!(event.transaction_id());
        value["sender"] = json!(event.sender());
        value["timestamp"] = json!(event.timestamp());
        value["own"] = json!(event.is_own());
        let content = event.content();
        if let Some(message) = content.as_message() {
            value["kind"] = json!("message");
            value["body"] = json!(message.body());
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

fn change(diff: VectorDiff<Arc<TimelineItem>>) -> Value {
    match diff {
        VectorDiff::Append { values } => {
            json!({"op":"append", "items":values.iter().map(|i| item(i)).collect::<Vec<_>>()})
        }
        VectorDiff::Clear => json!({"op":"clear"}),
        VectorDiff::PushFront { value } => json!({"op":"push_front", "item":item(&value)}),
        VectorDiff::PushBack { value } => json!({"op":"push_back", "item":item(&value)}),
        VectorDiff::PopFront => json!({"op":"pop_front"}),
        VectorDiff::PopBack => json!({"op":"pop_back"}),
        VectorDiff::Insert { index, value } => {
            json!({"op":"insert", "index":index, "item":item(&value)})
        }
        VectorDiff::Set { index, value } => json!({"op":"set", "index":index, "item":item(&value)}),
        VectorDiff::Remove { index } => json!({"op":"remove", "index":index}),
        VectorDiff::Truncate { length } => json!({"op":"truncate", "length":length}),
        VectorDiff::Reset { values } => {
            json!({"op":"reset", "items":values.iter().map(|i| item(i)).collect::<Vec<_>>()})
        }
    }
}
