//! Desktop-notification candidates from the SDK's push-rule evaluation.
//!
//! The SDK evaluates the account's push rules (after decryption) for every
//! synced event and reports those that should notify.  One sync becomes at
//! most one bounded batch, coalesced per room, so a backlog after an outage
//! cannot flood the transport.

use std::collections::BTreeMap;

use matrix_sdk::{
    Client, Room,
    deserialized_responses::RawAnySyncOrStrippedTimelineEvent,
    ruma::{
        OwnedRoomId, OwnedUserId,
        events::{
            AnyStrippedStateEvent, AnySyncMessageLikeEvent, AnySyncTimelineEvent,
            SyncMessageLikeEvent,
            room::{
                member::MembershipState,
                message::{MessageType, Relation, RoomMessageEventContent},
            },
        },
    },
    sync::Notification,
};
use serde_json::{Value, json};

use crate::account::{room_title, summary_text};

const ROOMS_LIMIT: usize = 32;

/// Return one `notifications` event for a sync's push notifications, if any.
pub async fn batch(
    client: &Client,
    notifications: &BTreeMap<OwnedRoomId, Vec<Notification>>,
) -> Option<Value> {
    let mut rooms = Vec::new();
    for (room_id, list) in notifications {
        let (Some(last), Some(room)) = (list.last(), client.get_room(room_id)) else {
            continue;
        };
        let mut entry = describe(&room, &last.event).await;
        entry["room_id"] = json!(room_id);
        entry["room_name"] = json!(summary_text(&room_title(&room)));
        entry["count"] = json!(list.len());
        entry["highlight"] = json!(
            list.iter()
                .any(|notification| notification.actions.iter().any(|action| action.is_highlight()))
        );
        rooms.push(entry);
    }
    finish(rooms)
}

/// Keep mentions first, then the most recent rooms, within the batch bound.
fn finish(mut rooms: Vec<Value>) -> Option<Value> {
    if rooms.is_empty() {
        return None;
    }
    rooms.sort_by(|a, b| {
        b["highlight"]
            .as_bool()
            .cmp(&a["highlight"].as_bool())
            .then_with(|| b["timestamp"].as_u64().cmp(&a["timestamp"].as_u64()))
    });
    let omitted = rooms.len().saturating_sub(ROOMS_LIMIT);
    rooms.truncate(ROOMS_LIMIT);
    Some(json!({"type":"notifications", "rooms":rooms, "omitted":omitted}))
}

async fn describe(room: &Room, event: &RawAnySyncOrStrippedTimelineEvent) -> Value {
    let mut value = json!({"kind":"other"});
    let sender: Option<OwnedUserId> = match event {
        RawAnySyncOrStrippedTimelineEvent::Sync(raw) => raw.deserialize().ok().map(|event| {
            value["event_id"] = json!(event.event_id());
            value["timestamp"] = json!(event.origin_server_ts());
            match &event {
                AnySyncTimelineEvent::MessageLike(AnySyncMessageLikeEvent::RoomMessage(
                    SyncMessageLikeEvent::Original(message),
                )) => {
                    value["kind"] = json!("message");
                    value["body"] = json!(message_body(&message.content));
                }
                AnySyncTimelineEvent::MessageLike(AnySyncMessageLikeEvent::RoomEncrypted(_)) => {
                    value["kind"] = json!("encrypted");
                }
                AnySyncTimelineEvent::MessageLike(AnySyncMessageLikeEvent::Sticker(
                    SyncMessageLikeEvent::Original(sticker),
                )) => {
                    value["kind"] = json!("message");
                    value["body"] = json!(format!("Sticker: {}", summary_text(&sticker.content.body)));
                }
                _ => value["body"] = json!(event.event_type().to_string()),
            }
            event.sender().to_owned()
        }),
        RawAnySyncOrStrippedTimelineEvent::Stripped(raw) => raw.deserialize().ok().map(|event| {
            if let AnyStrippedStateEvent::RoomMember(member) = &event
                && member.content.membership == MembershipState::Invite
            {
                value["kind"] = json!("invite");
            }
            event.sender().to_owned()
        }),
    };
    if let Some(sender) = sender {
        if let Ok(Some(member)) = room.get_member_no_sync(&sender).await
            && let Some(name) = member.display_name()
        {
            value["sender_name"] = json!(summary_text(name));
        }
        value["sender"] = json!(sender);
    }
    value
}

fn message_body(content: &RoomMessageEventContent) -> String {
    let msgtype = match &content.relates_to {
        Some(Relation::Replacement(replacement)) => &replacement.new_content.msgtype,
        _ => &content.msgtype,
    };
    match msgtype {
        MessageType::Image(_) => "Image".to_owned(),
        MessageType::Video(_) => "Video".to_owned(),
        MessageType::Audio(_) => "Audio".to_owned(),
        MessageType::File(file) => format!("File: {}", summary_text(file.filename())),
        MessageType::Emote(emote) => format!("* {}", summary_text(&emote.body)),
        other => summary_text(other.body()).to_owned(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use matrix_sdk::ruma::events::room::message::ReplacementMetadata;

    #[test]
    fn batch_orders_mentions_then_recency_and_reports_omissions() {
        assert!(finish(Vec::new()).is_none());
        let mut rooms: Vec<Value> = (0..ROOMS_LIMIT as u64 + 2)
            .map(|n| json!({"room_id":format!("!r{n}:x"), "highlight":false, "timestamp":n}))
            .collect();
        rooms.push(json!({"room_id":"!old-mention:x", "highlight":true, "timestamp":0}));
        let batch = finish(rooms).unwrap();
        let rooms = batch["rooms"].as_array().unwrap();
        assert_eq!(rooms.len(), ROOMS_LIMIT);
        assert_eq!(batch["omitted"], 3);
        assert_eq!(rooms[0]["room_id"], "!old-mention:x");
        assert_eq!(rooms[1]["timestamp"], ROOMS_LIMIT as u64 + 1);
    }

    #[test]
    fn message_bodies_follow_edits_and_label_media() {
        let original = RoomMessageEventContent::text_plain("before");
        let edit = RoomMessageEventContent::text_plain("after").make_replacement(
            ReplacementMetadata::new("$original".try_into().unwrap(), None),
        );
        assert_eq!(message_body(&original), "before");
        assert_eq!(message_body(&edit), "after");
        assert_eq!(message_body(&RoomMessageEventContent::emote_plain("waves")), "* waves");
    }
}
