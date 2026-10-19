//! Readable sentences for the state changes a room timeline shows.
//!
//! Only changes people act on are described; configuration noise (power
//! levels, join rules, ACLs, policies) stays undescribed and hidden.

use matrix_sdk::ruma::events::StateEventContentChange;
use matrix_sdk_ui::timeline::{
    AnyOtherStateEventContentChange, EventTimelineItem, MemberProfileChange, MembershipChange,
    RoomMembershipChange, TimelineDetails, TimelineItemContent,
};

/// Return the state event type and a sentence describing EVENT's change.
pub fn state(event: &EventTimelineItem) -> Option<(&'static str, String)> {
    let actor = match event.sender_profile() {
        TimelineDetails::Ready(profile) => profile.display_name.clone(),
        _ => None,
    }
    .unwrap_or_else(|| event.sender().to_string());
    match event.content() {
        TimelineItemContent::MembershipChange(change) => {
            Some(("m.room.member", membership(&actor, change)?))
        }
        TimelineItemContent::ProfileChange(change) => Some(("m.room.member", profile(change)?)),
        TimelineItemContent::OtherState(state) => other(&actor, state.content()),
        _ => None,
    }
}

fn membership(actor: &str, change: &RoomMembershipChange) -> Option<String> {
    let target = change.display_name().unwrap_or_else(|| change.user_id().to_string());
    let text = match change.change()? {
        MembershipChange::Joined | MembershipChange::InvitationAccepted => {
            format!("{target} joined")
        }
        MembershipChange::Left => format!("{target} left"),
        MembershipChange::Invited => format!("{actor} invited {target}"),
        MembershipChange::InvitationRejected => format!("{target} declined the invitation"),
        MembershipChange::InvitationRevoked => {
            format!("{actor} withdrew {target}'s invitation")
        }
        MembershipChange::Kicked => format!("{actor} removed {target}"),
        MembershipChange::Banned => format!("{actor} banned {target}"),
        MembershipChange::KickedAndBanned => format!("{actor} removed and banned {target}"),
        MembershipChange::Unbanned => format!("{actor} unbanned {target}"),
        MembershipChange::Knocked => format!("{target} asked to join"),
        MembershipChange::KnockAccepted => format!("{actor} let {target} in"),
        MembershipChange::KnockRetracted => format!("{target} withdrew their request to join"),
        MembershipChange::KnockDenied => format!("{actor} declined {target}'s request to join"),
        MembershipChange::None | MembershipChange::Error | MembershipChange::NotImplemented => {
            return None;
        }
    };
    let reason = match change.content() {
        StateEventContentChange::Original { content, .. } => content.reason.as_deref(),
        StateEventContentChange::Redacted(_) => None,
    };
    Some(match reason.map(str::trim).filter(|reason| !reason.is_empty()) {
        Some(reason) => format!("{text}: {reason}"),
        None => text,
    })
}

fn profile(change: &MemberProfileChange) -> Option<String> {
    let user = change.user_id().to_string();
    if let Some(name) = change.displayname_change() {
        let old = name.old.as_deref().unwrap_or(&user);
        return Some(match name.new.as_deref() {
            Some(new) if name.old.is_some() => format!("{old} changed their name to {new}"),
            Some(new) => format!("{old} set their name to {new}"),
            None => format!("{old} removed their name"),
        });
    }
    change.avatar_url_change().map(|_| format!("{user} changed their avatar"))
}

fn other(actor: &str, content: &AnyOtherStateEventContentChange) -> Option<(&'static str, String)> {
    use AnyOtherStateEventContentChange as State;
    use StateEventContentChange::Original;
    Some(match content {
        State::RoomCreate(_) => ("m.room.create", format!("{actor} created the room")),
        State::RoomName(Original { content, .. }) => ("m.room.name", if content.name.is_empty() {
            format!("{actor} removed the room name")
        } else {
            format!("{actor} renamed the room to {}", content.name)
        }),
        State::RoomTopic(Original { content, .. }) => ("m.room.topic", if content.topic.is_empty() {
            format!("{actor} removed the topic")
        } else {
            format!("{actor} changed the topic: {}", content.topic)
        }),
        State::RoomAvatar(Original { .. }) => {
            ("m.room.avatar", format!("{actor} changed the room avatar"))
        }
        State::RoomEncryption(Original { .. }) => {
            ("m.room.encryption", format!("{actor} turned on end-to-end encryption"))
        }
        State::RoomCanonicalAlias(Original { content, .. }) => (
            "m.room.canonical_alias",
            match &content.alias {
                Some(alias) => format!("{actor} set the room address to {alias}"),
                None => format!("{actor} removed the room address"),
            },
        ),
        State::RoomPinnedEvents(Original { .. }) => {
            ("m.room.pinned_events", format!("{actor} changed the pinned messages"))
        }
        State::RoomTombstone(Original { content, .. }) => ("m.room.tombstone", {
            let body = content.body.trim();
            if body.is_empty() {
                "This room has been replaced".to_owned()
            } else {
                format!("This room has been replaced: {body}")
            }
        }),
        _ => return None,
    })
}
