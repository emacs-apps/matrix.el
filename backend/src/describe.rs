//! Readable sentences for the state changes a room timeline shows.
//!
//! Only changes people act on are described; configuration noise (power
//! levels, join rules, ACLs, policies) stays undescribed and hidden.  A
//! sentence keeps the people it names as user parts, so clients can show
//! their colours and avatars and open them.

use matrix_sdk::ruma::{OwnedMxcUri, UserId, events::StateEventContentChange};
use matrix_sdk_ui::timeline::{
    AnyOtherStateEventContentChange, EventTimelineItem, MemberProfileChange, MembershipChange,
    RoomMembershipChange, TimelineDetails, TimelineItemContent,
};
use serde_json::{Value, json};

/// A described state change: its event type and its sentence's parts.
pub struct Notice {
    pub state_type: &'static str,
    parts: Vec<Value>,
}

impl Notice {
    /// The sentence as plain text.
    pub fn body(&self) -> String {
        self.parts.iter()
            .filter_map(|part| part.get("text").or_else(|| part.get("name")))
            .filter_map(Value::as_str)
            .collect()
    }

    /// Each part is {"text"} or a user {"user_id", "name", "avatar_url"?}.
    pub fn parts(self) -> Value {
        Value::Array(self.parts)
    }
}

struct Person<'a> {
    id: &'a UserId,
    name: String,
    avatar: Option<OwnedMxcUri>,
}

#[derive(Default)]
struct Sentence(Vec<Value>);

impl Sentence {
    fn user(mut self, person: &Person) -> Self {
        let mut part = json!({"user_id":person.id, "name":person.name});
        if let Some(avatar) = &person.avatar {
            part["avatar_url"] = json!(avatar);
        }
        self.0.push(part);
        self
    }

    fn text(mut self, text: impl AsRef<str>) -> Self {
        self.0.push(json!({"text":text.as_ref()}));
        self
    }

    fn notice(self, state_type: &'static str) -> Notice {
        Notice { state_type, parts: self.0 }
    }
}

/// Describe EVENT's state change, if it is one people act on.
pub fn state(event: &EventTimelineItem) -> Option<Notice> {
    let profile = match event.sender_profile() {
        TimelineDetails::Ready(profile) => Some(profile),
        _ => None,
    };
    let actor = Person {
        id: event.sender(),
        name: profile.and_then(|profile| profile.display_name.clone())
            .unwrap_or_else(|| event.sender().to_string()),
        avatar: profile.and_then(|profile| profile.avatar_url.clone()),
    };
    match event.content() {
        TimelineItemContent::MembershipChange(change) => membership(&actor, change),
        TimelineItemContent::ProfileChange(change) => profile_change(&actor, change),
        TimelineItemContent::OtherState(state) => other(&actor, state.content()),
        _ => None,
    }
}

fn membership(actor: &Person, change: &RoomMembershipChange) -> Option<Notice> {
    let target = Person {
        id: change.user_id(),
        name: change.display_name().unwrap_or_else(|| change.user_id().to_string()),
        avatar: change.avatar_url(),
    };
    let sentence = Sentence::default();
    let sentence = match change.change()? {
        MembershipChange::Joined | MembershipChange::InvitationAccepted => {
            sentence.user(&target).text(" joined")
        }
        MembershipChange::Left => sentence.user(&target).text(" left"),
        MembershipChange::Invited => sentence.user(actor).text(" invited ").user(&target),
        MembershipChange::InvitationRejected => {
            sentence.user(&target).text(" declined the invitation")
        }
        MembershipChange::InvitationRevoked => {
            sentence.user(actor).text(" withdrew ").user(&target).text("'s invitation")
        }
        MembershipChange::Kicked => sentence.user(actor).text(" removed ").user(&target),
        MembershipChange::Banned => sentence.user(actor).text(" banned ").user(&target),
        MembershipChange::KickedAndBanned => {
            sentence.user(actor).text(" removed and banned ").user(&target)
        }
        MembershipChange::Unbanned => sentence.user(actor).text(" unbanned ").user(&target),
        MembershipChange::Knocked => sentence.user(&target).text(" asked to join"),
        MembershipChange::KnockAccepted => sentence.user(actor).text(" let ").user(&target).text(" in"),
        MembershipChange::KnockRetracted => {
            sentence.user(&target).text(" withdrew their request to join")
        }
        MembershipChange::KnockDenied => {
            sentence.user(actor).text(" declined ").user(&target).text("'s request to join")
        }
        MembershipChange::None | MembershipChange::Error | MembershipChange::NotImplemented => {
            return None;
        }
    };
    let reason = match change.content() {
        StateEventContentChange::Original { content, .. } => content.reason.as_deref(),
        StateEventContentChange::Redacted(_) => None,
    };
    let sentence = match reason.map(str::trim).filter(|reason| !reason.is_empty()) {
        Some(reason) => sentence.text(format!(": {reason}")),
        None => sentence,
    };
    Some(sentence.notice("m.room.member"))
}

fn profile_change(actor: &Person, change: &MemberProfileChange) -> Option<Notice> {
    let sentence = if let Some(name) = change.displayname_change() {
        // Name them as they were known before the change.
        let before = Person {
            id: change.user_id(),
            name: name.old.clone().unwrap_or_else(|| change.user_id().to_string()),
            avatar: actor.avatar.clone(),
        };
        let sentence = Sentence::default().user(&before);
        match (&name.old, &name.new) {
            (Some(_), Some(new)) => sentence.text(format!(" changed their name to {new}")),
            (None, Some(new)) => sentence.text(format!(" set their name to {new}")),
            (_, None) => sentence.text(" removed their name"),
        }
    } else {
        change.avatar_url_change()?;
        Sentence::default().user(actor).text(" changed their avatar")
    };
    Some(sentence.notice("m.room.member"))
}

fn other(actor: &Person, content: &AnyOtherStateEventContentChange) -> Option<Notice> {
    use AnyOtherStateEventContentChange as State;
    use StateEventContentChange::Original;
    let by = || Sentence::default().user(actor);
    Some(match content {
        State::RoomCreate(_) => by().text(" created the room").notice("m.room.create"),
        State::RoomName(Original { content, .. }) => if content.name.is_empty() {
            by().text(" removed the room name")
        } else {
            by().text(format!(" renamed the room to {}", content.name))
        }.notice("m.room.name"),
        State::RoomTopic(Original { content, .. }) => if content.topic.is_empty() {
            by().text(" removed the topic")
        } else {
            by().text(format!(" changed the topic: {}", content.topic))
        }.notice("m.room.topic"),
        State::RoomAvatar(Original { .. }) => {
            by().text(" changed the room avatar").notice("m.room.avatar")
        }
        State::RoomEncryption(Original { .. }) => {
            by().text(" turned on end-to-end encryption").notice("m.room.encryption")
        }
        State::RoomCanonicalAlias(Original { content, .. }) => match &content.alias {
            Some(alias) => by().text(format!(" set the room address to {alias}")),
            None => by().text(" removed the room address"),
        }.notice("m.room.canonical_alias"),
        State::RoomPinnedEvents(Original { .. }) => {
            by().text(" changed the pinned messages").notice("m.room.pinned_events")
        }
        State::RoomTombstone(Original { content, .. }) => {
            let body = content.body.trim();
            Sentence::default().text(if body.is_empty() {
                "This room has been replaced".to_owned()
            } else {
                format!("This room has been replaced: {body}")
            }).notice("m.room.tombstone")
        }
        _ => return None,
    })
}
