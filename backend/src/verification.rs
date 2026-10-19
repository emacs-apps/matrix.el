use std::{collections::HashMap, sync::Arc, time::Duration};

use futures_util::StreamExt;
use matrix_sdk::{
    Client,
    encryption::verification::{
        SasState, SasVerification, VerificationRequest, VerificationRequestState,
    },
    ruma::{
        DeviceId, OwnedUserId, UserId,
        events::{
            key::verification::{VerificationMethod, request::ToDeviceKeyVerificationRequestEvent},
            room::message::OriginalSyncRoomMessageEvent,
        },
    },
};
use serde_json::{Value, json};
use tokio::sync::Mutex;
use tokio_util::{sync::CancellationToken, task::TaskTracker};

use crate::protocol::{NETWORK, Output, Result, RpcError};

type FlowKey = (OwnedUserId, String);
#[derive(Default)]
struct Flow {
    accepted: bool,
    presented: bool,
}

pub struct Verifications {
    client: Client,
    output: Output,
    flows: Mutex<HashMap<FlowKey, Flow>>,
    cancel: CancellationToken,
    tasks: TaskTracker,
}

impl Verifications {
    pub fn new(client: Client, output: Output, cancel: CancellationToken) -> Arc<Self> {
        let this = Arc::new(Self {
            client,
            output,
            flows: Mutex::new(HashMap::new()),
            cancel,
            tasks: TaskTracker::new(),
        });
        let weak = Arc::downgrade(&this);
        this.client
            .add_event_handler(move |event: ToDeviceKeyVerificationRequestEvent| {
                let weak = weak.clone();
                async move {
                    if let Some(this) = weak.upgrade()
                        && let Some(request) = this
                            .client
                            .encryption()
                            .get_verification_request(
                                &event.sender,
                                event.content.transaction_id.as_str(),
                            )
                            .await
                        {
                            let _ = this.watch(request, false).await;
                        }
                }
            });
        let weak = Arc::downgrade(&this);
        this.client
            .add_event_handler(move |event: OriginalSyncRoomMessageEvent| {
                let weak = weak.clone();
                async move {
                    if let Some(this) = weak.upgrade() {
                        // The SDK registers in-room verification requests under their event ID.
                        if let Some(request) = this
                            .client
                            .encryption()
                            .get_verification_request(&event.sender, event.event_id.as_str())
                            .await
                        {
                            let _ = this.watch(request, false).await;
                        }
                    }
                }
            });
        this
    }

    pub async fn request(self: &Arc<Self>, user: &UserId, device: &DeviceId) -> Result<Value> {
        if self.flows.lock().await.len() >= 32 {
            return Err(limit());
        }
        let device = self
            .client
            .encryption()
            .get_device(user, device)
            .await
            .map_err(|_| NETWORK)?
            .ok_or(RpcError::new(
                "device_missing",
                "Device keys are not available; wait for sync and refresh devices",
            ))?;
        let request = device
            .request_verification_with_methods(vec![VerificationMethod::SasV1])
            .await
            .map_err(|_| NETWORK)?;
        let result = json!({"user_id":request.other_user_id(), "flow_id":request.flow_id()});
        self.watch(request, true).await?;
        Ok(result)
    }

    async fn watch(self: &Arc<Self>, request: VerificationRequest, accepted: bool) -> Result<()> {
        let key = (
            request.other_user_id().to_owned(),
            request.flow_id().to_owned(),
        );
        let mut flows = self.flows.lock().await;
        if flows.contains_key(&key) {
            return Ok(());
        }
        if flows.len() >= 32 {
            return Err(limit());
        }
        flows.insert(
            key.clone(),
            Flow {
                accepted,
                presented: false,
            },
        );
        drop(flows);
        let this = self.clone();
        self.tasks.spawn(async move {
            tokio::select! {
                _ = this.cancel.cancelled() => {},
                _ = tokio::time::sleep(Duration::from_secs(600)) => {
                    let _ = request.cancel().await;
                    this.emit(&key, "cancelled", None).await;
                },
                _ = this.watch_request(&key, &request) => {},
            }
            this.flows.lock().await.remove(&key);
        });
        Ok(())
    }

    async fn watch_request(&self, key: &FlowKey, request: &VerificationRequest) {
        let changes = request.changes();
        futures_util::pin_mut!(changes);
        let mut state = request.state();
        loop {
            match state {
                VerificationRequestState::Created { .. }
                | VerificationRequestState::Requested { .. } => {
                    self.emit(key, "requested", None).await;
                }
                VerificationRequestState::Ready { .. } => {
                    self.emit(key, "ready", None).await;
                    if request.we_started() {
                        match request.start_sas().await {
                            Ok(Some(sas)) => {
                                self.watch_sas(key, sas).await;
                                return;
                            }
                            Ok(None) => {}
                            Err(_) => {
                                self.fail(key).await;
                                return;
                            }
                        }
                    }
                }
                VerificationRequestState::Transitioned { verification } => {
                    if let Some(sas) = verification.sas() {
                        self.watch_sas(key, sas).await;
                    } else {
                        self.fail(key).await;
                    }
                    return;
                }
                VerificationRequestState::Done => {
                    self.emit(key, "done", None).await;
                    return;
                }
                VerificationRequestState::Cancelled(_) => {
                    self.emit(key, "cancelled", None).await;
                    return;
                }
            }
            let Some(next) = changes.next().await else {
                return;
            };
            state = next;
        }
    }

    async fn watch_sas(&self, key: &FlowKey, sas: SasVerification) {
        let changes = sas.changes();
        futures_util::pin_mut!(changes);
        let accepted = self
            .flows
            .lock()
            .await
            .get(key)
            .is_some_and(|flow| flow.accepted);
        if accepted && sas.accept().await.is_err() {
            self.fail(key).await;
            return;
        }
        let mut state = sas.state();
        loop {
            match state {
                SasState::KeysExchanged { .. } => {
                    let Some((a, b, c)) = sas.decimals() else {
                        self.fail(key).await;
                        return;
                    };
                    let mut event = json!({"type":"verification", "user_id":key.0, "flow_id":key.1,
                        "device_id":sas.other_device().device_id(), "state":"sas", "decimals":[a,b,c]});
                    if let Some(emojis) = sas.emoji() {
                        event["emojis"] =
                            json!(emojis.iter().map(|emoji| emoji.symbol).collect::<Vec<_>>());
                    }
                    let mut flows = self.flows.lock().await;
                    if self.output.event(event).await
                        && let Some(flow) = flows.get_mut(key) {
                            flow.presented = true;
                        }
                }
                SasState::Confirmed => {
                    self.emit(key, "confirmed", Some(sas.other_device().device_id()))
                        .await;
                }
                SasState::Done { .. } => {
                    self.emit(key, "done", Some(sas.other_device().device_id()))
                        .await;
                    return;
                }
                SasState::Cancelled(_) => {
                    self.emit(key, "cancelled", Some(sas.other_device().device_id()))
                        .await;
                    return;
                }
                _ => {}
            }
            let Some(next) = changes.next().await else {
                return;
            };
            state = next;
        }
    }

    async fn emit(&self, key: &FlowKey, state: &str, device: Option<&DeviceId>) {
        self.output
            .event(
                json!({"type":"verification", "user_id":key.0, "flow_id":key.1,
            "state":state, "device_id":device}),
            )
            .await;
    }

    async fn fail(&self, key: &FlowKey) {
        let _ = self.cancel(&key.0, &key.1).await;
        self.output
            .event(
                json!({"type":"verification", "user_id":key.0, "flow_id":key.1,
            "state":"cancelled", "message":"Verification could not continue; start a new request"}),
            )
            .await;
    }

    pub async fn accept(&self, user: &UserId, flow_id: &str) -> Result<()> {
        let key = (user.to_owned(), flow_id.to_owned());
        let mut flows = self.flows.lock().await;
        let flow = flows.get_mut(&key).ok_or(missing())?;
        flow.accepted = true;
        drop(flows);
        if let Some(sas) = self
            .client
            .encryption()
            .get_verification(user, flow_id)
            .await
            .and_then(|v| v.sas())
        {
            sas.accept().await.map_err(|_| NETWORK)?;
        } else {
            let request = self
                .client
                .encryption()
                .get_verification_request(user, flow_id)
                .await
                .ok_or(missing())?;
            request
                .accept_with_methods(vec![VerificationMethod::SasV1])
                .await
                .map_err(|_| NETWORK)?;
        }
        Ok(())
    }

    pub async fn confirm(&self, user: &UserId, flow_id: &str) -> Result<()> {
        let key = (user.to_owned(), flow_id.to_owned());
        if !self
            .flows
            .lock()
            .await
            .get(&key)
            .is_some_and(|flow| flow.presented && flow.accepted)
        {
            return Err(RpcError::new(
                "sas_not_presented",
                "Accept verification and compare the displayed SAS before confirming",
            ));
        }
        let sas = self
            .client
            .encryption()
            .get_verification(user, flow_id)
            .await
            .and_then(|v| v.sas())
            .ok_or(missing())?;
        if !sas.can_be_presented() || sas.is_done() || sas.is_cancelled() {
            return Err(missing());
        }
        sas.confirm().await.map_err(|_| NETWORK)
    }

    pub async fn cancel(&self, user: &UserId, flow_id: &str) -> Result<()> {
        if let Some(sas) = self
            .client
            .encryption()
            .get_verification(user, flow_id)
            .await
            .and_then(|v| v.sas())
        {
            sas.cancel().await.map_err(|_| NETWORK)
        } else {
            let request = self
                .client
                .encryption()
                .get_verification_request(user, flow_id)
                .await
                .ok_or(missing())?;
            request.cancel().await.map_err(|_| NETWORK)
        }
    }

    pub async fn shutdown(&self) {
        self.cancel.cancel();
        self.tasks.close();
        self.tasks.wait().await;
    }
}

fn missing() -> RpcError {
    RpcError::new(
        "verification_missing",
        "Verification is unknown, expired or already completed",
    )
}
fn limit() -> RpcError {
    RpcError::new(
        "verification_limit",
        "Too many active verification requests",
    )
}
