#![recursion_limit = "256"]

mod account;
mod media;
mod protocol;
mod storage;
mod timeline;
mod verification;

use std::{path::PathBuf, sync::Arc, time::Duration};

use serde_json::Value;
use tokio::{
    io::BufReader,
    sync::{Semaphore, mpsc},
    task::JoinSet,
};
use tokio_util::sync::CancellationToken;
use zeroize::Zeroizing;

use account::Backend;
use protocol::{Output, Request, RpcError};
use storage::Store;

fn main() {
    // Establish permissions before the runtime or SDK can create any files.
    unsafe {
        libc::umask(0o077);
    }
    // Neither dependency errors nor panic payloads may disclose secrets.
    std::panic::set_hook(Box::new(|_| eprintln!("Matrix backend internal failure")));
    let mut args = std::env::args_os().skip(1);
    let path = match (args.next(), args.next(), args.next()) {
        (Some(flag), Some(path), None) if flag == "--store" => PathBuf::from(path),
        _ => {
            eprintln!("Usage: matrix-backend --store ABSOLUTE_DIRECTORY");
            std::process::exit(2);
        }
    };
    let store = match Store::open(path) {
        Ok(store) => store,
        Err(error) => {
            eprintln!("{}: {}", error.code, error.message);
            std::process::exit(1);
        }
    };
    let runtime = match tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
    {
        Ok(runtime) => runtime,
        Err(_) => {
            eprintln!("Unable to start Matrix runtime");
            std::process::exit(1);
        }
    };
    runtime.block_on(run(store));
    runtime.shutdown_timeout(Duration::from_secs(5));
}

async fn run(store: Store) {
    let closed = CancellationToken::new();
    // Frames are bounded independently of concurrency. Slow consumers exert backpressure.
    let (tx, rx) = mpsc::channel(16);
    let output = Output::new(tx, closed.clone());
    let writer = tokio::spawn(protocol::write_frames(
        tokio::io::stdout(),
        rx,
        closed.clone(),
    ));
    let backend = Backend::new(store, output.clone(), closed.clone());
    let semaphore = Arc::new(Semaphore::new(16));
    // Media can fill the Resource broker without rejecting control requests.
    // Network concurrency is bounded separately at the SDK acquisition boundary.
    let media_requests = Arc::new(Semaphore::new(64));
    let mut requests = JoinSet::new();
    let (frames_tx, mut frames) = mpsc::channel(1);
    // Framing owns its input task: read_exact/read_until must never be cancelled
    // merely because an unrelated network request completed.
    let reader = tokio::spawn(async move {
        let mut input = BufReader::new(tokio::io::stdin());
        loop {
            match protocol::read_frame(&mut input).await {
                Ok(Some(bytes)) => {
                    if frames_tx.send(Ok(Zeroizing::new(bytes))).await.is_err() {
                        break;
                    }
                }
                Ok(None) => break,
                Err(error) => {
                    let _ = frames_tx.send(Err(error)).await;
                    break;
                }
            }
        }
    });
    loop {
        let frame = tokio::select! {
            _ = closed.cancelled() => break,
            signal = tokio::signal::ctrl_c() => { let _ = signal; break; },
            result = requests.join_next(), if !requests.is_empty() => {
                if result.is_some_and(|result| result.is_err()) {
                    output.event(serde_json::json!({"type":"status", "state":"error", "message":"A backend request failed internally"})).await;
                }
                continue;
            }
            result = frames.recv() => result,
        };
        let bytes = match frame {
            Some(Ok(bytes)) => bytes,
            None => break,
            Some(Err(_)) => {
                output
                    .response(
                        Value::Null,
                        Err(RpcError::new(
                            "invalid_request",
                            "Invalid or oversized protocol frame",
                        )),
                    )
                    .await;
                break;
            }
        };
        let request = match Request::parse(&bytes) {
            Ok(request) => request,
            Err(error) => {
                output.response(Value::Null, Err(error)).await;
                continue;
            }
        };
        let admission = if request.method == "media.avatar" {
            &media_requests
        } else {
            &semaphore
        };
        let permit = match admission.clone().try_acquire_owned() {
            Ok(permit) => permit,
            Err(_) => {
                output
                    .response(
                        request.id,
                        Err(RpcError::new(
                            "request_limit",
                            "Too many concurrent Matrix requests",
                        )),
                    )
                    .await;
                continue;
            }
        };
        let backend = backend.clone();
        let output = output.clone();
        requests.spawn(async move {
            let _permit = permit;
            let result = backend.dispatch(&request.method, request.params).await;
            output.response(request.id, result).await;
        });
    }
    // Closing input ends the transport, not the durable operations already accepted.
    // Release output backpressure before draining those operations.
    // The same token revokes view initialization/pagination before request draining;
    // durable sends and session/key operations are deliberately not cancelled.
    closed.cancel();
    reader.abort();
    let _ = reader.await;
    // Do not cancel a login between receiving credentials and atomically saving its
    // session, or an accepted send between its durable SDK queue write and response.
    while requests.join_next().await.is_some() {}
    backend.shutdown().await;
    drop(backend);
    drop(output);
    let mut writer = writer;
    if tokio::time::timeout(Duration::from_secs(2), &mut writer)
        .await
        .is_err()
    {
        writer.abort();
        let _ = writer.await;
    }
}
