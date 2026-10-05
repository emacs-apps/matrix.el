use std::io;

use serde::Deserialize;
use serde_json::{Value, json};
use tokio::io::{AsyncBufRead, AsyncBufReadExt, AsyncReadExt, AsyncWrite, AsyncWriteExt};
use tokio::sync::mpsc;
use tokio_util::sync::CancellationToken;

pub const MAX_FRAME: usize = 8 * 1024 * 1024;
const MAX_HEADER: usize = 8192;

#[derive(Clone, Copy, Debug)]
pub struct RpcError {
    pub code: &'static str,
    pub message: &'static str,
}

impl RpcError {
    pub const fn new(code: &'static str, message: &'static str) -> Self {
        Self { code, message }
    }

    pub fn value(self) -> Value {
        let code = match self.code {
            "invalid_request" => -32600,
            "method_not_found" => -32601,
            "invalid_params" => -32602,
            "parse_error" => -32700,
            _ => -32000,
        };
        json!({"code": code, "message": self.message, "data": {"code": self.code}})
    }
}
pub type Result<T, E = RpcError> = std::result::Result<T, E>;
pub const INVALID: RpcError = RpcError::new("invalid_params", "Invalid request parameters");
pub const NETWORK: RpcError = RpcError::new(
    "matrix_request_failed",
    "Matrix request failed; check connection, permissions and verification status",
);

#[derive(Deserialize)]
pub struct Request {
    pub jsonrpc: String,
    pub id: Value,
    pub method: String,
    #[serde(default = "empty_object")]
    pub params: Value,
}

fn empty_object() -> Value {
    json!({})
}

impl Request {
    pub fn parse(bytes: &[u8]) -> Result<Self> {
        let request: Self = serde_json::from_slice(bytes)
            .map_err(|_| RpcError::new("invalid_request", "Invalid JSON-RPC request"))?;
        if request.jsonrpc != "2.0"
            || !(request.id.is_string() || request.id.is_number())
            || !request.params.is_object()
            || request.method.len() > 128
        {
            return Err(RpcError::new("invalid_request", "Invalid JSON-RPC request"));
        }
        Ok(request)
    }
}

pub fn params<T: serde::de::DeserializeOwned>(value: Value) -> Result<T> {
    serde_json::from_value(value).map_err(|_| INVALID)
}

#[derive(Clone)]
pub struct Output {
    tx: mpsc::Sender<Vec<u8>>,
    closed: CancellationToken,
}

impl Output {
    pub fn new(tx: mpsc::Sender<Vec<u8>>, closed: CancellationToken) -> Self {
        Self { tx, closed }
    }

    pub async fn send(&self, value: Value) -> bool {
        let Ok(bytes) = serde_json::to_vec(&value) else {
            self.closed.cancel();
            return false;
        };
        if bytes.len() > MAX_FRAME {
            // Never silently drop an incremental timeline notification.
            self.closed.cancel();
            return false;
        }
        tokio::select! {
            _ = self.closed.cancelled() => false,
            result = self.tx.send(bytes) => {
                if result.is_err() { self.closed.cancel(); }
                result.is_ok()
            }
        }
    }

    pub async fn event(&self, event: Value) -> bool {
        self.send(json!({"jsonrpc": "2.0", "method": "matrix.event", "params": event}))
            .await
    }

    pub async fn response(&self, id: Value, result: Result<Value>) {
        let response = match result {
            Ok(result) => json!({"jsonrpc": "2.0", "id": id, "result": result}),
            Err(error) => json!({"jsonrpc": "2.0", "id": id, "error": error.value()}),
        };
        self.send(response).await;
    }
}

fn framing_error() -> io::Error {
    io::Error::new(
        io::ErrorKind::InvalidData,
        "Invalid or oversized protocol frame",
    )
}

pub async fn read_frame<R: AsyncBufRead + Unpin>(reader: &mut R) -> io::Result<Option<Vec<u8>>> {
    let mut size = None;
    let mut header_bytes = 0;
    loop {
        let mut line = Vec::new();
        let read = (&mut *reader)
            .take((MAX_HEADER - header_bytes + 1) as u64)
            .read_until(b'\n', &mut line)
            .await?;
        if read == 0 {
            return if header_bytes == 0 {
                Ok(None)
            } else {
                Err(framing_error())
            };
        }
        header_bytes += read;
        if header_bytes > MAX_HEADER || !line.ends_with(b"\r\n") {
            return Err(framing_error());
        }
        if line == b"\r\n" {
            break;
        }
        let text = std::str::from_utf8(&line[..line.len() - 2]).map_err(|_| framing_error())?;
        let (name, value) = text.split_once(':').ok_or_else(framing_error)?;
        if name.eq_ignore_ascii_case("Content-Length") {
            if size.is_some() {
                return Err(framing_error());
            }
            let value = value.trim();
            if value.is_empty() || !value.bytes().all(|b| b.is_ascii_digit()) {
                return Err(framing_error());
            }
            let length: usize = value.parse().map_err(|_| framing_error())?;
            if length == 0 || length > MAX_FRAME {
                return Err(framing_error());
            }
            size = Some(length);
        } else if !name.eq_ignore_ascii_case("Content-Type") {
            return Err(framing_error());
        }
    }
    let mut body = vec![0; size.ok_or_else(framing_error)?];
    reader.read_exact(&mut body).await?;
    Ok(Some(body))
}

pub async fn write_frames<W: AsyncWrite + Unpin>(
    mut writer: W,
    mut rx: mpsc::Receiver<Vec<u8>>,
    closed: CancellationToken,
) {
    while let Some(body) = rx.recv().await {
        let header = format!("Content-Length: {}\r\n\r\n", body.len());
        if writer.write_all(header.as_bytes()).await.is_err()
            || writer.write_all(&body).await.is_err()
            || writer.flush().await.is_err()
        {
            closed.cancel();
            break;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn framing_consumes_exactly_one_utf8_body() {
        let mut bytes = &b"Content-Length: 2\r\n\r\n\xc3\xa9Content-Length: 2\r\n\r\n{}"[..];
        assert_eq!(
            read_frame(&mut bytes).await.unwrap().unwrap(),
            "é".as_bytes()
        );
        assert_eq!(read_frame(&mut bytes).await.unwrap().unwrap(), b"{}");
        assert!(read_frame(&mut bytes).await.unwrap().is_none());
    }

    #[tokio::test]
    async fn rejects_ambiguous_truncated_and_oversized_frames() {
        for input in [
            "Content-Length: 1\r\nContent-Length: 1\r\n\r\nx",
            "Content-Length: 8388609\r\n\r\n",
            "Content-Length: +1\r\n\r\nx",
            "Content-Length: 2\r\n\r\nx",
            "Content-Length: 1\n\nx",
        ] {
            assert!(read_frame(&mut input.as_bytes()).await.is_err());
        }
        let oversized = vec![b'a'; MAX_HEADER + 1];
        assert!(read_frame(&mut oversized.as_slice()).await.is_err());
    }

    #[test]
    fn invalid_request_never_echoes_secret_input() {
        let error = Request::parse(br#"{"password":"top-secret"}"#)
            .err()
            .unwrap();
        assert!(!error.value().to_string().contains("top-secret"));
    }
}
