;;; matrix-rpc.el --- Owned Matrix backend transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Matrix contributors
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; One private, non-logging JSON-RPC process belongs to one AppKit Source.
;; Only its gates deliver results; transport callbacks never touch a model or UI.

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)
(require 'appkit-source)
(require 'subr-x)

(defgroup matrix nil
  "Matrix messaging with AppKit."
  :group 'applications
  :prefix "matrix-")

(defconst matrix-rpc--package-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this client and its backend checkout.")

(defcustom matrix-backend-program nil
  "Matrix backend executable, or nil to find a locally built backend.
Local release/debug builds are preferred to `exec-path'.  Build explicitly;
connecting never downloads dependencies or compiles code."
  :type '(choice (const :tag "Find backend" nil) file)
  :group 'matrix)

(defcustom matrix-request-timeout 120
  "Seconds before a backend request has an unknown outcome.
A timeout does not undo a server write; never resend it as a new message."
  :type 'natnum
  :group 'matrix)

(cl-defstruct (matrix-rpc-transport (:constructor matrix-rpc-transport-create))
  store program connection process emit close stopping timer quiesced
  (pending (make-hash-table :test #'eql)))

(cl-defstruct (matrix-rpc-call (:constructor matrix-rpc-call-create))
  "An owned, short-lived transport payload.  PARAMS is released after writing."
  id method params)

(defun matrix-rpc-program ()
  "Return an executable backend path, or report the explicit build command."
  (or (and matrix-backend-program
           (or (and (file-executable-p matrix-backend-program)
                    (expand-file-name matrix-backend-program))
               (executable-find matrix-backend-program)
               (user-error "Matrix backend is not executable")))
      (cl-loop for relative in '("backend/target/release/matrix-backend"
                                 "backend/target/debug/matrix-backend")
               for path = (expand-file-name relative matrix-rpc--package-directory)
               when (file-executable-p path) return path)
      (executable-find "matrix-backend")
      (user-error "Build the Matrix backend with make -C %s backend"
                  (directory-file-name matrix-rpc--package-directory))))

(defun matrix-rpc--timeout (method)
  "Return METHOD's response timeout; attachment downloads outlast other calls."
  (if (equal method "media.save")
      (max matrix-request-timeout 660)
    matrix-request-timeout))

(defun matrix-rpc-uncertain-error ()
  "Return a content-free error for a request whose outcome is unknown."
  '(:code -32098
    :message "Connection lost or timed out; the operation may already have succeeded"
    :data (:code "transport_uncertain")))

(defun matrix-rpc--finish (transport id result error)
  "Deliver one RESULT or ERROR for TRANSPORT request ID through its gate."
  (when (gethash id (matrix-rpc-transport-pending transport))
    (remhash id (matrix-rpc-transport-pending transport))
    (unless (matrix-rpc-transport-stopping transport)
      (funcall (matrix-rpc-transport-emit transport)
               (list 'result id result error)))))

(defun matrix-rpc--clean-buffer (buffer)
  "Retire an internal BUFFER without prompting about its dead process."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))))

(defun matrix-rpc--shutdown (transport connection)
  "Fence TRANSPORT after CONNECTION has physically stopped."
  (when-let* ((timer (matrix-rpc-transport-timer transport)))
    (cancel-timer timer)
    (setf (matrix-rpc-transport-timer transport) nil))
  (unless (matrix-rpc-transport-stopping transport)
    (funcall (matrix-rpc-transport-close transport) 'backend-exited))
  (setf (matrix-rpc-transport-stopping transport) t)
  (clrhash (matrix-rpc-transport-pending transport))
  (when-let* ((quiesced (matrix-rpc-transport-quiesced transport)))
    (setf (matrix-rpc-transport-quiesced transport) nil)
    (funcall quiesced))
  (matrix-rpc--clean-buffer
   (process-buffer (matrix-rpc-transport-process transport)))
  (matrix-rpc--clean-buffer (jsonrpc-stderr-buffer connection))
  (matrix-rpc--clean-buffer (jsonrpc-events-buffer connection)))

(defun matrix-rpc-stop (transport &optional quiesced)
  "Revoke TRANSPORT, close stdin, and report physical stop to QUIESCED.
Give the SDK time to flush; terminate a stuck backend after three seconds."
  (setf (matrix-rpc-transport-stopping transport) t
        (matrix-rpc-transport-quiesced transport) quiesced)
  (let ((process (matrix-rpc-transport-process transport)))
    (if (and process (process-live-p process))
        (progn
          (condition-case nil (process-send-eof process)
            (error (delete-process process)))
          (when (and (process-live-p process)
                     (not (matrix-rpc-transport-timer transport)))
            (setf (matrix-rpc-transport-timer transport)
                  (run-at-time
                   3 nil
                   (lambda ()
                     (when (process-live-p process) (delete-process process)))))))
      (when quiesced
        (setf (matrix-rpc-transport-quiesced transport) nil)
        (funcall quiesced)))))

(defun matrix-rpc-source-start (_context transport emit close)
  "Start TRANSPORT as a Source, using only EMIT and CLOSE for delivery."
  (setf (matrix-rpc-transport-emit transport) emit
        (matrix-rpc-transport-close transport) close)
  (condition-case nil
      (let* ((name (format "matrix-%s" (file-name-nondirectory
                                       (directory-file-name
                                        (matrix-rpc-transport-store transport)))))
             (jsonrpc-event-hook nil)
             (connection
              (jsonrpc-process-connection
               :name name
               :events-buffer-config '(:size 0)
               :process
               (lambda (_connection)
                 (let ((process
                        (make-process
                         :name name :connection-type 'pipe :noquery t
                         :command (list (matrix-rpc-transport-program transport)
                                        "--store" (matrix-rpc-transport-store transport))
                         :stderr (get-buffer-create (format "*%s stderr*" name)))))
                   (setf (matrix-rpc-transport-process transport) process)
                   process))
               :notification-dispatcher
               (lambda (_connection method params)
                 (unless (matrix-rpc-transport-stopping transport)
                   (if (eq method 'matrix.event)
                       (funcall emit (list 'event params))
                     (funcall close 'invalid-notification))))
               :request-dispatcher
               (lambda (_connection _method _params)
                 (jsonrpc-error :code -32601 :message "Client requests are not supported"))
               :on-shutdown
               (lambda (connection) (matrix-rpc--shutdown transport connection)))))
        ;; `:size 0' disables built-in logging, but user event hooks would still
        ;; receive secrets.  Disable those for this connection, not globally.
        (with-current-buffer (jsonrpc-events-buffer connection)
          (setq-local jsonrpc-event-hook nil))
        (setf (matrix-rpc-transport-connection transport) connection))
    (error
     (matrix-rpc-stop transport)
     (funcall close 'startup-failed)))
  (appkit-source-cancellation-create
   :kind 'quiescent-transport
   :cancel (lambda (quiesced) (matrix-rpc-stop transport quiesced))))

(defun matrix-rpc-source-outbound (_context transport call _settle)
  "Write CALL to TRANSPORT, returning only the Source admission outcome."
  (unwind-protect
      (let ((connection (matrix-rpc-transport-connection transport))
            (id (matrix-rpc-call-id call)))
        (cond
         ((or (matrix-rpc-transport-stopping transport)
              (not connection) (not (jsonrpc-running-p connection)))
          'closed)
         ((>= (hash-table-count (matrix-rpc-transport-pending transport)) 64)
          'backpressured)
         (t
          (puthash id t (matrix-rpc-transport-pending transport))
          (let ((debug-on-error nil) (debug-on-quit nil)
                (inhibit-debugger t))
            (condition-case nil
                (progn
                  (jsonrpc-async-request
                   connection (matrix-rpc-call-method call)
                   (or (matrix-rpc-call-params call) (make-hash-table))
                   :timeout (matrix-rpc--timeout (matrix-rpc-call-method call))
                   :success-fn
                   (lambda (result) (matrix-rpc--finish transport id result nil))
                   :error-fn
                   (lambda (error)
                     (matrix-rpc--finish
                      transport id nil
                      (if (equal (plist-get error :code) -1)
                          (matrix-rpc-uncertain-error)
                        error)))
                   :timeout-fn
                   (lambda ()
                     (matrix-rpc--finish transport id nil
                                         (matrix-rpc-uncertain-error))))
                  'accepted)
              (error
               ;; Bytes might already have reached the helper.  Do not claim
               ;; an ordinary rejection and invite duplicate message sends.
               (matrix-rpc--finish transport id nil (matrix-rpc-uncertain-error))
               'accepted))))))
    (setf (matrix-rpc-call-params call) nil)))

(defun matrix-rpc-intent-result (call outcome)
  "Map admission OUTCOME without retaining CALL's potentially secret payload."
  (setf (matrix-rpc-call-params call) nil)
  (list 'matrix-intent (matrix-rpc-call-id call) outcome))

(provide 'matrix-rpc)
;;; matrix-rpc.el ends here
