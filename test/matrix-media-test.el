;;; matrix-media-test.el --- Matrix image lifecycle boundaries -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'matrix-avatar-test)
(require 'matrix-ui-test)
(require 'matrix-media)

(defconst matrix-media-test--source (make-string 64 ?a))
(defconst matrix-media-test--replacement (make-string 64 ?b))

(defun matrix-media-test--item (&optional source event caption)
  "Return a genuine SDK-shaped image message."
  (list :id "stable-row" :kind "message" :event_id (or event "$image:test")
        :sender "@other:test" :sender_name "Other" :timestamp 1700000000000
        :body (or caption "Image caption")
        :image (list :source_id (or source matrix-media-test--source)
                     :filename "photo.png" :width 2 :height 1)))

(defun matrix-media-test--install (app item &optional subscription)
  "Give fixture APP the authoritative SDK ITEM and exact subscription."
  (let ((model (appkit-app-model app))
        (state (matrix-ui-test--items-state (and item (list item)))))
    (setf (plist-get model :timelines) (plist-get state :timelines)
          (plist-get model :backend-active) t
          (plist-get model :session) (plist-get state :session))
    (appkit-app-send app (list 'matrix-test-snapshot model))
    (setf (plist-get (gethash "!room:test" (plist-get model :timelines)) :subscription_id)
          (or subscription "subscription"))))

(defun matrix-media-test--file (store &optional source)
  "Reuse the actual PNG fixture in a private backend-shaped media cache."
  (let* ((directory (expand-file-name "media/fixture-generation/" store))
         (file (expand-file-name (concat (or source matrix-media-test--source) ".image") directory)))
    (make-directory directory t)
    (set-file-modes (expand-file-name "media/" store) #o700)
    (set-file-modes directory #o700)
    (copy-file (matrix-avatar-test--image-file store) file t)
    (set-file-modes file #o600)
    file))

(defconst matrix-media-test--surface-type
  (appkit-surface-type-create
   :name 'matrix-media-test :mode #'special-mode
   :init (lambda (_context item) (appkit-next :model item :render 'image))
   :update (lambda (_context _model item) (appkit-next :model item :render 'image))
   :renderer-factory
   (lambda (_surface)
     (appkit-generated-renderer-create
      :mount #'ignore :unmount #'ignore :merge (lambda (_left right) right)
      :resource-request (lambda (_keys) 'image)
      :render
      (lambda (surface _view item _request)
        (let* ((app (appkit-surface-app surface))
               (timeline (matrix-room--timeline (matrix-runtime-state app) "!room:test"))
               (demand (matrix-media-demand app "!room:test"
                                            (plist-get timeline :subscription_id) item)))
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (or (plist-get item :body) "Redacted")))
          (matrix--avatar-resource-result
           surface nil (and demand (list (cons (plist-get item :id) demand))))))))))

(defun matrix-media-test--open (app item)
  "Open a real owner and actual Resource without requiring native rendering."
  (appkit-open-generated-surface matrix-media-test--surface-type
                                 :app app :identity (make-symbol "image") :input item))

(defun matrix-media-test--resource (surface item &optional subscription)
  "Return ITEM's committed Resource in SURFACE."
  (appkit-resource-state
   surface (matrix-media-resource-key (appkit-surface-app surface) "!room:test"
                                      (or subscription "subscription") item)))

(ert-deftest matrix-media-exact-identities-isolate-event-subscription-and-account ()
  (matrix-avatar-test--with-accounts
    (let* ((one (account "one")) (two (account "two"))
           (item (matrix-media-test--item))
           (key (matrix-media-resource-key one "!room:test" "subscription" item)))
      (should-not (equal key (matrix-media-resource-key two "!room:test" "subscription" item)))
      (should-not (equal key (matrix-media-resource-key one "!room:test" "new" item)))
      (should-not (equal key (matrix-media-resource-key one "!other:test" "subscription" item)))
      (should-not (equal key (matrix-media-resource-key
                             one "!room:test" "subscription"
                             (matrix-media-test--item nil "$other:test"))))
      (should (equal key (matrix-media-resource-key
                         one "!room:test" "subscription"
                         (matrix-media-test--item nil nil "Edited caption"))))
      (dolist (bad '(nil "" "mxc://example.test/source" "ABC"))
        (let ((invalid (matrix-media-test--item)))
          (setf (plist-get (plist-get invalid :image) :source_id) bad)
          (should-not (matrix-media-resource-key one "!room:test" "subscription" invalid)))))))

(ert-deftest matrix-media-source-replacement-redaction-and-stale-completion ()
  ;; Only native demand eligibility is substituted.  Resource owners, cancellation,
  ;; the committed SDK snapshot, private files and publication gates are real.
  (cl-letf (((symbol-function 'matrix-media-rendering-available-p) (lambda () t)))
    (matrix-avatar-test--with-accounts
      (let* ((app (account "one")) (item (matrix-media-test--item))
             (store (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))
             (file (matrix-media-test--file store)))
        (matrix-media-test--install app item)
        (let* ((surface (matrix-media-test--open app item))
               (old (car requests))
               (replacement (matrix-media-test--item matrix-media-test--replacement))
               (replacement-file (matrix-media-test--file store matrix-media-test--replacement)))
          (push surface surfaces)
          (appkit-surface-send surface (matrix-media-test--item nil nil "Caption edit"))
          (should (= 1 (length requests)))
          (matrix-media-test--install app replacement)
          (appkit-surface-send surface replacement)
          (should (plist-get old :cancelled))
          (should (= 2 (length requests)))
          (finish old (list :path file))
          (should (eq 'pending (appkit-resource-state-status
                               (matrix-media-test--resource surface replacement))))
          (finish (car requests) (list :path replacement-file))
          (should (eq 'ready (appkit-resource-state-status
                             (matrix-media-test--resource surface replacement))))
          (matrix-media-test--install app nil)
          (appkit-surface-send surface '(:id "stable-row" :kind "redacted" :body "Redacted"))
          (should-not (matrix-media-test--resource surface replacement))
          (should (string-match-p "Redacted" (with-current-buffer (appkit-surface-buffer surface)
                                              (buffer-string)))))))))

(ert-deftest matrix-media-current-snapshot-fences-completion-before-row-rerender ()
  (cl-letf (((symbol-function 'matrix-media-rendering-available-p) (lambda () t)))
    (matrix-avatar-test--with-accounts
      (let* ((app (account "one")) (item (matrix-media-test--item))
             (file (matrix-media-test--file
                    (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))))
        (matrix-media-test--install app item)
        (let ((surface (matrix-media-test--open app item)))
          (push surface surfaces)
          ;; The model has already committed redaction, but the old Surface has
          ;; not yet replaced its Resource interests.
          (matrix-media-test--install app nil)
          (finish (car requests) (list :path file))
          (should (eq 'failed (appkit-resource-state-status
                              (matrix-media-test--resource surface item)))))))))

(ert-deftest matrix-media-cross-account-files-and-retired-owner-are-rejected ()
  (cl-letf (((symbol-function 'matrix-media-rendering-available-p) (lambda () t)))
    (matrix-avatar-test--with-accounts
      (let* ((one (account "one")) (two (account "two"))
             (item (matrix-media-test--item))
             (file (matrix-media-test--file
                    (matrix-rpc-transport-store (plist-get (appkit-app-model one) :transport)))))
        (matrix-media-test--install one item)
        (matrix-media-test--install two item)
        (let* ((first (matrix-media-test--open one item)) (old (car requests))
               (second (matrix-media-test--open two item)))
          (push first surfaces) (push second surfaces)
          (finish (car requests) (list :path file))
          (should (eq 'failed (appkit-resource-state-status
                              (matrix-media-test--resource second item))))
          (appkit-surface-stop first)
          (should (plist-get old :cancelled))
          (let ((successor (matrix-media-test--open one item)))
            (push successor surfaces)
            (finish old (list :path file))
            (should (eq 'pending (appkit-resource-state-status
                                 (matrix-media-test--resource successor item))))
            (appkit-app-close one)
            (should (plist-get (car requests) :cancelled))))))))

(ert-deftest matrix-media-private-source-admission-rejects-links-permissions-and-markup ()
  (matrix-avatar-test--with-accounts
    (let* ((app (account "one"))
           (store (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))
           (file (matrix-media-test--file store))
           (source matrix-media-test--source))
      (should (matrix-media--source store source (list :path file)))
      (should-not (matrix-media--source store matrix-media-test--replacement (list :path file)))
      (let ((link (expand-file-name "media/link-generation" store)))
        (make-symbolic-link (file-name-directory file) link)
        (should-not (matrix-media--source
                     store source (list :path (expand-file-name (concat source ".image") link)))))
      (set-file-modes file #o644)
      (should-not (matrix-media--source store source (list :path file)))
      (set-file-modes file #o600)
      (write-region "<svg xmlns='http://www.w3.org/2000/svg'></svg>" nil file nil 'silent)
      (should-not (matrix-media--source store source (list :path file))))))

(ert-deftest matrix-media-terminal-room-keeps-caption-draft-and-starts-no-downloads ()
  (cl-letf (((symbol-function 'matrix-media-rendering-available-p) (lambda () nil))
            ((symbol-function 'matrix-runtime-resource-request)
             (lambda (&rest _) (ert-fail "Non-Canvas room must not download images"))))
    (matrix-ui-test--with-room
      (appkit-chatbuf-input-replace "Untouched draft")
      (let ((revision (appkit-chatbuf-composer-revision)))
        (appkit-app-send app (matrix-ui-test--items-state (list (matrix-media-test--item))))
        (appkit-surface-send surface '(matrix-update timeline "!room:test"))
        (should (string-match-p "Image caption" (buffer-string)))
        (should (string-match-p "Canvas display required.*photo.png" (buffer-string)))
        (should (equal (appkit-chat-timeline-keys) '("stable-row")))
        (should (equal (appkit-chatbuf-input-string) "Untouched draft"))
        (should (= revision (appkit-chatbuf-composer-revision)))))))

(ert-deftest matrix-media-avatar-and-image-share-one-interest-replacement ()
  (cl-letf (((symbol-function 'matrix-media-rendering-available-p) (lambda () t))
            ((symbol-function 'appkit-media-inline-image-rendering-available-p) (lambda (&rest _) t)))
    (matrix-avatar-test--with-accounts
      (let* ((app (account "one")) (item (matrix-media-test--item))
             (_state (matrix-media-test--install app item))
             (surface (matrix-media-test--open app item)))
        (push surface surfaces)
        (let* ((demand (matrix-media-demand app "!room:test" "subscription" item))
               (result (matrix--avatar-resource-result
                        surface '(("stable-row" . "mxc://example.test/avatar"))
                        (list (cons "stable-row" demand))))
               (update (appkit-render-result-resource-interest-update result)))
          (should (eq 'replace (appkit-resource-interest-update-mode update)))
          (should (= 2 (length (appkit-resource-interest-update-entries update))))
          (should (= 2 (length (appkit-render-result-resource-demands result)))))))))

(ert-deftest matrix-media-native-room-completion-canvas-occurrences-and-draft ()
  ;; No Canvas constructor, decoder, host or image display is substituted.
  (skip-unless (matrix-media-rendering-available-p))
  (require 'appkit-media-inline)
  (matrix-avatar-test--with-accounts
    (let* ((app (account "native")) (item (matrix-media-test--item))
           (store (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))
           (file (matrix-media-test--file store)))
      (matrix-media-test--install app item)
      (let* ((surface (matrix-ui-test--open app))
             (buffer (appkit-surface-buffer surface)))
        (push surface surfaces)
        (save-window-excursion
          (switch-to-buffer buffer)
          (appkit-surface-enable-responsive-geometry surface #'matrix--geometry)
          (appkit-chatbuf-input-replace "Never send from an image")
          (let ((revision (appkit-chatbuf-composer-revision)))
            (should (eq 'pending (appkit-resource-state-status
                                 (matrix-media-test--resource surface item))))
            (finish (car requests) (list :path file))
            (should (= 1 (length appkit-media--inline-hosts)))
            (let* ((host (car appkit-media--inline-hosts))
                   (overlay (car (appkit-media-inline-host-overlays host)))
                   (poster (appkit-media--occurrence-image
                            (appkit-media-inline-host-occurrence host)))
                   (key (matrix-media-resource-key app "!room:test" "subscription" item))
                   (cached (matrix-media-cached-poster app key 200)))
              (should (eq 'canvas (plist-get (cdr poster) :type)))
              (should (plist-get (cdr poster) :video-static-poster))
              (should (<= (plist-get (cdr poster) :data-width) appkit-media-preview-max-width))
              (should (<= (plist-get (cdr poster) :data-height) appkit-media-preview-max-height))
              (should (eq cached (matrix-media-cached-poster app key 200)))
              ;; Authored copies have separate Canvas backing stores and hosts.
              (let ((first (appkit-media--occurrence-image
                            (get-text-property (overlay-start overlay) 'appkit-media-occurrence))))
                (should-not (eq first cached)))
              (let ((draw (symbol-function 'matrix-media--poster)) (draws 0))
                ;; Count the real native renderer, never replace its output.
                (cl-letf (((symbol-function 'matrix-media--poster)
                           (lambda (&rest args)
                             (cl-incf draws)
                             (apply draw args))))
                  (matrix-media-cached-poster app key 210)
                  (matrix-media-cached-poster app key 210)
                  (should (= draws 1))
                  (matrix-media-cached-poster app key 220)
                  (matrix-media-cached-poster app key 220)
                  (should (= draws 2))
                  (should (= 1 (hash-table-count (gethash app matrix-media--posters))))))
              ;; A narrow user preview budget must scale with text, not become
              ;; an absolute pixel clamp.  Exercise the actual geometry hook.
              (let ((appkit-media-preview-max-width 20))
                (text-scale-set 0)
                (appkit-surface-send surface '(matrix-geometry))
                (let ((before (image-size
                               (appkit-media--occurrence-image
                                (appkit-media-inline-host-occurrence
                                 (car appkit-media--inline-hosts))) t)))
                  (text-scale-set 2)
                  (redisplay t)
                  (appkit-loop-run-pass (appkit-surface-loop surface))
                  (let ((after (image-size
                                (appkit-media--occurrence-image
                                 (appkit-media-inline-host-occurrence
                                  (car appkit-media--inline-hosts))) t)))
                    (should (> (car after) (car before)))
                    (should (> (cdr after) (cdr before)))
                    (should (<= (abs (- (car after)
                                        (/ (* (car before) (cdr after))
                                           (float (cdr before))))) 1)))
                  (text-scale-set 0)
                  (redisplay t)
                  (appkit-loop-run-pass (appkit-surface-loop surface))))
              (matrix-media-test--install app (matrix-media-test--item nil nil "Edited caption"))
              (appkit-surface-send surface '(matrix-update timeline "!room:test"))
              (should (= 1 (length requests)))
              (should (string-match-p "Edited caption" (buffer-string)))
              (should (= 1 (length appkit-media--inline-hosts)))
              (appkit-surface-send surface '(matrix-geometry))
              (should (= 1 (length appkit-media--inline-hosts)))
              (let ((replacement (matrix-media-test--item matrix-media-test--replacement))
                    (replacement-file (matrix-media-test--file store matrix-media-test--replacement)))
                (matrix-media-test--install app replacement)
                (appkit-surface-send surface '(matrix-update timeline "!room:test"))
                (should-not appkit-media--inline-hosts)
                (should (eq 'pending (appkit-resource-state-status
                                     (matrix-media-test--resource surface replacement))))
                (finish (car requests) (list :path replacement-file))
                (should (= 1 (length appkit-media--inline-hosts)))
                (setq host (car appkit-media--inline-hosts)))
              (should (equal (appkit-chatbuf-input-string) "Never send from an image"))
              (should (= revision (appkit-chatbuf-composer-revision)))
              (let ((redacted '(:id "stable-row" :kind "redacted" :body "Redacted")))
                (matrix-media-test--install app redacted)
                (appkit-surface-send surface '(matrix-update timeline "!room:test"))
                (should-not (appkit-media-inline-host-live-p host))
                (should-not appkit-media--inline-hosts)
                (should (equal (appkit-chatbuf-input-string) "Never send from an image"))
                (should (= revision (appkit-chatbuf-composer-revision)))))))))))

(defun matrix-media-test--video-item (event poster-own)
  "Return an SDK-shaped video message whose poster is POSTER-OWN or a thumbnail."
  (list :id event :kind "message" :event_id (concat "$" event ":test")
        :sender "@other:test" :sender_name "Other" :timestamp 1700000000000
        :body "🤔"
        :attachment (list :kind "video" :source_id matrix-media-test--replacement
                          :filename "🤔" :size 8000
                          :poster (list :source_id matrix-media-test--source
                                        :own (if poster-own t :json-false)
                                        :width 320 :height 240))))

(ert-deftest matrix-media-native-video-posters-play-their-own-or-fetched-video ()
  (skip-unless (and (matrix-media-rendering-available-p) (executable-find "ffmpeg")))
  (require 'appkit-media-inline)
  (matrix-avatar-test--with-accounts
    (let* ((app (account "video"))
           (store (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))
           (file (matrix-media-test--file store)))
      ;; The cached poster is a real WebM: a video posting its own first frame.
      (call-process "ffmpeg" nil nil nil "-nostdin" "-loglevel" "error" "-y"
                    "-f" "lavfi" "-i" "testsrc=size=320x240:rate=10" "-t" "1"
                    "-c:v" "libvpx-vp9" "-f" "webm" file)
      (dolist (own '(t nil))
        (let ((item (matrix-media-test--video-item (if own "own" "thumb") own)))
          (matrix-media-test--install app item)
          (let* ((surface (matrix-ui-test--open app))
                 (buffer (appkit-surface-buffer surface)))
            (push surface surfaces)
            (save-window-excursion
              (switch-to-buffer buffer)
              (appkit-surface-enable-responsive-geometry surface #'matrix--geometry)
              (should (string-match-p "\\[Video loading" (buffer-string)))
              (finish (car requests) (list :path file))
              (should (string-match-p "\\[video\\] 🤔" (buffer-string)))
              (let* ((host (car appkit-media--inline-hosts))
                     (plan (appkit-media-inline-host-plan host)))
                (should (eq 'canvas (plist-get (cdr (appkit-media--occurrence-image
                                                     (appkit-media-inline-host-occurrence host)))
                                               :type)))
                (should (eq 'video (appkit-media--playback-kind plan)))
                (if own
                    (should (equal (alist-get 'file (appkit-media-inline-host-resource host))
                                   file))
                  (should-not (appkit-media-inline-host-resource host))
                  (should (functionp (appkit-media--playback-resolve-function plan))))))
            (appkit-surface-stop surface)
            (kill-buffer buffer)))))))

(ert-deftest matrix-media-native-return-and-click-open-real-viewer-never-send ()
  (skip-unless (matrix-media-rendering-available-p))
  (require 'appkit-media-inline)
  (when (featurep 'evil) (require 'matrix-evil))
  (save-window-excursion
    (matrix-avatar-test--with-accounts
      (let* ((app (account "viewer")) (item (matrix-media-test--item))
             (file (matrix-media-test--file
                    (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))))
        (matrix-media-test--install app item)
        (let* ((surface (matrix-ui-test--open app))
               (buffer (appkit-surface-buffer surface))
               viewer)
          (push surface surfaces)
          (unwind-protect
              (progn
                (finish (car requests) (list :path file))
                (with-current-buffer buffer
                  (appkit-chatbuf-input-replace "Do not send this")
                  (let ((revision (appkit-chatbuf-composer-revision))
                        (host (car appkit-media--inline-hosts)))
                    (dolist (state (if (featurep 'evil) '(nil normal motion insert) '(nil)))
                      (dolist (key '("RET" "<return>" "<mouse-1>"))
                        (switch-to-buffer buffer)
                        (when (featurep 'evil)
                          (evil-local-mode (if state 1 -1))
                          (when state (funcall (intern (format "evil-%s-state" state)))))
                        (goto-char (overlay-start (car (appkit-media-inline-host-overlays host))))
                        (appkit-chatbuf-update-context-mode)
                        (cl-letf (((symbol-function 'matrix-runtime-request)
                                   (lambda (&rest _) (ert-fail "Image interaction sent the draft"))))
                          (if (equal key "<mouse-1>")
                              (call-interactively
                               (lookup-key (appkit-media-inline-host-map host) (kbd key)))
                            (execute-kbd-macro (kbd key))))
                        (setq viewer
                              (video-inline-presentation-buffer
                               (appkit-media-video-inline-inline
                                (appkit-media-inline-host-inline host))))
                        (should (buffer-live-p viewer))
                        (should (with-current-buffer viewer
                                  (video-player-live-p video--buffer-player)))
                        (with-current-buffer buffer
                          (should (equal (appkit-chatbuf-input-string) "Do not send this"))
                          (should (= revision (appkit-chatbuf-composer-revision)))))))))
            (when (buffer-live-p viewer) (kill-buffer viewer))))))))

(ert-deftest matrix-media-native-shared-source-has-independent-animation-occurrences ()
  (skip-unless (matrix-media-rendering-available-p))
  (require 'appkit-media-inline)
  (let ((appkit-media-inline-animation-enabled nil))
    (matrix-avatar-test--with-accounts
      (let* ((app (account "animated"))
             (first (matrix-media-test--item nil "$first:test"))
             (second (matrix-media-test--item nil "$second:test"))
             (store (matrix-rpc-transport-store (plist-get (appkit-app-model app) :transport)))
             (file (matrix-media-test--file store))
             ;; Same valid one-pixel frame fixture used by shared image tests.
             (header (concat "GIF89a"
                             (unibyte-string 1 0 1 0 128 0 0 0 0 0 255 255 255)))
             (frame (unibyte-string 44 0 0 0 0 1 0 1 0 0 2 2 68 1 0)))
        (let ((coding-system-for-write 'no-conversion))
          (write-region (concat header frame frame (unibyte-string 59)) nil file nil 'silent))
        (setf (plist-get second :id) "second-row")
        (matrix-media-test--install app first)
        (setf (plist-get (matrix-room--timeline (matrix-runtime-state app) "!room:test") :items)
              (list first second))
        (let ((surface (matrix-ui-test--open app)))
          (push surface surfaces)
          (dolist (request (copy-sequence requests)) (finish request (list :path file)))
          (save-window-excursion
            (switch-to-buffer (appkit-surface-buffer surface))
            (appkit-chatbuf-input-replace "Animation must leave this alone")
            (should (= 2 (length appkit-media--inline-hosts)))
            (let* ((one (car appkit-media--inline-hosts))
                   (two (cadr appkit-media--inline-hosts)))
              (should (appkit-media-inline-host-animated one))
              (should (appkit-media-inline-host-animated two))
              (should-not (eq (appkit-media-inline-host-occurrence one)
                              (appkit-media-inline-host-occurrence two)))
              (goto-char (overlay-start (car (appkit-media-inline-host-overlays one))))
              (matrix-room-return)
              (goto-char (overlay-start (car (appkit-media-inline-host-overlays two))))
              (matrix-room-return)
              (let ((inline-one (appkit-media-video-inline-inline
                                 (appkit-media-inline-host-inline one)))
                    (inline-two (appkit-media-video-inline-inline
                                 (appkit-media-inline-host-inline two))))
                (should (video-inline-target inline-one))
                (should (video-inline-target inline-two))
                (should-not (eq (video-inline-target inline-one) (video-inline-target inline-two)))
                (should-not (eq (video-target-canvas (video-inline-target inline-one))
                                (video-target-canvas (video-inline-target inline-two)))))
              (should (equal (appkit-chatbuf-input-string) "Animation must leave this alone"))
              (appkit-surface-stop surface)
              (should-not (appkit-media-inline-host-live-p one))
              (should-not (appkit-media-inline-host-live-p two)))))))))

(provide 'matrix-media-test)
;;; matrix-media-test.el ends here
