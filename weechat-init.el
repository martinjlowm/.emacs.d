;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/.emacs.d/vendor/weechat.el")
(require 'weechat)

(setq weechat-auto-monitor-buffers
      '(;; Codetalk
        "codetalk.#lobby-irc"

        ;; Freenode
        "freenode.#archlinux"
        "freenode.#gcc"
        "freenode.#hackrf"
        "freenode.#winehackers"
        "freenode.#gnus"

        ;; MIXXNet
        "mixxnet.#armind"

        ;; Twitch
        "twitch.#zeroempires"))

(defun weechat-split-windows ()
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-vertically)
  (windmove-right)
  (split-window-vertically)

  (switch-to-buffer "codetalk.#lobby-irc")
  (windmove-down)
  (switch-to-buffer "twitch.#zeroempires")
  (windmove-left)
  (switch-to-buffer "freenode.#archlinux")
  (windmove-up)
  (switch-to-buffer "freenode.#winehackers"))

(add-hook 'weechat-connect-hook 'weechat-split-windows 'append)

(load-library "weechat-notifications")

(custom-set-variables
 '(weechat-relay-ping-idle-seconds 10)
 '(weechat-header-line-format nil)
 '(weechat-prompt " » ")
 '(weechat-max-nick-length 12)
 '(weechat-text-column 16)
 '(weechat-time-format "")
 '(weechat-host-default "home.martinjlowm.dk")
 '(weechat-port-default 6667)
 '(weechat-mode-default 'ssl)
 '(weechat-fill-column 64)
 '(weechat-color-list '(unspecified "#191a14" "#717363" "#953331" "#8d4a4a"
                                    "#546a29" "#7e9960" "#a05e30"
                                    "#e0db55" "#385e6b" "#add8e6"
                                    "#7f355e" "#9f55be" "#34676f"
                                    "#84d7ef" "#b9bbab" "#fff8ce")))

(defun weechat-mode (process buffer-ptr buffer-hash)
  "Major mode used by weechat buffers.

\\{weechat-mode-map}"

  ;; Hack to restore prompt location
  (let ((prompt-start (when (boundp 'weechat-prompt-start-marker)
                        weechat-prompt-start-marker))
        (prompt-end (when (boundp 'weechat-prompt-end-marker)
                      weechat-prompt-end-marker)))

    (kill-all-local-variables)

    (puthash :emacs/buffer (current-buffer) buffer-hash)
    (add-hook 'kill-buffer-hook
              (lambda ()
                (when (hash-table-p weechat--buffer-hashes)
                  (let ((hash (weechat-buffer-hash weechat-buffer-ptr)))
                    (when (hash-table-p hash)
                      (remhash :emacs/buffer hash)))))
              nil
              'local-hook)

    (use-local-map weechat-mode-map)
    (setq mode-name "weechat")
    (setq major-mode 'weechat-mode)

    (set (make-local-variable 'weechat-buffer-ptr) buffer-ptr)
    (set (make-local-variable 'weechat-server-buffer) (process-buffer process))
    (set (make-local-variable 'weechat-buffer-number) (gethash "number" buffer-hash))
    (set (make-local-variable 'weechat-topic) (gethash "title" buffer-hash))
    (set (make-local-variable 'weechat-lines-received) 0)

    ;; Start with empty user list
    (set (make-local-variable 'weechat-user-list) nil)

    ;; Setup prompt
    (make-local-variable 'weechat-local-prompt)
    (set (make-local-variable 'weechat-prompt-start-marker)
         (or prompt-start(point-max-marker)))
    (set (make-local-variable 'weechat-prompt-end-marker)
         (or prompt-end(point-max-marker)))
    (weechat-update-prompt)

    ;; Initialize input-ring
    (set (make-local-variable 'weechat-input-ring) (make-ring weechat-input-ring-size))

    ;; Don't auto-add newlines on next-line
    (set (make-local-variable 'next-line-add-newlines) nil)
    ;; Fix scrolling
    (set (make-local-variable 'scroll-conservatively) 1000)
    (set (make-local-variable 'scroll-margin) 0)

    ;; Initialize buffer
    (weechat-request-initial-lines buffer-ptr)

    ;; Set Header
    (weechat-update-header-line-buffer (current-buffer))

    ;; Hooks
    (run-mode-hooks 'weechat-mode-hook)))

(defun weechat-word-length-at (position)
  (save-excursion
    (goto-char position)
    (cond
     ((looking-at "[[:alnum:]#&'\\.,\?]")
      (let* ((start (save-excursion
                      (skip-chars-backward "[:alnum:]'#")
                      (point)))
             (end (save-excursion
                    (skip-chars-forward "[:alnum:]&'\\.,\?")
                    (point))))
        (- end start)))
     ((looking-at "[[:space:]]")
      (skip-chars-forward "[:space:]")
      (- (point) position))
     (t 0))))

(cl-defun weechat-print-line (buffer-ptr &key prefix text date line-type highlight invisible nick)
  (setq text   (or text ""))
  (setq prefix (or prefix ""))
  (let ((buffer (weechat--emacs-buffer buffer-ptr)))
    (unless (bufferp buffer)
      (error "Couldn't find Emacs buffer for weechat-buffer %s" buffer-ptr))
    (with-current-buffer buffer
      (let ((at-end (= (point) weechat-prompt-end-marker))
            (old-point (point-marker)))
        (let ((inhibit-read-only t))
          (goto-char (marker-position weechat-prompt-start-marker))

          (save-restriction
            ;; Hack borrowed from rcirc:
            ;; temporarily set the marker insertion-type because
            ;; insert-before-markers results in hidden text in new buffers
            (set-marker-insertion-type weechat-prompt-start-marker t)
            (set-marker-insertion-type weechat-prompt-end-marker t)

            (weechat-narrow-to-line)

            (when (and date (not (string= weechat-time-format "")))
              (insert (propertize
                       (format-time-string weechat-time-format date)
                       'face 'weechat-time-face)
                      " "))

            (if (s-blank? (weechat-handle-color-codes prefix))
                (let ((chars-to-insert weechat-text-column))
                  (when (> chars-to-insert 0)
                    (insert-char ?\s chars-to-insert)))
              (let* ((colorized-prefix (weechat-handle-color-codes prefix))
                     (nick-length (if (and (integerp weechat-max-nick-length)
                                           (> weechat-max-nick-length 0))
                                      (min (length colorized-prefix)
                                           weechat-max-nick-length)
                                    (length colorized-prefix)))
                     (chars-to-insert (- weechat-text-column
                                         (+ nick-length 1))))
                (when (> chars-to-insert 0)
                  (insert-char ?\s chars-to-insert))
                (insert (if (and (integerp weechat-max-nick-length)
                                 (> weechat-max-nick-length 0))
                            (substring colorized-prefix 0
                                       nick-length)
                          colorized-prefix))))

            ;; (when (or (eq line-type :irc/privmsg)
            ;;           (not line-type))
            (insert " ");; )

            ;; (let ((chars-to-insert
            ;;        (- weechat-text-column
            ;;           (- (point-max) (point-min)))))
            ;;   (when (> chars-to-insert 0)
            ;;     (insert-char ?\s chars-to-insert)))

            ;; Calculate `prefix-string' for nice `auto-fill' (using
            ;; overlays)
            (let ((prefix-string (make-string (- (point-max) (point-min)) ?\s))
                  (text-start (point)))
              ;; trim & handle color codes
              (let* ((text (weechat-> text
                                      (s-trim)
                                      (weechat-handle-color-codes)
                                      (propertize 'weechat-text t))))
                (insert (cond
                         (highlight
                          (propertize text 'face 'weechat-highlight-face))
                         ((eq line-type :irc/x-error)
                          (propertize text 'face 'weechat-error-face))
                         (t text))
                        "\n"))

              (when weechat-fill-text
                ;; Filling is slightly misleading here.  We use this
                ;; awesome text property called `wrap-prefix'.
                (cond
                 ;; Overlay continuous lines with `prefix-string'
                 ((eq weechat-fill-column 'frame-width)
                  (let ((overlay (make-overlay text-start (point-max))))
                    (overlay-put overlay 'wrap-prefix
                                 (propertize prefix-string 'face 'default))))
                 ;; Overlay `weechat-fill-column'-length strings with a
                 ;; newline and `prefix-string'
                 ((and (numberp weechat-fill-column)
                       (> (length text) weechat-fill-column))
                  (let ((text-end (1- (point-max)))
                        (wrap-start (+ text-start weechat-fill-column)))
                    (while (< wrap-start text-end)
                      (let* ((word-length-at-start (weechat-word-length-at wrap-start))
                             (offset (save-excursion
                                       (goto-char wrap-start)
                                       (skip-chars-backward "[:alnum:]#&'\\.,\?")
                                       (- (point) wrap-start)))
                             (offset (if (or (< word-length-at-start 10) (< offset 4))
                                         offset
                                       0))
                             (midword (save-excursion
                                        (goto-char wrap-start)
                                        (looking-back "[[:alnum:]']")))
                             (hyphenate (if (and (= offset 0) midword)
                                            "-"
                                          ""))
                             (overlay (make-overlay (+ wrap-start offset)
                                                    (+ wrap-start offset 1))))
                        (overlay-put overlay 'before-string
                                     (propertize (concat hyphenate "\n" prefix-string) 'face 'default))
                        (setq wrap-start (+ wrap-start offset 1 weechat-fill-column))))))

                 ;; Default
                 (t
                  ))))

            ;; Go to start of inserted line
            (goto-char (1- (point)))    ;skip newline
            (goto-char (point-at-bol))

            ;; Add general properties
            (weechat-line-add-properties nick date highlight invisible)

            ;; Important: Run the hook after everything else
            (save-restriction
              (run-hooks 'weechat-insert-modify-hook))))

        ;; Restore old position
        (let ((p-to-go (if at-end weechat-prompt-end-marker old-point))
              (w (get-buffer-window buffer)))
          ;; ...for non-active buffers (in windows)
          (when (and (not (eq (selected-window) w))
                     (eq (current-buffer)
                         (window-buffer w)))
            (set-window-point w p-to-go))

          ;; ...for active buffer
          (goto-char p-to-go))

        ;; Recenter window if there are more lines than fit in the
        ;; frame.  This is borrowed from rcirc.
        (weechat-recenter-bottom-maybe)

        (set-marker-insertion-type weechat-prompt-start-marker nil)
        (set-marker-insertion-type weechat-prompt-end-marker nil))

      ;; Truncate
      (weechat-truncate-buffer)

      ;; Drop undo information (borrowed from weechat)
      (when (not (s-blank? (weechat-get-input)))
        (buffer-disable-undo)
        (buffer-enable-undo)))))

(defun weechat-handle-reconnect-maybe ()
  (weechat-cancel-reconnect)
  (unless (boundp 'weechat-auto-reconnect-retries-left)
    (setq weechat-auto-reconnect-retries-left
          weechat-auto-reconnect-retries))
  (when (> weechat-auto-reconnect-retries-left 0)
    (let ((host (car weechat-host-history))
          (port weechat-last-port)
          (delay 10))
      (if (not (weechat-get-password host port))
          (weechat-message "Not reconnecting: No password stored.")
        (weechat-message "Reconnecting in %ds..." delay)
        (setq weechat-reconnect-timer
              (run-with-timer
               delay nil
               (lambda ()
                 (weechat-connect
                  host
                  port
                  (weechat-get-password host port)
                  (car weechat-mode-history)
                  'force-disconnect)))))
      t)))


(defun weechat-connect (&optional host port password mode force-disconnect)
  "Connect to WeeChat.

HOST is the relay host, `weechat-host-default' by default.
PORT is the port where the relay listens, `weechat-port-default' by default.
PASSWORD is either a string, a function or nil.
MODE is null or 'plain for a plain socket, t or 'ssl for a TLS socket;
a string denotes a command to run.  You can use %h and %p to interpolate host
and port number respectively."
  (interactive
   (let* ((host
           (read-string
            (format "Relay host (default '%s'): " weechat-host-default)
            nil nil weechat-host-default))
          (port
           (read-number "Port: " (or weechat-last-port weechat-port-default)))
          (mode (let*
                    ((minibuffer-local-completion-map weechat-mode-completion-map)
                     (modestr (completing-read
                               (format "Mode (`plain', `ssl' or command, default `%s'): "
                                       weechat-mode-default)
                               '("plain" "ssl" "ssh -W localhost:%p %h")
                               nil nil nil 'weechat-mode-history
                               ;; NOTE: `completing-read' is fine when
                               ;; passed a symbol, but helm breaks.
                               ;; The following ensures we always pass
                               ;; a string.
                               (format "%s" weechat-mode-default))))
                  (cond
                   ((string= modestr "") nil)
                   ((string= modestr "plain") 'plain)
                   ((string= modestr "ssl") 'ssl)
                   (t modestr)))))
     (list
      host port
      (or
       (progn
         (weechat-message "Trying to get password via `weechat-password-callback'...")
         (weechat-get-password host port))
       ;; Use lexical-let to scramble password lambda in *Backtrace*
       (read-passwd "Password: "))
      mode
      nil)))
  ;; Cancel the reconnect timer to prevent surprises
  (weechat-cancel-reconnect)
  ;; Handle when the user is already connected etc.
  (let* ((host (or host weechat-host-default))
         (port (or port weechat-port-default))
         (password (or password
                       (weechat-get-password host port)))
         (mode (or mode weechat-mode-default)))
    (weechat-message "Weechat connecting to %s:%d" host port)
    (when (weechat-relay-connected-p)
      (if (or force-disconnect
              (y-or-n-p "Already connected.  Disconnect other connection? "))
          (weechat-relay-disconnect)
        (error "Can't open two connections")))
    (when (and (stringp host)
               (integerp port))
      (push mode weechat-mode-history)
      (setq weechat-last-port port)
      (push host weechat-host-history)
      (weechat-relay-connect
       host
       port
       mode
       (lambda (process)
         (if process
             (progn
               (weechat-relay-authenticate password)
               (weechat-relay-send-command
                "info version"
                (lambda (data)
                  (let ((version-str (cdar data)))
                    (weechat-message "Connected to '%s', version %s"
                                     host version-str)
                    (setq weechat-version version-str))
                  (weechat-update-buffer-list
                   (lambda ()
                     (weechat-relay-send-command "sync")
                     (setq weechat--connected t)
                     (weechat--relay-start-ping-timer)
                     (weechat-cancel-reconnect)
                     (run-hooks 'weechat-connect-hook))))))
           ;; Connection failed, kill buffers for the failed connection
           (kill-buffer weechat-relay-buffer-name)
           (when (get-buffer weechat-relay-log-buffer-name)
             (kill-buffer weechat-relay-log-buffer-name))
           (weechat-handle-reconnect-maybe)))))))

(defun weechat-relay-connect (host port mode &optional callback)
  "Open a new weechat relay connection to HOST at PORT.

Argument MODE Null or 'plain for a plain socket, t or 'ssl for a TLS socket;
a string denotes a command to run. You can use %h and %p to interpolate host
and port number respectively.

Optional argument CALLBACK Called after initialization is finished."
  ;; Clean relay buffer to start with clean state
  (with-current-buffer (get-buffer-create weechat-relay-buffer-name)
    (let ((inhibit-read-only t)
          (proc (get-buffer-process (current-buffer))))
      (when proc
        (set-process-sentinel proc nil)
        (delete-process proc))
      (delete-region (point-min) (point-max))))
  (let* ((pfun (cond
                ((or (null mode) (eq mode 'plain)) #'weechat--relay-plain-socket)
                ((or (eq mode t) (eq mode 'ssl)) #'weechat--relay-tls-socket)
                ((stringp mode) (weechat--relay-from-command mode))))
         (process
          (funcall pfun weechat-relay-buffer-name host port)))
    (when process
      (set-process-sentinel process #'weechat--relay-process-sentinel)
      (set-process-coding-system process 'binary)
      (set-process-filter process #'weechat--relay-process-filter)
      (with-current-buffer (get-buffer weechat-relay-buffer-name)
        (setq buffer-read-only t)
        (set-buffer-multibyte nil)
        (buffer-disable-undo))
      (with-current-buffer (get-buffer-create
                            weechat-relay-log-buffer-name)
        (buffer-disable-undo)))
    (when (functionp callback)
      (funcall callback process)))

  ;; Rethink this, it's called regardless of succesfulness
  (run-hooks 'weechat-relay-connect-hook))

(defun weechat--relay-start-ping-timer ()
  (weechat--relay-stop-ping-timer)
  (setq weechat--relay-ping-timer
        (run-with-timer
         0
         (/ weechat-relay-ping-idle-seconds 2)
         (lambda ()
           (if (weechat-relay-connected-p)
               (let
                   ((last-received (float-time (time-since weechat-relay-last-receive))))
                 (cond
                  ;; Expect a timeout if 5 seconds passes without response after
                  ;; a ping
                  ((>= last-received
                       (+ weechat-relay-ping-idle-seconds 5))
                   (weechat-relay-disconnect)
                   (run-hooks 'weechat-relay-disconnect-hook))
                  ;; If it's not there yet, simply send a ping
                  ((>= last-received
                       weechat-relay-ping-idle-seconds)
                   (weechat--relay-send-ping))))
             ;; Stop the ping timer if we aren't connected
             (weechat--relay-stop-ping-timer))))))

(defun weechat-relay-disconnect ()
  "Disconnect current weechat relay connection and close all buffers."
  (message "%s" "deeeeeeerp")
  (when (weechat-relay-connected-p)
    (message "%s" "are we connected?")
    (weechat--relay-send-message "quit")
    (with-current-buffer weechat-relay-buffer-name
      (let ((proc (get-buffer-process (current-buffer))))
        ;; When disconnecting interactively (e.g. when this function
        ;; is called), prevent running any disconnect hooks.
        (set-process-sentinel proc nil)
        (delete-process proc)
        ;; Stop the ping timer
        (weechat--relay-stop-ping-timer))
      (kill-buffer))
    (when (get-buffer weechat-relay-log-buffer-name)
      (kill-buffer weechat-relay-log-buffer-name))))

;; (defun weechat-relay-disconnect ()
;;   "Disconnect current weechat relay connection and close all buffers."
;;   (when (weechat-relay-connected-p)
;;     (weechat--relay-send-message "quit")
;;     (with-current-buffer weechat-relay-buffer-name
;;       (let ((proc (get-buffer-process (current-buffer))))
;;         ;; When disconnecting interactively (e.g. when this function
;;         ;; is called), prevent running any disconnect hooks.
;;         (when proc
;;           (set-process-sentinel proc nil)
;;           (delete-process proc))
;;         ;; Stop the ping timer
;;         (weechat--relay-stop-ping-timer))
;;       (kill-buffer))
;;     (when (get-buffer weechat-relay-log-buffer-name)
;;       (kill-buffer weechat-relay-log-buffer-name))))

;; (defun weechat--relay-send-message (text &optional id)
;;   "Send message TEXT with optional ID.
;; Trim TEXT prior to sending it."
;;   (let ((msg (concat (when id (format "(%s) " id)) (s-trim text) "\n")))
;;     (weechat-relay-log (format "Sending msg: '%s'" (s-trim msg))
;;                        :debug)
;;     (let ((process (get-buffer-process weechat-relay-buffer-name)))
;;       (if process
;;           (send-string process msg)
;;         (if (weechat-relay-connected-p)
;;             (weechat-warn "`get-buffer-process' returned nil for `weechat-relay-buffer-name'")

;;           (weechat--relay-stop-ping-timer)
;;           (unless (timerp weechat-reconnect-timer)
;;             (weechat-handle-reconnect-maybe)))))))

(weechat-connect "home.martinjlowm.dk" 6667 nil 'ssl)
