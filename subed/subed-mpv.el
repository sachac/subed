;;; subed-mpv.el --- mpv integration for subed  -*- lexical-binding: t; -*-

;;; License:
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Based on:
;; https://github.com/mk-fg/emacs-setup/blob/master/extz/emms-player-mpv.el

;;; Code:

(require 'subed-config)
(require 'subed-debug)
(require 'json)

(defvar-local subed-mpv-is-playing nil
  "Whether mpv is currently playing or paused.")

(defvar-local subed-mpv-playback-speed nil
  "How fast mpv is playing the video.
1.0 is normal speed, 0.5 is half speed, etc.")

(defvar-local subed-mpv-playback-position nil
  "Current playback position in milliseconds.")

(defvar-local subed-mpv-playback-position-hook nil
  "Functions to call when mpv changes playback position.")

(defvar-local subed-mpv-file-loaded-hook '(subed-mpv-pause
                                           subed-mpv-jump-to-current-subtitle)
  "Functions to call when mpv has loaded a file and starts playing.")

(defvar-local subed-mpv-video-file nil "Current file.")

(defvar-local subed-mpv--server-proc nil
  "Running mpv process.")

(defvar-local subed-mpv--client-proc nil
  "IPC socket process that communicates over `subed-mpv--socket'.")

(defconst subed-mpv--client-test-request
  (json-encode (list :command '(get_property mpv-version)))
  "Request as a string to send to check whether IPC connection is working.")

(defconst subed-mpv--retry-delays
  ;; Sums up to 5 seconds in total before failing
  '(0.1 0.1 0.1 0.1 0.2 0.2 0.3 0.4 0.5 0.5 0.5 0.5 0.5 0.5 0.5)
  "List of delays between attemps to connect to `subed-mpv--socket'.")

(defvar-local subed-mpv--client-command-queue nil
  "Commands to call when connection to `subed-mpv--socket' is established.")


;;; Server (mpv process that provides an IPC socket)

(defun subed-mpv--socket ()
  "Path to mpv's RPC socket for a particular buffer.
See also `subed-mpv-socket-dir'."
  (unless (file-exists-p subed-mpv-socket-dir)
    (condition-case err
        (make-directory subed-mpv-socket-dir :create-parents)
      (file-error
       (error "%s" (mapconcat #'identity (cdr err) ": ")))))
  (concat (file-name-as-directory subed-mpv-socket-dir)
          (format "%s:%s"
                  (let ((filename (file-name-sans-extension (subed--buffer-file-name))))
                    (substring (substring filename 0 (min (length filename) 20))))
                  (buffer-hash))))

(defun subed-mpv--server-start (&rest args)
  "Run mpv in JSON IPC mode.

Pass ARGS as command line arguments.  \"--idle\" and
\"--input-ipc-server\" are hardcoded."
  (subed-mpv--server-stop)
  (let* ((socket-file (subed-mpv--socket))
         (argv (append (list subed-mpv-executable
                             (format "--input-ipc-server=%s" socket-file)
                             "--idle")
                       args)))
    (when (file-exists-p socket-file)
      (error "An mpv instance for %s is already running: %s"
             (subed--buffer-file-name) socket-file))
    (subed-debug "Running %s" argv)
    (condition-case err
        (setq subed-mpv--server-proc (make-process :command argv
                                                   :name "subed-mpv-server"
                                                   :buffer nil
                                                   :noquery t))
      (error
       (error "%s" (mapconcat #'identity (cdr (cdr err)) ": "))))))

(defun subed-mpv--server-stop ()
  "Kill a running mpv process."
  (when (and subed-mpv--server-proc (process-live-p subed-mpv--server-proc))
    (delete-process subed-mpv--server-proc)
    (subed-debug "Killed mpv process"))
  (let ((socket-file (subed-mpv--socket)))
    (when (file-exists-p socket-file)
      (subed-debug "Removing IPC socket: %s" socket-file)
      (ignore-errors (delete-file socket-file))))
  (setq subed-mpv--server-proc nil))

(defun subed-mpv--server-started-p ()
  "Whether `subed-mpv--server-proc' is a running process."
  (if subed-mpv--server-proc t nil))


;;; Client (elisp process that connects to server's IPC socket)

(defun subed-mpv--client-buffer ()
  "Unique name of buffer that store RPC responses."
  (let ((buffer-name (format "*subed-mpv-buffer:%s-%s*"
                             (file-name-base (or (buffer-file-name) "unnamed"))
                             (buffer-hash))))
    (if subed-debugging-enabled-p
        buffer-name
      (concat " " buffer-name))))

(defun subed-mpv--client-connect (delays)
  "Try to connect to `subed-mpv--socket'.
If a connection attempt fails, wait (car DELAYS) seconds and try
again with (cdr DELAYS) as arguments."
  (subed-debug "Attempting to connect to IPC socket: %s" (subed-mpv--socket))
  (subed-mpv--client-disconnect)
  ;; NOTE: make-network-process doesn't fail when the socket file doesn't exist
  (let ((proc (make-network-process :name "subed-mpv-client"
                                    :family 'local
                                    :service (subed-mpv--socket)
                                    :coding '(utf-8 . utf-8)
					                :buffer (subed-mpv--client-buffer)
                                    :filter #'subed-mpv--client-filter
                                    :noquery t
                                    :nowait t)))
    ;; Test connection by sending a test request
    (condition-case err
        (progn
          (process-send-string proc (concat subed-mpv--client-test-request "\n"))
          (subed-debug "Connected to %s (%s)" proc (process-status proc))
          (setq subed-mpv--client-proc proc))
      (error
       (if delays
           (progn
             (subed-debug "Failed to connect (trying again in %s seconds)" (car delays))
             (run-at-time (car delays) nil #'subed-mpv--client-connect (cdr delays)))
         (progn
           (subed-debug "Connection failed: %s" err))))))
  ;; Run commands that were sent while the connection wasn't up yet
  (when (subed-mpv--client-connected-p)
    (while subed-mpv--client-command-queue
      (let ((cmd (pop subed-mpv--client-command-queue)))
        (subed-debug "Running queued command: %s" cmd)
        (apply #'subed-mpv--client-send (list cmd))))))

(defun subed-mpv--client-disconnect ()
  "Close connection to mpv process, if there is one."
  (when (subed-mpv--client-connected-p)
    (delete-process subed-mpv--client-proc)
    (subed-debug "Closed connection to mpv process"))
  (setq subed-mpv--client-proc nil
        subed-mpv-is-playing nil
        subed-mpv-playback-position nil))

(defun subed-mpv--client-connected-p ()
  "Whether the server connection has been established and tested successfully."
  (if subed-mpv--client-proc t nil))

(defun subed-mpv--client-send (cmd)
  "Send JSON IPC command.
If we're not connected yet but the server has been started, add
CMD to `subed-mpv--client-command-queue' which is evaluated by
`subed-mpv--client-connect' when the connection is up."
  (if (subed-mpv--client-connected-p)
      (let ((request-data (concat (json-encode (list :command cmd)))))
        (subed-debug "Sending request: %s" request-data)
        (condition-case err
            (process-send-string subed-mpv--client-proc (concat request-data "\n"))
          (error
           (subed-mpv-kill)
           (error "Unable to send commands via %s: %s" (subed-mpv--socket) (cdr err))))
        t)
    (when (subed-mpv--server-started-p)
      (subed-debug "Queueing command: %s" cmd)
      (setq subed-mpv--client-command-queue (append subed-mpv--client-command-queue (list cmd)))
      t)))

(defun subed-mpv--client-filter (proc response)
  "Handle response from the server.

PROC is the mpv process and RESPONSE is the response as a JSON
string."
  ;; JSON-STRING contains zero or more lines with JSON encoded objects, e.g.:
  ;;   {"data":"mpv 0.29.1","error":"success"}
  ;;   {"data":null,"request_id":1,"error":"success"}
  ;;   {"event":"start-file"}{"event":"tracks-changed"}
  ;; JSON-STRING can also contain incomplete JSON, e.g. `{"error":"succ'.
  ;; Therefore we maintain a buffer and process only complete lines.
  (when (buffer-live-p (process-buffer proc))
    (let ((orig-buffer (current-buffer)))
	  (with-current-buffer (process-buffer proc)
        ;; Insert new response where previous response ended
	    (let* ((proc-mark (process-mark proc))
               (moving (= (point) proc-mark)))
		  (save-excursion
		    (goto-char proc-mark)
		    (insert response)
		    (set-marker proc-mark (point)))
		  (if moving (goto-char proc-mark)))
	    ;; Process and remove all complete lines of JSON (lines are complete if
	    ;; they end with \n)
	    (let ((p0 (point-min)))
		  (while (progn (goto-char p0)
                        (end-of-line)
			            (equal (following-char) ?\n))
		    (let* ((p1 (point))
			       (line (buffer-substring p0 p1)))
			  (delete-region p0 (+ p1 1))
              ;; Return context to the subtitle file buffer because we're using
              ;; buffer-local variables to store player state.
              (with-current-buffer orig-buffer
			    (subed-mpv--client-handle-json line)))))))))

(defun subed-mpv--client-handle-json (json-string)
  "Process server response JSON-STRING."
  (let* ((json-data (condition-case nil
                        (json-read-from-string json-string)
                      (error
                       (subed-debug "Unable to parse JSON response:\n%S" json-string)
                       nil)))
         (event (when json-data
                  (alist-get 'event json-data))))
    (when event
      (subed-mpv--client-handle-event json-data))))

(defun subed-mpv--client-handle-event (json-data)
  "Handler for relevant mpv events.

JSON-DATA is mpv's JSON response in the form of an association
list.

See \"List of events\" in mpv(1)."
  (let ((event (alist-get 'event json-data)))
    (pcase event
      ("property-change"
       (when (string= (alist-get 'name json-data) "time-pos")
         (let ((pos-msecs (* 1000 (or (alist-get 'data json-data) 0))))
           (setq subed-mpv-playback-position (round pos-msecs))
           (run-hook-with-args 'subed-mpv-playback-position-hook subed-mpv-playback-position))))
      ("file-loaded"
       (setq subed-mpv-is-playing t)
       ;; TODO: Remove this code.  It seems unnecessary now.  Not sure why, but
       ;; I can't reproduce the issue.
       ;; Because mpv can report the player position AFTER the file was loaded
       ;; we disable automatic movement of point for a while so that the effect
       ;; of `subed-mpv-jump-to-current-subtitle' isn't cancelled immediately.
       ;; (subed-disable-sync-point-to-player-temporarily)
       (run-hooks 'subed-mpv-file-loaded-hook))
      ("unpause"
       (setq subed-mpv-is-playing t))
      ((or "pause" "end-file" "shutdown" "idle")
       (setq subed-mpv-is-playing nil)))))



;;; High-level functions

(defun subed-mpv-pause ()
  "Stop playback."
  (interactive)
  (when subed-mpv-is-playing
    (when (subed-mpv--client-send `(set_property pause yes))
      (subed-mpv--client-handle-event '((event . "pause"))))))

(defun subed-mpv-unpause ()
  "Start playback."
  (interactive)
  (unless subed-mpv-is-playing
    (when (subed-mpv--client-send `(set_property pause no))
      (subed-mpv--client-handle-event '((event . "unpause"))))))

(defun subed-mpv-toggle-pause ()
  "Start or stop playback."
  (interactive)
  (if subed-mpv-is-playing (subed-mpv-pause) (subed-mpv-unpause)))

(defun subed-mpv-playback-speed (factor)
  "Play video slower (FACTOR < 1) or faster (FACTOR > 1)."
  (interactive)
  (unless (eq subed-mpv-playback-speed factor)
    (when (subed-mpv--client-send `(set_property speed ,factor))
      (setq subed-mpv-playback-speed factor))))

(defun subed-mpv-seek (msec)
  "Move playback position MSEC milliseconds relative to current position."
  (subed-mpv--client-send `(seek ,(/ msec 1000.0) relative+exact)))

(defun subed-mpv-jump (msec)
  "Move playback position to absolute position MSEC milliseconds."
  (subed-mpv--client-send `(seek ,(/ msec 1000.0) absolute+exact)))

(defun subed-mpv-jump-to-current-subtitle ()
  "Move playback position to start of currently focused subtitle if possible."
  (let ((cur-sub-start (subed-subtitle-msecs-start)))
    (when cur-sub-start
      (subed-debug "Seeking player to focused subtitle: %S" cur-sub-start)
      (subed-mpv-jump cur-sub-start))))

(defun subed-mpv-frame-step ()
  "Step one frame forward.
Set up keybindings so that repeatedly pressing `,' and `.' moves
by frames until any other key is pressed."
  (interactive)
  (subed-mpv--client-send `(frame-step))
  (set-transient-map subed-mpv-frame-step-map))

(defun subed-mpv-frame-back-step ()
  "Step one frame backward.
Set up keybindings so that repeatedly pressing `,' and `.' moves
by frames until any other key is pressed."
  (interactive)
  (subed-mpv--client-send `(frame-back-step))
  (set-transient-map subed-mpv-frame-step-map))

(defun subed-mpv-add-subtitles (file)
  "Load FILE as subtitles in mpv."
  (subed-mpv--client-send `(sub-add ,file select)))

(defun subed-mpv-reload-subtitles ()
  "Reload subtitle file from disk."
  (subed-mpv--client-send '(sub-reload)))

(defun subed-mpv--is-video-file-p (filename)
  "Return whether FILENAME is a video file or directory."
  (and (not (or (string= filename ".") (string= filename "..")))
       (let ((filepath (expand-file-name filename)))
         (or (file-directory-p filepath)
             (member (file-name-extension filename) subed-video-extensions)))))

(defun subed-mpv--play (video)
  "Open VIDEO and play it in mpv."
  (when (subed-mpv--server-started-p)
    (subed-mpv-kill))
  (when (apply #'subed-mpv--server-start subed-mpv-arguments)
      (subed-debug "Opening video: %s" video)
      (subed-mpv--client-connect subed-mpv--retry-delays)
      (subed-mpv--client-send `(loadfile ,video replace))
      ;; mpv won't add the subtitles if the file doesn't exist yet, so we add it
      ;; via after-save-hook.
      (if (file-exists-p (buffer-file-name))
          (subed-mpv-add-subtitles (buffer-file-name))
        (add-hook 'after-save-hook #'subed-mpv--add-subtitle-after-first-save :append :local))
      (subed-mpv--client-send `(observe_property 1 time-pos))
      (subed-mpv-playback-speed subed-playback-speed-while-not-typing)))

(defun subed-mpv-play-video-from-url (url)
  "Open video file from URL in mpv.
See the mpv manual for a list of supported URL types.  If you
have youtube-dl installed, mpv can open videos from a variety of
hosting providers."
  (interactive "MURL: ")
  (setq subed-mpv-video-file url)
  (subed-mpv--play url))

(defun subed-mpv-find-video (file)
  "Open video file FILE in mpv.

Video files are expected to have any of the extensions listed in
`subed-video-extensions'."
  (interactive (list (read-file-name "Find video: " nil nil t nil #'subed-mpv--is-video-file-p)))
  (setq subed-mpv-video-file (expand-file-name file))
  (subed-mpv--play (expand-file-name file)))

(defun subed-mpv--add-subtitle-after-first-save ()
  "Tell mpv to load subtitles from function `buffer-file-name'.

Don't send the load command to mpv if `subed-subtitle-id' returns
nil because that likely means the file is empty or invalid.

This function is supposed to be added to `after-save-hook', and
it removes itself from it so mpv doesn't add the same file every
time the buffer is saved."
  (when (subed-subtitle-id)
    (subed-mpv-add-subtitles (buffer-file-name))
    (remove-hook 'after-save-hook #'subed-mpv--add-subtitle-after-first-save :local)))

(defun subed-mpv-kill ()
  "Close connection to mpv process and kill the process."
  (subed-mpv--client-disconnect)
  (subed-mpv--server-stop))

(provide 'subed-mpv)
;;; subed-mpv.el ends here
