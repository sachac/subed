;;; subed.el --- A major mode for editing SubRip (srt) subtitles  -*- lexical-binding: t; -*-

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
;;
;;
;; See README.org or https://github.com/rndusr/subed for more information.
;;
;;
;;; Code:

(add-to-list 'auto-mode-alist '("\\.srt$" . subed-mode-enable))

(require 'subed-config)
(require 'subed-srt)
(require 'subed-mpv)

;; Abstraction layer to allow support for other subtitle formats
(set 'subed-font-lock-keywords 'subed-srt-font-lock-keywords)

(fset 'subed--subtitle-id 'subed-srt--subtitle-id)
(fset 'subed--subtitle-msecs-start 'subed-srt--subtitle-msecs-start)
(fset 'subed--subtitle-msecs-stop 'subed-srt--subtitle-msecs-stop)
(fset 'subed--subtitle-text 'subed-srt--subtitle-text)
(fset 'subed--subtitle-relative-point 'subed-srt--subtitle-relative-point)
(fset 'subed--adjust-subtitle-start-relative 'subed-srt--adjust-subtitle-start-relative)
(fset 'subed--adjust-subtitle-stop-relative 'subed-srt--adjust-subtitle-stop-relative)

(fset 'subed-jump-to-subtitle-id 'subed-srt-jump-to-subtitle-id)
(fset 'subed-jump-to-subtitle-time-start 'subed-srt-jump-to-subtitle-time-start)
(fset 'subed-jump-to-subtitle-time-stop 'subed-srt-jump-to-subtitle-time-stop)
(fset 'subed-jump-to-subtitle-text-at-msecs 'subed-srt-jump-to-subtitle-text-at-msecs)
(fset 'subed-jump-to-subtitle-text 'subed-srt-jump-to-subtitle-text)
(fset 'subed-jump-to-subtitle-end 'subed-srt-jump-to-subtitle-end)

(fset 'subed-forward-subtitle-id 'subed-srt-forward-subtitle-id)
(fset 'subed-backward-subtitle-id 'subed-srt-backward-subtitle-id)
(fset 'subed-forward-subtitle-text 'subed-srt-forward-subtitle-text)
(fset 'subed-backward-subtitle-text 'subed-srt-backward-subtitle-text)
(fset 'subed-forward-subtitle-time-start 'subed-srt-forward-subtitle-time-start)
(fset 'subed-backward-subtitle-time-start 'subed-srt-backward-subtitle-time-start)
(fset 'subed-forward-subtitle-time-stop 'subed-srt-forward-subtitle-time-stop)
(fset 'subed-backward-subtitle-time-stop 'subed-srt-backward-subtitle-time-stop)

(fset 'subed-increase-start-time 'subed-srt-increase-start-time)
(fset 'subed-decrease-start-time 'subed-srt-decrease-start-time)
(fset 'subed-increase-stop-time 'subed-srt-increase-stop-time)
(fset 'subed-decrease-stop-time 'subed-srt-decrease-stop-time)

(fset 'subed-subtitle-insert 'subed-srt-subtitle-insert)
(fset 'subed-subtitle-kill 'subed-srt-subtitle-kill)
(fset 'subed-sanitize 'subed-srt-sanitize)
(fset 'subed-sort 'subed-srt-sort)


;;; Debugging

(defun subed-enable-debugging ()
  "Hide debugging messages and set `debug-on-error' to `nil'."
  (interactive)
  (unless subed--debug-enabled
    (setq subed--debug-enabled t
          debug-on-error t)
    (let ((debug-buffer (get-buffer-create subed-debug-buffer))
          (debug-window (or (get-buffer-window subed-debug-buffer)
                            (split-window-horizontally (max 35 (floor (* 0.3 (window-width))))))))
      (set-window-buffer debug-window debug-buffer)
      (with-current-buffer debug-buffer
        (buffer-disable-undo)
        (setq-local buffer-read-only t)))
    (add-hook 'kill-buffer-hook 'subed-disable-debugging :append :local)))

(defun subed-disable-debugging ()
  "Display debugging messages in separate window and set
`debug-on-error' to `t'."
  (interactive)
  (when subed--debug-enabled
    (setq subed--debug-enabled nil
          debug-on-error nil)
    (let ((debug-window (get-buffer-window subed-debug-buffer)))
      (when debug-window
        (delete-window debug-window)))
    (kill-buffer subed-debug-buffer)
    (remove-hook 'kill-buffer-hook 'subed-disable-debugging :local)))

(defun subed-toggle-debugging ()
  "Display or hide debugging messages in separate window and set
`debug-on-error' to `t' or `nil'."
  (interactive)
  (if subed--debug-enabled
      (subed-disable-debugging)
    (subed-enable-debugging)))

(defun subed-debug (format-string &rest args)
  "Display message in debugging buffer if debugging is enabled."
  (when subed--debug-enabled
    (with-current-buffer (get-buffer-create subed-debug-buffer)
      (setq-local buffer-read-only nil)
      (insert (apply 'format (concat format-string "\n") args))
      (setq-local buffer-read-only t)
      (let ((debug-window (get-buffer-window subed-debug-buffer)))
        (when debug-window
          (set-window-point debug-window (goto-char (point-max))))))))


;;; Utilities

(defmacro subed--save-excursion (&rest body)
  "Restore relative point within current subtitle after executing BODY.
This also works if the buffer changes (e.g. when sorting
subtitles) as long the subtitle IDs don't change."
  (save-excursion
    `(let ((sub-id (subed--subtitle-id))
           (sub-pos (subed--subtitle-relative-point)))
       (progn ,@body)
       (subed-jump-to-subtitle-id sub-id)
       ;; Subtitle text may have changed and we may not be able to move to the
       ;; exact original position
       (condition-case nil
           (forward-char sub-pos)
         ('beginning-of-buffer nil)
         ('end-of-buffer nil)))))

(defmacro subed--for-each-subtitle (&optional beg end &rest body)
  "Run BODY for each subtitle between the region specified by BEG and END.
If END is nil, it defaults to `point-max'.
If BEG and END are both nil, run BODY only on the subtitle at point.
Before BODY is run, point is placed on the subtitle's ID."
  (declare (indent defun))
  `(atomic-change-group
     (if (not ,beg)
         ;; Run body on subtitle at point
         (progn (save-excursion (subed-jump-to-subtitle-id)
                                ,@body))
       (progn
         ;; Run body on multiple subtitles
         (save-excursion
           (goto-char ,beg)
           (subed-jump-to-subtitle-id)
           (catch 'last-subtitle-reached
             (while t
               (when (> (point) (or ,end (point-max)))
                 (throw 'last-subtitle-reached t))
               (progn ,@body)
               (unless (subed-forward-subtitle-id)
                 (throw 'last-subtitle-reached t)))))))))

(defun subed--right-pad (string length fillchar)
  "Use FILLCHAR to make STRING LENGTH characters long."
  (concat string (make-string (- length (length string)) fillchar)))


;;; Moving subtitles

(defun subed-move-subtitle-forward (&optional arg)
  "Move subtitle `subed-milliseconds-move' forward in time while
preserving its duration, i.e. increase start and stop time by the
same amount.

If the region is active, all subtitles that are fully or
partially in it are moved.

The first step is to set `subed-milliseconds-move' to the value of the
prefix argument if it is provided and if it is a number.  If the
prefix argument is provided but not numerical,
`subed-milliseconds-move' is reset to its default value.

Example usage:
  \\[universal-argument] 1000 \\[subed-move-subtitle-forward]  Move subtitle 1000ms forward in time
           \\[subed-move-subtitle-forward]  Move subtitle 1000ms forward in time again
   \\[universal-argument] 500 \\[subed-move-subtitle-forward]  Move subtitle 500ms forward in time
           \\[subed-move-subtitle-forward]  Move subtitle 500ms forward in time again
       \\[universal-argument] \\[subed-move-subtitle-forward]  Move subtitle 100ms (the default) forward in time
           \\[subed-move-subtitle-forward]  Move subtitle 100ms (the default) forward in time again"
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (subed--get-milliseconds-move arg))
        (beg (when (use-region-p) (region-beginning)))
        (end (when (use-region-p) (region-end))))
    (subed--for-each-subtitle beg end
      (subed--adjust-subtitle-start-relative msecs)
      (subed--adjust-subtitle-stop-relative msecs))))

(defun subed-move-subtitle-backward (&optional arg)
  "Move subtitle `subed-milliseconds-move' backward in time while
preserving its duration, i.e. decrease start and stop time by the
same amount.

See `subed-move-subtitle-forward'."
  (interactive "P" (if (use-region-p) (list (region-beginning) (region-end))))
  (let ((deactivate-mark nil)
        (msecs (* -1 (subed--get-milliseconds-move arg)))
        (beg (when (use-region-p) (region-beginning)))
        (end (when (use-region-p) (region-end))))
    (subed--for-each-subtitle beg end
      (subed--adjust-subtitle-start-relative msecs)
      (subed--adjust-subtitle-stop-relative msecs))))


;;; Replay time-adjusted subtitle
(defun subed-replay-adjusted-subtitle-p ()
  "Whether adjusting a subtitle's start/stop time causes the
player to jump to the subtitle's start position."
  (member 'subed--replay-adjusted-subtitle subed-subtitle-time-adjusted-hook))

(defun subed-enable-replay-adjusted-subtitle ()
  "Automatically replay a subtitle when its start/stop time is adjusted."
  (interactive)
  (unless (subed-replay-adjusted-subtitle-p)
    (add-hook 'subed-subtitle-time-adjusted-hook 'subed--replay-adjusted-subtitle :append :local)
    (subed-debug "Enabled replaying adjusted subtitle: %s" subed-subtitle-time-adjusted-hook)))

(defun subed-disable-replay-adjusted-subtitle ()
  "Do not replay a subtitle automatically when its start/stop time is adjusted."
  (interactive)
  (when (subed-replay-adjusted-subtitle-p)
    (remove-hook 'subed-subtitle-time-adjusted-hook 'subed--replay-adjusted-subtitle :local)
    (subed-debug "Disabled replaying adjusted subtitle: %s" subed-subtitle-time-adjusted-hook)))

(defun subed-toggle-replay-adjusted-subtitle ()
  "Enable or disable automatic replaying of subtitle when its
start/stop time is adjusted."
  (interactive)
  (if (subed-replay-adjusted-subtitle-p)
      (subed-disable-replay-adjusted-subtitle)
    (subed-enable-replay-adjusted-subtitle)))

(defun subed--replay-adjusted-subtitle (sub-id msecs-start)
  "Move point to currently playing subtitle."
  (subed-mpv-jump msecs-start)
  (subed-debug "Replaying subtitle at: %s" (subed-srt--msecs-to-timestamp msecs-start)))


;;; Sync point-to-player

(defun subed-sync-point-to-player-p ()
  "Whether point is automatically moved to currently playing subtitle."
  (member 'subed--sync-point-to-player subed-mpv-playback-position-hook))

(defun subed-enable-sync-point-to-player ()
  "Automatically move point to the currently playing subtitle."
  (interactive)
  (unless (subed-sync-point-to-player-p)
    (add-hook 'subed-mpv-playback-position-hook 'subed--sync-point-to-player :append :local)
    (subed-debug "Enabled syncing point to playback position: %s" subed-mpv-playback-position-hook)))

(defun subed-disable-sync-point-to-player ()
  "Do not move point automatically to the currently playing
subtitle."
  (interactive)
  (when (subed-sync-point-to-player-p)
    (remove-hook 'subed-mpv-playback-position-hook 'subed--sync-point-to-player :local)
    (subed-debug "Disabled syncing point to playback position: %s" subed-mpv-playback-position-hook)))

(defun subed-toggle-sync-point-to-player ()
  "Enable or disable moving point automatically to the currently
playing subtitle."
  (interactive)
  (if (subed-sync-point-to-player-p)
      (subed-disable-sync-point-to-player)
    (subed-enable-sync-point-to-player)))

(defun subed--sync-point-to-player (msecs)
  "Move point to currently playing subtitle."
  (when (subed-jump-to-subtitle-text-at-msecs msecs)
    (subed-debug "Synchronized point to playback position: %s -> #%s"
                 (subed-srt--msecs-to-timestamp msecs) (subed--subtitle-id))
    ;; post-command-hook is not triggered because we didn't move interactively.
    ;; But there's not really a difference, e.g. the minor mode `hl-line' breaks
    ;; unless we call its post-command function, so we do it manually.
    ;; It's also important NOT to call our own post-command function because
    ;; that causes player-to-point syncing, which would get hairy.
    (remove-hook 'post-command-hook 'subed--post-command-handler)
    (run-hooks 'post-command-hook)
    (add-hook 'post-command-hook 'subed--post-command-handler :append :local)))

(defun subed-disable-sync-point-to-player-temporarily ()
  "If point is synced to playback position, temporarily disable
that for `subed-point-sync-delay-after-motion' seconds."
  (if subed--point-sync-delay-after-motion-timer
      (cancel-timer subed--point-sync-delay-after-motion-timer)
    (setq subed--point-was-synced (subed-sync-point-to-player-p)))
  (when subed--point-was-synced
    (subed-disable-sync-point-to-player))
  (when subed--point-was-synced
    (setq subed--point-sync-delay-after-motion-timer
          (run-at-time subed-point-sync-delay-after-motion nil
                       '(lambda ()
                          (setq subed--point-sync-delay-after-motion-timer nil)
                          (subed-enable-sync-point-to-player))))))


;;; Sync player-to-point

(defun subed-sync-player-to-point-p ()
  "Whether playback position is automatically adjusted to
subtitle at point."
  (member 'subed--sync-player-to-point subed-subtitle-motion-hook))

(defun subed-enable-sync-player-to-point ()
  "Automatically seek player to subtitle at point."
  (interactive)
  (unless (subed-sync-player-to-point-p)
    (subed--sync-player-to-point)
    (add-hook 'subed-subtitle-motion-hook 'subed--sync-player-to-point :append :local)
    (subed-debug "Enabled syncing playback position to point: %s" subed-subtitle-motion-hook)))

(defun subed-disable-sync-player-to-point ()
  "Do not automatically seek player to subtitle at point."
  (interactive)
  (when (subed-sync-player-to-point-p)
    (remove-hook 'subed-subtitle-motion-hook 'subed--sync-player-to-point :local)
    (subed-debug "Disabled syncing playback position to point: %s" subed-subtitle-motion-hook)))

(defun subed-toggle-sync-player-to-point ()
  "Enable or disable automatically seeking player to subtitle at point."
  (interactive)
  (if (subed-sync-player-to-point-p)
      (subed-disable-sync-player-to-point)
    (subed-enable-sync-player-to-point)))

(defun subed--sync-player-to-point ()
  "Seek player to currently focused subtitle."
  (subed-debug "Seeking player to subtitle at point %s" (point))
  (let ((cur-sub-start (subed--subtitle-msecs-start))
        (cur-sub-stop (subed--subtitle-msecs-stop)))
    (when (and subed-mpv-playback-position cur-sub-start cur-sub-stop
               (or (< subed-mpv-playback-position cur-sub-start)
                   (> subed-mpv-playback-position cur-sub-stop)))
      (subed-mpv-jump cur-sub-start)
      (subed-debug "Synchronized playback position to point: #%s -> %s"
                   (subed--subtitle-id) cur-sub-start))))


;;; Loop over single subtitle

(defun subed-subtitle-loop-p ()
  "Whether player is rewinded to start of current subtitle every
time it reaches the subtitle's stop time."
  (or subed--subtitle-loop-start subed--subtitle-loop-stop))

(defun subed-toggle-subtitle-loop ()
  "Enable or disable looping in player over currently focused
subtitle."
  (interactive)
  (if (subed-subtitle-loop-p)
      (progn
        (remove-hook 'subed-mpv-playback-position-hook 'subed--ensure-subtitle-loop :local)
        (remove-hook 'subed-subtitle-motion-hook 'subed--set-subtitle-loop :local)
        (setq subed--subtitle-loop-start nil
              subed--subtitle-loop-stop nil)
        (subed-debug "Disabling loop: %s - %s" subed--subtitle-loop-start subed--subtitle-loop-stop))
    (progn
      (subed--set-subtitle-loop (subed--subtitle-id))
      (add-hook 'subed-mpv-playback-position-hook 'subed--ensure-subtitle-loop :append :local)
      (add-hook 'subed-subtitle-motion-hook 'subed--set-subtitle-loop :append :local)
      (subed-debug "Enabling loop: %s - %s" subed--subtitle-loop-start subed--subtitle-loop-stop))))

(defun subed--set-subtitle-loop (&optional sub-id)
  "Set loop positions to start/stop time of SUB-ID or current subtitle."
  (setq subed--subtitle-loop-start (- (subed--subtitle-msecs-start sub-id)
                                      (* subed-loop-seconds-before 1000))
        subed--subtitle-loop-stop (+ (subed--subtitle-msecs-stop sub-id)
                                     (* subed-loop-seconds-after 1000)))
  (subed-debug "Set loop: %s - %s"
               (subed-srt--msecs-to-timestamp subed--subtitle-loop-start)
               (subed-srt--msecs-to-timestamp subed--subtitle-loop-stop)))

(defun subed--ensure-subtitle-loop (cur-msecs)
  "Seek back to `subed--subtitle-loop-start' if player is after
`subed--subtitle-loop-stop'."
  (when (and subed--subtitle-loop-start subed--subtitle-loop-stop
             subed-mpv-is-playing)
    (when (or (< cur-msecs subed--subtitle-loop-start)
              (> cur-msecs subed--subtitle-loop-stop))
      (subed-debug "%s -> Looping over %s - %s"
                   (subed-srt--msecs-to-timestamp cur-msecs)
                   (subed-srt--msecs-to-timestamp subed--subtitle-loop-start)
                   (subed-srt--msecs-to-timestamp subed--subtitle-loop-stop))
      (subed-mpv-jump subed--subtitle-loop-start))))


;;; Pause player while the user is editing

(defun subed-pause-while-typing-p ()
  "Whether player is automatically paused or slowed down while
the user is editing the buffer.
See `subed-playback-speed-while-typing' and
`subed-playback-speed-while-not-typing'."
  (member 'subed--pause-while-typing after-change-functions))

(defun subed-enable-pause-while-typing ()
  "Automatically pause player while the user is editing the
buffer for `subed-unpause-after-typing-delay' seconds."
  (unless (subed-pause-while-typing-p)
    (add-hook 'after-change-functions 'subed--pause-while-typing :append :local)))

(defun subed-disable-pause-while-typing ()
  "Do not automatically pause player while the user is editing
the buffer."
  (when (subed-pause-while-typing-p)
    (remove-hook 'after-change-functions 'subed--pause-while-typing :local)))

(defun subed-toggle-pause-while-typing ()
  "Enable or disable auto-pausing while the user is editing the
buffer."
  (interactive)
  (if (subed-pause-while-typing-p)
      (subed-disable-pause-while-typing)
    (subed-enable-pause-while-typing)))

(defun subed--pause-while-typing (&rest args)
  "Pause or slow down playback for `subed-unpause-after-typing-delay' seconds."
  (when subed--unpause-after-typing-timer
    (cancel-timer subed--unpause-after-typing-timer))

  (when (or subed-mpv-is-playing subed--player-is-auto-paused)
    (if (>= 0 subed-playback-speed-while-typing)
        ;; Pause playback
        (progn
          (subed-mpv-pause)
          (setq subed--player-is-auto-paused t)
          (setq subed--unpause-after-typing-timer
                (run-at-time subed-unpause-after-typing-delay nil
                             (lambda ()
                               (setq subed--player-is-auto-paused nil)
                               (subed-mpv-unpause)))))
      ;; Slow down playback
      (progn
        (subed-mpv-playback-speed subed-playback-speed-while-typing)
        (setq subed--player-is-auto-paused t)
        (setq subed--unpause-after-typing-timer
              (run-at-time subed-unpause-after-typing-delay nil
                           (lambda ()
                             (setq subed--player-is-auto-paused nil)
                             (subed-mpv-playback-speed subed-playback-speed-while-not-typing))))))))


(defun subed-guess-video-file ()
  "Return path to video if replacing the buffer file name's
extension with members of `subed-video-extensions' yields an
existing file."
  (catch 'found-videofile
    (let ((file-base (file-name-sans-extension (buffer-file-name))))
      (dolist (extension subed-video-extensions)
        (let ((file-video (format "%s.%s" file-base extension)))
          (when (file-exists-p file-video)
            (throw 'found-videofile file-video)))))))


(defun subed-mode-enable ()
  "Enable subed mode."
  (interactive)
  (kill-all-local-variables)
  (setq-local font-lock-defaults '(subed-font-lock-keywords))
  (setq-local paragraph-start "^[[:alnum:]\n]+")
  (setq-local paragraph-separate "\n\n")
  (use-local-map subed-mode-map)
  (add-hook 'post-command-hook 'subed--post-command-handler :append :local)
  (add-hook 'before-save-hook 'subed-sort :append :local)
  (add-hook 'after-save-hook 'subed-mpv-reload-subtitles :append :local)
  (add-hook 'kill-buffer-hook 'subed-mpv-kill :append :local)
  (when subed-auto-find-video
    (let ((video-file (subed-guess-video-file)))
      (when video-file
        (subed-debug "Auto-discovered video file: %s" video-file)
        (condition-case err
            (subed-mpv-find-video video-file)
          (error (message "%s -- Set subed-auto-find-video to nil to suppress this message."
                          (car (cdr err))))))))
  (subed-enable-pause-while-typing)
  (subed-enable-sync-point-to-player)
  (subed-enable-sync-player-to-point)
  (subed-enable-replay-adjusted-subtitle)
  (setq major-mode 'subed-mode
        mode-name "SubEd")
  (setq subed--mode-enabled t)
  (run-mode-hooks 'subed-mode-hook))

(defun subed-mode-disable ()
  "Disable subed mode."
  (interactive)
  (subed-disable-pause-while-typing)
  (subed-disable-sync-point-to-player)
  (subed-disable-sync-player-to-point)
  (subed-disable-replay-adjusted-subtitle)
  (subed-mpv-kill)
  (subed-disable-debugging)
  (kill-all-local-variables)
  (remove-hook 'post-command-hook 'subed--post-command-handler :local)
  (remove-hook 'before-save-hook 'subed-sort :local)
  (remove-hook 'after-save-hook 'subed-mpv-reload-subtitles :local)
  (remove-hook 'kill-buffer-hook 'subed-mpv-kill :local)
  (setq subed--mode-enabled nil))

(defun subed-mode ()
  "Major mode for editing subtitles.

This function enables or disables subed-mode.  See also
`subed-mode-enable' and `subed-mode-disable'.

Key bindings:
\\{subed-mode-map}"
  (interactive)
  ;; Use 'enabled property of this function to store enabled/disabled status
  (if subed--mode-enabled
      (subed-mode-disable)
    (subed-mode-enable)))

(provide 'subed)
;;; subed.el ends here
