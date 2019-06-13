;;; subed-config.el --- Customization variables and hooks for subed  -*- lexical-binding: t; -*-

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

;;; Code:

;; Key bindings

(defvar subed-mode-map
  (let ((subed-mode-map (make-keymap)))
    (define-key subed-mode-map (kbd "M-n") 'subed-forward-subtitle-text)
    (define-key subed-mode-map (kbd "M-p") 'subed-backward-subtitle-text)
    (define-key subed-mode-map (kbd "C-M-a") 'subed-move-to-subtitle-text)
    (define-key subed-mode-map (kbd "C-M-e") 'subed-move-to-subtitle-end)
    (define-key subed-mode-map (kbd "M-[") 'subed-decrease-start-time-100ms)
    (define-key subed-mode-map (kbd "M-]") 'subed-increase-start-time-100ms)
    (define-key subed-mode-map (kbd "M-{") 'subed-decrease-stop-time-100ms)
    (define-key subed-mode-map (kbd "M-}") 'subed-increase-stop-time-100ms)
    (define-key subed-mode-map (kbd "M-i") 'subed-subtitle-insert)
    (define-key subed-mode-map (kbd "M-k") 'subed-subtitle-kill)
    (define-key subed-mode-map (kbd "M-s") 'subed-sort)
    (define-key subed-mode-map (kbd "C-x C-v") 'subed-mpv-find-video)
    (define-key subed-mode-map (kbd "M-SPC") 'subed-mpv-toggle-pause)
    (define-key subed-mode-map (kbd "C-x .") 'subed-toggle-sync-point-to-player)
    (define-key subed-mode-map (kbd "C-x ,") 'subed-toggle-sync-player-to-point)
    (define-key subed-mode-map (kbd "C-x p") 'subed-toggle-pause-while-typing)
    (define-key subed-mode-map (kbd "C-x l") 'subed-toggle-subtitle-loop)
    (define-key subed-mode-map (kbd "C-x /") 'subed-toggle-replay-adjusted-subtitle)
    ;; (define-key subed-mode-map (kbd "C-x [") 'subed-copy-subtitle-start-time)
    ;; (define-key subed-mode-map (kbd "C-x ]") 'subed-copy-subtitle-stop-time)
    (define-key subed-mode-map (kbd "C-x d") 'subed-toggle-debugging)
    subed-mode-map)
  "Keymap for subed-mode")


;; Syntax highlighting

(defface subed-srt-id-face
  '((t (:foreground "sandybrown")))
  "Each subtitle's consecutive number")

(defface subed-srt-time-face
  '((t (:foreground "skyblue")))
  "Start and stop times of subtitles")

(defface subed-srt-time-separator-face
  '((t (:foreground "dimgray")))
  "Separator between the start and stop time (\" --> \")")

(defface subed-srt-text-face
  '((t (:foreground "brightyellow")))
  "Text of the subtitle")


;; Variables

(defgroup subed nil
  "Major mode for editing subtitles."
  :group 'languages
  :group 'hypermedia
  :prefix "subed-")

(defvar-local subed--mode-enabled nil
  "Whether `subed-mode' is enabled.
This is automatically set by `subed-mode-enable' and `subed-mode-disable'.")

(defvar-local subed--debug-enabled nil
  "Whether `subed-debug' prints to `subed-debugging-buffer'.")

(defcustom subed-debug-buffer "*subed-debug*"
  "Name of the buffer that contains debugging messages."
  :type 'string
  :group 'subed)

(defcustom subed-mode-hook nil
  "Functions to call when entering subed mode."
  :type 'hook
  :group 'subed)

(defcustom subed-video-extensions '("mkv" "mp4" "webm" "avi" "ts")
  "Video file name extensions."
  :type 'list
  :group 'subed)

(defcustom subed-auto-find-video t
  "Whether to open the video automatically when opening a subtitle file.
The corresponding video is found by replacing the file extension
of `buffer-file-name' with those in `subed-video-extensions'.
The first existing file is then passed to `subed-open-video'."
  :type 'boolean
  :group 'subed)

(defcustom subed-playback-speed-while-typing 0.3
  "Video playback speed while the user is editing the buffer.  If
set to zero or smaller, playback is paused."
  :type 'float
  :group 'subed)

(defcustom subed-playback-speed-while-not-typing 1.0
  "Video playback speed while the user is not editing the
buffer."
  :type 'float
  :group 'subed)

(defcustom subed-unpause-after-typing-delay 1.0
  "Number of seconds to wait after typing stopped before
unpausing the player."
  :type 'float
  :group 'subed)

(defvar-local subed--unpause-after-typing-timer nil
  "Timer that waits before unpausing the player after the user
typed something.")

(defvar-local subed--player-is-auto-paused nil
  "Whether the player was paused by the user or automatically.")


(defcustom subed-subtitle-spacing 100
  "How many milliseconds to keep between subtitles."
  :type 'integer
  :group 'subed)

(defcustom subed-default-subtitle-length 1.0
  "How long to make subtitles in seconds when inserted after the last subtitle."
  :type 'float
  :group 'subed)

(defcustom subed-loop-seconds-before 0
  "When looping over a single subtitle, start the loop this many
seconds before the subtitle starts."
  :type 'float
  :group 'subed)

(defcustom subed-loop-seconds-after 0
  "When looping over a single subtitle, end the loop this many
seconds after the subtitle stop."
  :type 'float
  :group 'subed)

(defvar-local subed--subtitle-loop-start nil
  "Start position of loop in player in milliseconds.")

(defvar-local subed--subtitle-loop-stop nil
  "Stop position of loop in player in milliseconds.")


(defcustom subed-point-sync-delay-after-motion 1.0
  "Number of seconds the player can't adjust point after point
was moved by the user."
  :type 'float
  :group 'subed)

(defvar-local subed--point-sync-delay-after-motion-timer nil
  "Timer that waits before re-adding
`subed--sync-point-to-player' after temporarily removing it.")

(defvar-local subed--point-was-synced nil
  "When temporarily disabling point-to-player sync, this variable
remembers whether it was originally enabled by the user.")


(defcustom subed-mpv-socket-base "/tmp/subed-mpv-socket"
  "Path to Unix IPC socket that is passed to mpv --input-ipc-server."
  :type 'file
  :group 'subed)

(defcustom subed-mpv-executable "mpv"
  "Path or filename of mpv executable."
  :type 'file
  :group 'subed)

(defcustom subed-mpv-arguments '("--osd-level" "2" "--osd-fractions")
  "Additional arguments for \"mpv\".
The options --input-ipc-server=SRTEDIT-MPV-SOCKET and --idle are
hardcoded."
  :type '(repeat string)
  :group 'subed)


;; Hooks

(defvar subed-subtitle-time-adjusted-hook ()
  "Functions to call when a subtitle's start or stop time has
changed.")

(defvar-local subed-point-motion-hook nil
  "Functions to call after point changed.")

(defvar-local subed-subtitle-motion-hook nil
  "Functions to call after current subtitle changed.")

(defvar-local subed--status-point 1
  "Keeps track of `(point)' to detect changes.")

(defvar-local subed--status-subtitle-id 1
  "Keeps track of `(subed--subtitle-id)' to detect changes.")

(defun subed--post-command-handler ()
  "Detect point motion and user entering text and signal hooks."
  ;; Check for point motion first; skip checking for other changes if it didn't
  (let ((new-point (point)))
    (when (and new-point subed--status-point
               (not (= new-point subed--status-point)))

      ;; If point is synced to playback position, temporarily disable that to
      ;; prevent race conditions that make the cursor doesn't move unexpectedly.
      (subed-disable-sync-point-to-player-temporarily)

      (setq subed--status-point new-point)
      ;; Signal point motion
      (run-hooks 'subed-point-motion-hook)
      (let ((new-sub-id (subed--subtitle-id)))
        (when (and new-sub-id subed--status-subtitle-id
                   (not (= subed--status-subtitle-id new-sub-id)))
          (setq subed--status-subtitle-id new-sub-id)
          ;; Signal motion between subtitles
          (run-hooks 'subed-subtitle-motion-hook))))))

(provide 'subed-config)
;;; subed-config.el ends here
