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


;;; Commentary:

;; Customization variables, hooks, keybindings, etc for subed-mode.


;;; Code:

(defvar subed-mode-map
  (let ((subed-mode-map (make-keymap)))
    (define-key subed-mode-map (kbd "M-n") #'subed-forward-subtitle-text)
    (define-key subed-mode-map (kbd "M-p") #'subed-backward-subtitle-text)
    (define-key subed-mode-map (kbd "C-M-a") #'subed-jump-to-subtitle-text)
    (define-key subed-mode-map (kbd "C-M-e") #'subed-jump-to-subtitle-end)
    (define-key subed-mode-map (kbd "M-[") #'subed-decrease-start-time)
    (define-key subed-mode-map (kbd "M-]") #'subed-increase-start-time)
    (define-key subed-mode-map (kbd "M-{") #'subed-decrease-stop-time)
    (define-key subed-mode-map (kbd "M-}") #'subed-increase-stop-time)
    (define-key subed-mode-map (kbd "C-M-n") #'subed-move-subtitle-forward)
    (define-key subed-mode-map (kbd "C-M-p") #'subed-move-subtitle-backward)
    (define-key subed-mode-map (kbd "C-M-f") #'subed-shift-subtitle-forward)
    (define-key subed-mode-map (kbd "C-M-b") #'subed-shift-subtitle-backward)
    (define-key subed-mode-map (kbd "M-i") #'subed-subtitle-insert)
    (define-key subed-mode-map (kbd "M-k") #'subed-subtitle-kill)
    (define-key subed-mode-map (kbd "M-s") #'subed-sort)
    (define-key subed-mode-map (kbd "M-SPC") #'subed-mpv-toggle-pause)
    (define-key subed-mode-map (kbd "C-c C-d") #'subed-toggle-debugging)
    (define-key subed-mode-map (kbd "C-c C-v") #'subed-mpv-find-video)
    (define-key subed-mode-map (kbd "C-c C-p") #'subed-toggle-pause-while-typing)
    (define-key subed-mode-map (kbd "C-c C-l") #'subed-toggle-subtitle-loop)
    (define-key subed-mode-map (kbd "C-c C-r") #'subed-toggle-replay-adjusted-subtitle)
    (define-key subed-mode-map (kbd "C-c [") #'subed-copy-player-pos-to-start-time)
    (define-key subed-mode-map (kbd "C-c ]") #'subed-copy-player-pos-to-stop-time)
    (define-key subed-mode-map (kbd "C-c .") #'subed-toggle-sync-point-to-player)
    (define-key subed-mode-map (kbd "C-c ,") #'subed-toggle-sync-player-to-point)
    subed-mode-map)
  "Keymap for ‘subed-mode’.")


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

(defvar-local subed-mode--enabled-p nil
  "Whether `subed-mode' is enabled.
This is set by `subed-mode-enable' and `subed-mode-disable'.")

(defvar-local subed-debugging-enabled-p nil
  "Whether debugging messages are displayed.")

(defcustom subed-debug-buffer "*subed-debug*"
  "Name of the buffer that contains debugging messages."
  :type 'string
  :group 'subed)

(defcustom subed-mode-hook nil
  "Functions to call when entering subed mode."
  :type 'hook
  :group 'subed)

(defcustom subed-video-extensions '("mkv" "mp4" "webm" "avi" "ts" "ogv")
  "Video file name extensions."
  :type 'list
  :group 'subed)

(defcustom subed-auto-find-video t
  "Whether to open the video automatically when opening a subtitle file."
  :type 'boolean
  :group 'subed)


(defcustom subed-milliseconds-adjust 100
  "Milliseconds to add or subtract from start/stop time.

This variable is used when adjusting, moving or shifting
subtitles without a prefix argument.

This variable is set when adjusting, moving or shifting subtitles
with a prefix argument.  See `subed-increase-start-time' for
details.

Use `setq-default' to change the default value of this variable."
  :type 'float
  :group 'subed)

(defun subed-get-milliseconds-adjust (arg)
  "Set `subed-milliseconds-adjust' to ARG if it's a number.

If ARG is non-nil, reset `subed-milliseconds-adjust' to its
default.

Return new `subed-milliseconds-adjust' value."
  (cond ((integerp arg)
          (setq subed-milliseconds-adjust arg))                     ;; Custom adjustment
        (arg
         (custom-reevaluate-setting #'subed-milliseconds-adjust)))  ;; Reset to default
  subed-milliseconds-adjust)


(defcustom subed-playback-speed-while-typing 0
  "Video playback speed while the user is editing the buffer.
If set to zero or smaller, playback is paused."
  :type 'float
  :group 'subed)

(defcustom subed-playback-speed-while-not-typing 1.0
  "Video playback speed while the user is not editing the buffer."
  :type 'float
  :group 'subed)

(defcustom subed-unpause-after-typing-delay 1.0
  "Number of seconds to wait after typing stopped before unpausing the player."
  :type 'float
  :group 'subed)

(defvar-local subed--player-is-auto-paused nil
  "Whether the player was paused by the user or automatically.")


(defcustom subed-subtitle-spacing 100
  "Milliseconds between subtitles when adjusting or inserting subtitles."
  :type 'integer
  :group 'subed)

(defcustom subed-default-subtitle-length 1.0
  "How long to make subtitles in seconds when inserted after the last subtitle."
  :type 'float
  :group 'subed)

(defcustom subed-loop-seconds-before 0
  "Prelude in seconds when looping over subtitle(s)."
  :type 'float
  :group 'subed)

(defcustom subed-loop-seconds-after 0
  "Addendum in seconds when looping over subtitle(s)."
  :type 'float
  :group 'subed)

(defvar-local subed--subtitle-loop-start nil
  "Start position of loop in player in milliseconds.")

(defvar-local subed--subtitle-loop-stop nil
  "Stop position of loop in player in milliseconds.")


(defcustom subed-point-sync-delay-after-motion 1.0
  "Number of seconds the player can't adjust point after point was moved by the user."
  :type 'float
  :group 'subed)

(defvar-local subed--point-was-synced nil
  "When temporarily disabling point-to-player sync, this variable
remembers whether it was originally enabled by the user.")


(defcustom subed-mpv-socket-dir (concat (temporary-file-directory) "subed-mpv-socket")
  "Path to Unix IPC socket that is passed to mpv's --input-ipc-server option."
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

(defun subed--buffer-file-name ()
  "Return base name of buffer file name or a default name."
  (file-name-nondirectory (or (buffer-file-name) "unnamed")))


;; Hooks

(defvar subed-subtitle-time-adjusted-hook ()
  "Functions to call when a subtitle's start or stop time has changed.
The functions are called with the subtitle's start time.")

(defun subed--run-subtitle-time-adjusted-hook ()
  "Run `subed-subtitle-time-adjusted-hook' functions.
The functions are called with the subtitle's start time."
  (when subed-subtitle-time-adjusted-hook
    (run-hook-with-args 'subed-subtitle-time-adjusted-hook
                        (subed-subtitle-msecs-start))))

(defvar-local subed-point-motion-hook nil
  "Functions to call after point changed.")

(defvar-local subed-subtitle-motion-hook nil
  "Functions to call after current subtitle changed.")

(defvar-local subed--status-point 1
  "Keeps track of `(point)' to detect changes.")

(defvar-local subed--status-subtitle-id 1
  "Keeps track of `(subed-subtitle-id)' to detect changes.")

(defun subed--post-command-handler ()
  "Detect point motion and user entering text and signal hooks."
  ;; Check for point motion first; skip checking for other changes if it didn't
  (let ((new-point (point)))
    (when (and new-point subed--status-point
               (not (= new-point subed--status-point)))

      ;; If point is synced to playback position, temporarily prevent unexpected
      ;; movement of the cursor.
      (subed-disable-sync-point-to-player-temporarily)

      (setq subed--status-point new-point)
      ;; Signal point motion
      (run-hooks 'subed-point-motion-hook)
      (let ((new-sub-id (subed-subtitle-id)))
        (when (and new-sub-id subed--status-subtitle-id
                   (not (= subed--status-subtitle-id new-sub-id)))
          (setq subed--status-subtitle-id new-sub-id)
          ;; Signal motion between subtitles
          (run-hooks 'subed-subtitle-motion-hook))))))

(provide 'subed-config)
;;; subed-config.el ends here
