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

(defgroup subed nil
  "Major mode for editing subtitles."
  :group 'files
  :group 'multimedia
  :prefix "subed-")

(defvar-local subed--subtitle-format nil
  "Short form of the subtitle format in the current buffer (e.g. \"srt\").")

;; This variable is set in subed.el to avoid compiler warnings because it uses
;; functions defined in subed-common.el, and (require 'subed-common) results in
;; recursive requires.
(defvar subed-mode-map nil
  "Keymap for ‘subed-mode’.")


;; Syntax highlighting

(defface subed-srt-id-face
  '((t (:inherit 'font-lock-constant-face)))
  "Each subtitle's consecutive number")

(defface subed-srt-time-face
  '((t (:inherit 'font-lock-string-face)))
  "Start and stop times of subtitles")

(defface subed-srt-time-separator-face
  '((t (:inherit 'font-lock-comment-face)))
  "Separator between the start and stop time (\" --> \")")

(defface subed-srt-text-face
  '((t (:inherit 'default)))
  "Text of the subtitle")

(defface subed-vtt-time-face
  '((t (:inherit 'font-lock-string-face)))
  "Start and stop times of subtitles")

(defface subed-vtt-time-separator-face
  '((t (:inherit 'font-lock-comment-face)))
  "Separator between the start and stop time (\" --> \")")

(defface subed-vtt-text-face
  '((t (:inherit 'default)))
  "Text of the subtitle")

;; Variables

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

(defcustom subed-video-extensions '("mkv" "mp4" "webm" "avi" "ts" "ogv" "wav" "ogg" "mp3")
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
          (setq subed-milliseconds-adjust arg))                    ;; Custom adjustment
        (arg
         (custom-reevaluate-setting 'subed-milliseconds-adjust)))  ;; Reset to default
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
  "Minimum time in milliseconds between subtitles when start/stop time is changed."
  :type 'integer
  :group 'subed)

(defcustom subed-default-subtitle-length 1000
  "How long to make inserted subtitles in milliseconds."
  :type 'float
  :group 'subed)

(defcustom subed-enforce-time-boundaries t
  "Non-nil means refuse time adjustments that result in invalid subtitles.
For example, refuse adjustments that result in overlapping
subtitles or negative duration."
  :type 'boolean
  :group 'subed)

(defcustom subed-sanitize-functions
  '(subed-sanitize-format
    subed-sort
    subed-trim-overlap-maybe-sanitize)
  "Functions to call when sanitizing subtitles."
  :type '(repeat function)
  :local t
  :group 'subed)

(defcustom subed-validate-functions
  '(subed-validate-format
    subed-trim-overlap-maybe-check)
  "Functions to validate this buffer.
Validation functions should throw an error or prompt the user for
action."
  :type '(repeat function)
  :local t
  :group 'subed)

(defcustom subed-loop-seconds-before 1
  "Prelude in seconds when looping over subtitle(s)."
  :type 'float
  :group 'subed)

(defcustom subed-loop-seconds-after 1
  "Addendum in seconds when looping over subtitle(s)."
  :type 'float
  :group 'subed)

(defvar-local subed--subtitle-loop-start nil
  "Start position of loop in player in milliseconds.")

(defvar-local subed--subtitle-loop-stop nil
  "Stop position of loop in player in milliseconds.")


(defcustom subed-point-sync-delay-after-motion 1.0
  "Player sync point delay in seconds after the user moves the point.
This prevents the player from moving the point while the user is
doing so."
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

(defcustom subed-mpv-arguments '("--osd-level=2" "--osd-fractions")
  "Additional arguments for \"mpv\".
The options --input-ipc-server=SRTEDIT-MPV-SOCKET and --idle are
hardcoded."
  :type '(repeat string)
  :group 'subed)

(defun subed--buffer-file-name ()
  "Return base name of buffer file name or a default name."
  (file-name-nondirectory (or (buffer-file-name) "unnamed")))

;;; Trim overlaps

;; checked by subed-sort
(defcustom subed-trim-overlap-on-save nil
  "Non-nil means trim all overlapping subtitles when saving.
Subtitles are trimmed according to `subed-trim-overlap-use-start'."
  :type '(choice
          (const :tag "Trim" nil)
          (const :tag "Do not trim" t))
  :group 'subed)

(defcustom subed-trim-overlap-check-on-save nil
  "Non-nil means check for overlapping subtitles when saving."
  :type '(choice
          (const :tag "Check" nil)
          (const :tag "Do not check" t))
  :group 'subed)

;; checked by subed mode hook
(defcustom subed-trim-overlap-check-on-load nil
  "Non-nil means check for overlapping subtitles on entering subed mode.
Subtitles are trimmed according to `subed-trim-overlap-use-start'."
  :type '(choice
          (const :tag "Check" t)
          (const :tag "Do not check" nil))
  :group 'subed)

(defcustom subed-trim-overlap-use-start nil
  "Non-nil means adjust the start time of the following subtitle for overlaps.
Otherwise, adjust the stop time of the current subtitle."
  :type '(choice
          (const :tag "Adjust stop time of the current subtitle" nil)
          (const :tag "Adjust start time of the next subtitle" t))
  :group 'subed)

;;; Hooks

(defvar-local subed-subtitle-time-adjusted-hook ()
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


(provide 'subed-config)
;;; subed-config.el ends here
