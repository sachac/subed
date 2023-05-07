;;; subed-ass.el --- Advanced SubStation Alpha implementation for subed  -*- lexical-binding: t; -*-

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

;; Advanced SubStation Alpha implementation for subed-mode.
;; Since ASS doesn't use IDs, we'll use the starting timestamp.

;;; Code:

(require 'subed)
(require 'subed-config)
(require 'subed-debug)
(require 'subed-common)

;;; Syntax highlighting

(defconst subed-ass-font-lock-keywords
  (list
   '("\\([0-9]+:\\)?[0-9]+:[0-9]+\\(\\.[0-9]+\\)?" . 'subed-time-face)
   '("\\(?:[0-9]+:\\)?[0-9]+:[0-9]+\\(?:\\.[0-9]+\\)? *\\(,\\) *\\(?:[0-9]+:\\)?[0-9]+:[0-9]+\\(?:\\.[0-9]+\\)?" 1 'subed-time-separator-face t))
  "Highlighting expressions for `subed-mode'.")


;;; Parsing

(defconst subed-ass--regexp-timestamp "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?")
(defconst subed-ass--regexp-start "\\(?:Dialogue\\|Comment\\|Picture\\|Sound\\|Movie\\|Command\\): +[0-9]+,")
(defconst subed-ass--regexp-separator "\n")

(cl-defmethod subed--timestamp-to-msecs (time-string &context (major-mode subed-ass-mode))
  "Find HH:MM:SS.MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern.
Use the format-specific function for MAJOR-MODE."
  (when (string-match subed--regexp-timestamp time-string)
    (let ((hours (string-to-number (or (match-string 2 time-string) "0")))
          (mins  (string-to-number (match-string 3 time-string)))
          (secs  (string-to-number (match-string 4 time-string)))
          (msecs (if (match-string 6 time-string) (string-to-number (subed--right-pad (match-string 6 time-string) 3 ?0)) 0)))
      (+ (* (truncate hours) 3600000)
         (* (truncate mins) 60000)
         (* (truncate secs) 1000)
         (truncate msecs)))))

(cl-defmethod subed--msecs-to-timestamp (msecs &context (major-mode subed-ass-mode))
  "Convert MSECS to string in the format H:MM:SS.CS.
Use the format-specific function for MAJOR-MODE."
  (concat (format-seconds "%h:%02m:%02s" (/ msecs 1000))
          "." (format "%02d" (/ (mod msecs 1000) 10))))

(cl-defmethod subed--subtitle-id (&context (major-mode subed-ass-mode))
  "Return the ID of the subtitle at point or nil if there is no ID.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (when (and (subed--jump-to-subtitle-time-start)
               (looking-at subed--regexp-timestamp))
      (match-string 0))))

(cl-defmethod subed--subtitle-id-at-msecs (msecs &context (major-mode subed-ass-mode))
  "Return the ID of the subtitle at MSECS milliseconds.
Return nil if there is no subtitle at MSECS.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (goto-char (point-min))
    (let* ((secs       (/ msecs 1000))
           (only-hours (truncate (/ secs 3600)))
           (only-mins  (truncate (/ (- secs (* only-hours 3600)) 60))))
      ;; Move to first subtitle in the relevant hour
      (when (re-search-forward (format "\\(%s\\|\\`\\)%02d:" subed--regexp-separator only-hours) nil t)
        (beginning-of-line)
        ;; Move to first subtitle in the relevant hour and minute
        (re-search-forward (format "\\(\n\n\\|\\`\\)%02d:%02d" only-hours only-mins) nil t)))
    ;; Move to first subtitle that starts at or after MSECS
    (catch 'subtitle-id
      (while (<= (or (subed-subtitle-msecs-start) -1) msecs)
        ;; If stop time is >= MSECS, we found a match
        (let ((cur-sub-end (subed-subtitle-msecs-stop)))
          (when (and cur-sub-end (>= cur-sub-end msecs))
            (throw 'subtitle-id (subed-subtitle-id))))
        (unless (subed--forward-subtitle-id)
          (throw 'subtitle-id nil))))))

;;; Traversing

(cl-defmethod subed--jump-to-subtitle-id (&context (major-mode subed-ass-mode) &optional sub-id)
  "Move to the ID of a subtitle and return point.
If SUB-ID is not given, focus the current subtitle's ID.
Return point or nil if no subtitle ID could be found.
ASS doesn't use IDs, so we use the starting timestamp instead.
Use the format-specific function for MAJOR-MODE."
  (if (stringp sub-id)
      (let* ((orig-point (point))
             (find-ms (subed--timestamp-to-msecs sub-id))
             (regex (concat "^\\(?:" subed-ass--regexp-start "\\)\\(" subed--regexp-timestamp "\\)"))
             done)
        (goto-char (point-min))
        (while (not done)
          (if (re-search-forward regex nil t)
              (when (= (save-match-data
                         (subed-timestamp-to-msecs
                          (match-string 1)))
                       find-ms)
                (setq done 'found)
                (goto-char (match-beginning 1)))
            (setq done 'not-found)
            (goto-char orig-point)))
        (when (eq done 'found)
          (beginning-of-line)
          (point)))
    (end-of-line)
    (let* ((regex (concat "^\\(?:" subed-ass--regexp-start "\\)\\(" subed--regexp-timestamp "\\)"))
           (match-found (re-search-backward regex nil t)))
      (when (or match-found (re-search-forward regex nil t)) ;; maybe at the beginning?
        (goto-char (match-beginning 0))
        (point)))))

(cl-defmethod subed--jump-to-subtitle-time-start (&context (major-mode subed-ass-mode) &optional sub-id)
  "Move point to subtitle's start time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no start time could be found.
Use the format-specific function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-id sub-id)
    (when (re-search-forward subed--regexp-timestamp (line-end-position) t)
      (goto-char (match-beginning 0))
      (point))))

(cl-defmethod subed--jump-to-subtitle-time-stop (&context (major-mode subed-ass-mode) &optional sub-id)
  "Move point to subtitle's stop time.
If SUB-ID is not given, use subtitle on point.  Return point or
nil if no stop time could be found.  Use the format-specific
function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-id sub-id)
    (re-search-forward (concat "\\(?:" subed--regexp-timestamp "\\),")
                       (line-end-position) t)
    (when (looking-at subed--regexp-timestamp)
      (point))))

(cl-defmethod subed--jump-to-subtitle-text (&context (major-mode subed-ass-mode) &optional sub-id)
  "Move point on the first character of subtitle's text.
If SUB-ID is not given, use subtitle on point.  Return point or
nil if a the subtitle's text can't be found.  Use the
format-specific function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-id sub-id)
    (beginning-of-line)
    (when (looking-at ".*?,.*?,.*?,.*?,.*?,.*?,.*?,.*?,.*?,")
      (goto-char (match-end 0)))
    (point)))

(cl-defmethod subed--jump-to-subtitle-end (&context (major-mode subed-ass-mode) &optional sub-id)
  "Move point after the last character of the subtitle's text.
If SUB-ID is not given, use subtitle on point.  Return point or
nil if point did not change or if no subtitle end can be found.
Use the format-specific function for MAJOR-MODE."
  (let ((orig-point (point)))
    (when (subed-jump-to-subtitle-text sub-id)
      (end-of-line)
      (unless (= orig-point (point))
        (point)))))

(cl-defmethod subed--forward-subtitle-id (&context (major-mode subed-ass-mode))
  "Move point to next subtitle's ID.
Return point or nil if there is no next subtitle.  Use the
format-specific function for MAJOR-MODE."
  (let ((pos (point)))
    (forward-line 1)
    (beginning-of-line)
    (while (not (or (eobp) (looking-at subed-ass--regexp-start)))
      (forward-line 1))
    (if (looking-at subed-ass--regexp-start)
        (point)
      (goto-char pos)
      nil)))

(cl-defmethod subed--backward-subtitle-id (&context (major-mode subed-ass-mode))
  "Move point to previous subtitle's ID.
Return point or nil if there is no previous subtitle.  Use the
format-specific function for MAJOR-MODE."
  (let ((orig-point (point)))
    (when (subed-jump-to-subtitle-id)
      (forward-line -1)
      (while (not (or (bobp) (looking-at subed-ass--regexp-start)))
        (forward-line -1))
      (if (looking-at subed-ass--regexp-start)
          (point)
        (goto-char orig-point)
        nil))))

;;; Manipulation

(cl-defmethod subed--make-subtitle (&context (major-mode subed-ass-mode)
                                             &optional _ start stop text _)
  "Generate new subtitle string.

START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
The ID and comment are ignored.

A newline is appended to TEXT, meaning you'll get two trailing
newlines if TEXT is nil or empty.  Use the format-specific
function for MAJOR-MODE."
  (format "Dialogue: 0,%s,%s,Default,,0,0,0,,%s\n"
          (subed-msecs-to-timestamp (or start 0))
          (subed-msecs-to-timestamp (or stop (+ (or start 0)
                                                subed-default-subtitle-length)))
          (replace-regexp-in-string "\n" "\\n" (or text ""))))

(cl-defmethod subed--prepend-subtitle (&context (major-mode subed-ass-mode)
                                                &optional id start stop text comment)
  "Insert new subtitle before the subtitle at point.

ID and START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT is ignored.

Move point to the text of the inserted subtitle.  Return new
point.  Use the format-specific function for MAJOR-MODE."
  (subed-jump-to-subtitle-id)
  (insert (subed-make-subtitle id start stop text comment))
  (forward-line -1)
  (subed-jump-to-subtitle-text))

(cl-defmethod subed--append-subtitle (&context (major-mode subed-ass-mode)
                                               &optional id start stop text comment)
  "Insert new subtitle after the subtitle at point.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT is ignored.

Move point to the text of the inserted subtitle.  Return new
point.  Use the format-specific function for MAJOR-MODE."
  (unless (subed-forward-subtitle-id)
    ;; Point is on last subtitle or buffer is empty
    (subed-jump-to-subtitle-end)
    (unless (bolp) (insert "\n")))
  (insert (subed-make-subtitle id start stop text comment))
  (forward-line -1)
  (subed-jump-to-subtitle-text))

(cl-defmethod subed--merge-with-next (&context (major-mode subed-ass-mode))
  "Merge the current subtitle with the next subtitle.
Update the end timestamp accordingly.  Use the format-specific
function for MAJOR-MODE."
  (save-excursion
    (subed-jump-to-subtitle-end)
    (let ((pos (point)) new-end)
      (if (subed-forward-subtitle-time-stop)
          (progn
            (when (looking-at subed--regexp-timestamp)
              (setq new-end (subed-timestamp-to-msecs (match-string 0))))
            (subed-jump-to-subtitle-text)
            (delete-region pos (point))
            (insert " ")
            (let ((subed-enforce-time-boundaries nil))
              (subed-set-subtitle-time-stop new-end)))
        (error "No subtitle to merge into")))))

(cl-defmethod subed--auto-insert (&context (major-mode subed-ass-mode))
  "Set up an empty SubStation Alpha file.
Use the format-specific function for MAJOR-MODE."
  (insert "[Script Info]
ScriptType: v4.00+
PlayResX: 384
PlayResY: 288
ScaledBorderAndShadow: yes

[V4+ Styles]
Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding
Style: Default,Arial,16,&Hffffff,&Hffffff,&H0,&H0,0,0,0,0,100,100,0,0,1,1,0,2,10,10,10,0

[Events]
Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n"))

;;;###autoload
(define-derived-mode subed-ass-mode subed-mode "Subed-ASS"
  "Major mode for editing Advanced SubStation Alpha subtitle files."
  (setq-local subed--subtitle-format "ass")
  (setq-local subed--regexp-timestamp subed-ass--regexp-timestamp)
  (setq-local subed--regexp-separator subed-ass--regexp-separator)
  (setq-local font-lock-defaults '(subed-ass-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ass\\'" . subed-ass-mode))

(provide 'subed-ass)
;;; subed-ass.el ends here
