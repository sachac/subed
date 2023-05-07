;;; subed-tsv.el --- Tab-separated subtitles, such as Audacity labels  -*- lexical-binding: t; -*-

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

;; This file supports tab-separated values such as labels exported from Audacity.
;; Example:
;;
;; 6.191196 27.488912 This is a test
;; 44.328966  80.733201 This is another line, a little longer than the first.

;;; Code:

(require 'subed)
(require 'subed-config)
(require 'subed-debug)
(require 'subed-common)

;;; Syntax highlighting

(defconst subed-tsv-font-lock-keywords
  (list
   '("^\\([0-9]+\\.[0-9]+\\)\t\\([0-9]+\\.[0-9]+\\)" (0 'subed-time-face)))
  "Highlighting expressions for `subed-mode'.")

;;; Parsing

(defconst subed-tsv--regexp-timestamp "\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?")
(defconst subed-tsv--regexp-separator "\n")

(cl-defmethod subed--timestamp-to-msecs (time-string &context (major-mode subed-tsv-mode))
  "Find SS.MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern.
Use the format-specific function for MAJOR-MODE."
  (save-match-data
    (when (string-match subed-tsv--regexp-timestamp time-string)
      (* 1000 (string-to-number (match-string 0 time-string))))))

(cl-defmethod subed--msecs-to-timestamp (msecs &context (major-mode subed-tsv-mode))
  "Convert MSECS to string in the format H:MM:SS.CS.
Use the format-specific function for MAJOR-MODE."
  ;; We need to wrap format-seconds in save-match-data because it does regexp
  ;; stuff and we need to preserve our own match-data.
  (format "%f" (/ msecs 1000.0)))

(cl-defmethod subed--subtitle-id (&context (major-mode subed-tsv-mode))
  "Return the ID of the subtitle at point or nil if there is no ID.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (when (subed-jump-to-subtitle-id)
      (when (looking-at subed-tsv--regexp-timestamp)
        (match-string 0)))))

(cl-defmethod subed--subtitle-id-max (&context (major-mode subed-tsv-mode))
  "Return the ID of the last subtitle or nil if there are no subtitles.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (goto-char (point-max))
    (subed-subtitle-id)))

(cl-defmethod subed--subtitle-id-at-msecs (msecs &context (major-mode subed-tsv-mode))
  "Return the ID of the subtitle at MSECS milliseconds.
Return nil if there is no subtitle at MSECS.
Use the format-specific function for MAJOR-MODE."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      ;; Move to first subtitle that starts at or after MSECS
      (catch 'subtitle-id
        (while (<= (or (subed-subtitle-msecs-start) -1) msecs)
          ;; If stop time is >= MSECS, we found a match
          (let ((cur-sub-end (subed-subtitle-msecs-stop)))
            (when (and cur-sub-end (>= cur-sub-end msecs))
              (throw 'subtitle-id (subed-subtitle-id))))
          (unless (subed-forward-subtitle-id)
            (throw 'subtitle-id nil)))))))

(cl-defmethod subed--subtitle-msecs-start (&context (major-mode subed-tsv-mode) &optional sub-id)
  "Subtitle start time in milliseconds or nil if it can't be found.
If SUB-ID is not given, use subtitle on point.
Use the format-specific function for MAJOR-MODE."
  (let ((timestamp (save-excursion
                     (when (subed-jump-to-subtitle-time-start sub-id)
                       (when (looking-at subed-tsv--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-timestamp-to-msecs timestamp))))

(cl-defmethod subed--subtitle-msecs-stop (&context (major-mode subed-tsv-mode) &optional sub-id)
  "Subtitle stop time in milliseconds or nil if it can't be found.
If SUB-ID is not given, use subtitle on point.
Use the format-specific function for MAJOR-MODE."
  (let ((timestamp (save-excursion
                     (when (subed-jump-to-subtitle-time-stop sub-id)
                       (when (looking-at subed-tsv--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-timestamp-to-msecs timestamp))))

(cl-defmethod subed--subtitle-text (&context (major-mode subed-tsv-mode) &optional sub-id)
  "Return subtitle's text or an empty string.
If SUB-ID is not given, use subtitle on point.
Use the format-specific function for MAJOR-MODE."
  (or (save-excursion
        (let ((beg (subed-jump-to-subtitle-text sub-id))
              (end (subed-jump-to-subtitle-end sub-id)))
          (when (and beg end)
            (buffer-substring beg end))))
      ""))

(cl-defmethod subed--subtitle-relative-point (&context (major-mode subed-tsv-mode))
  "Point relative to subtitle's ID or nil if ID can't be found.
Use the format-specific function for MAJOR-MODE."
  (let ((start-point (save-excursion
                       (when (subed-jump-to-subtitle-id)
                         (point)))))
    (when start-point
      (- (point) start-point))))

;;; Traversing

(cl-defmethod subed--jump-to-subtitle-id (&context (major-mode subed-tsv-mode) &optional sub-id)
  "Move to the ID of a subtitle and return point.
If SUB-ID is not given, focus the current subtitle's ID.
Return point or nil if no subtitle ID could be found.
ASS doesn't use IDs, so we use the starting timestamp instead.
Use the format-specific function for MAJOR-MODE."
  (save-match-data
    (if (stringp sub-id)
        (let* ((orig-point (point))
               (find-ms (subed-timestamp-to-msecs sub-id))
               done)
          (goto-char (point-min))
          ;; Find the first timestamp that ends after the time we're looking for
          (catch 'found-ending-after
            (while (not (eobp))
              (when (and (looking-at (concat "^[^\t]+\t\\(" subed-tsv--regexp-timestamp "\\)\t"))
                         (> (subed-timestamp-to-msecs (match-string 1)) find-ms ))
                (throw 'found-ending-after "Found ending"))
              (forward-line 1)))
          ;; Does the time fit in the current one?
          (if (>= find-ms (subed-subtitle-msecs-start))
              (progn
                (beginning-of-line)
                (point))
            (goto-char orig-point)
            nil))
      (beginning-of-line)
      (when (looking-at (concat subed-tsv--regexp-timestamp "\t" subed-tsv--regexp-timestamp "\t.*"))
        (point)))))

(cl-defmethod subed--jump-to-subtitle-id-at-msecs (msecs &context (major-mode subed-tsv-mode))
  "Move point to the ID of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-tsv--subtitle-id-at-msecs'.
Use the format-specific function for MAJOR-MODE."
  (let ((current-sub-id (subed-subtitle-id))
        (target-sub-id (subed-subtitle-id-at-msecs msecs)))
    (when (and target-sub-id current-sub-id (not (equal target-sub-id current-sub-id)))
      (subed-jump-to-subtitle-id target-sub-id))))

(cl-defmethod subed--jump-to-subtitle-text-at-msecs (msecs &context (major-mode subed-tsv-mode))
  "Move point to the text of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-tsv--subtitle-id-at-msecs'.
Use the format-specific function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-id-at-msecs msecs)
    (subed-jump-to-subtitle-text)))

(cl-defmethod subed--jump-to-subtitle-time-start (&context (major-mode subed-tsv-mode) &optional sub-id)
  "Move point to subtitle's start time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no start time could be found.
Use the format-specific function for MAJOR-MODE."
  (save-match-data
    (when (subed-jump-to-subtitle-id sub-id)
      (when (re-search-forward subed-tsv--regexp-timestamp (line-end-position) t)
        (goto-char (match-beginning 0))
        (point)))))

(cl-defmethod subed--jump-to-subtitle-time-stop (&context (major-mode subed-tsv-mode) &optional sub-id)
  "Move point to subtitle's stop time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no stop time could be found.
Use the format-specific function for MAJOR-MODE."
    (save-match-data
    (when (subed-jump-to-subtitle-id sub-id)
      (re-search-forward (concat "\\(?:" subed-tsv--regexp-timestamp "\\)\t")
                         (point-at-eol) t)
      (when (looking-at subed-tsv--regexp-timestamp)
        (point)))))

(cl-defmethod subed--jump-to-subtitle-text (&context (major-mode subed-tsv-mode) &optional sub-id)
  "Move point on the first character of subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if a the subtitle's text can't be found.
Use the format-specific function for MAJOR-MODE."
    (when (subed-jump-to-subtitle-id sub-id)
    (beginning-of-line)
    (when (looking-at ".*?\t.*?\t")
      (goto-char (match-end 0)))
    (point)))

(cl-defmethod subed--jump-to-subtitle-end (&context (major-mode subed-tsv-mode) &optional sub-id)
  "Move point after the last character of the subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if point did not change or if no subtitle end
can be found.
Use the format-specific function for MAJOR-MODE."
    (save-match-data
    (let ((orig-point (point)))
      (when (subed-jump-to-subtitle-text sub-id)
        (end-of-line)
        (unless (= orig-point (point))
          (point))))))

(cl-defmethod subed--forward-subtitle-id (&context (major-mode subed-tsv-mode))
  "Move point to next subtitle's ID.
Return point or nil if there is no next subtitle.
Use the format-specific function for MAJOR-MODE."
    (save-match-data
    (let ((pos (point)))
      (forward-line 1)
      (if (eobp)
          (prog1 nil (goto-char pos))
        (beginning-of-line)
        (if (looking-at subed-tsv--regexp-timestamp)
            (point)
          (goto-char pos)
          nil)))))

(cl-defmethod subed--backward-subtitle-id (&context (major-mode subed-tsv-mode))
  "Move point to previous subtitle's ID.
Return point or nil if there is no previous subtitle.
Use the format-specific function for MAJOR-MODE."
    (let ((orig-point (point)))
      (if (bobp)
          nil
        (when (subed-jump-to-subtitle-id)
          (if (bobp)
              (progn (goto-char orig-point) nil)
            (forward-line -1)
            (while (not (or (bobp) (looking-at subed-tsv--regexp-timestamp)))
              (forward-line -1))
            (if (looking-at subed-tsv--regexp-timestamp)
                (point)
              (goto-char orig-point)
              nil))))))

(cl-defmethod subed--forward-subtitle-text (&context (major-mode subed-tsv-mode))
  "Move point to next subtitle's text.
Return point or nil if there is no next subtitle.
Use the format-specific function for MAJOR-MODE."
    (when (subed-forward-subtitle-id)
    (subed-jump-to-subtitle-text)))

(cl-defmethod subed--backward-subtitle-text (&context (major-mode subed-tsv-mode))
  "Move point to previous subtitle's text.
Return point or nil if there is no previous subtitle.
Use the format-specific function for MAJOR-MODE."
    (when (subed-backward-subtitle-id)
    (subed-jump-to-subtitle-text)))

(cl-defmethod subed--forward-subtitle-end (&context (major-mode subed-tsv-mode))
  "Move point to end of next subtitle.
Return point or nil if there is no next subtitle.
Use the format-specific function for MAJOR-MODE."
    (when (subed-forward-subtitle-id)
    (subed-jump-to-subtitle-end)))

(cl-defmethod subed--backward-subtitle-end (&context (major-mode subed-tsv-mode))
  "Move point to end of previous subtitle.
Return point or nil if there is no previous subtitle.
Use the format-specific function for MAJOR-MODE."
    (when (subed-backward-subtitle-id)
    (subed-jump-to-subtitle-end)))

(cl-defmethod subed--forward-subtitle-time-start (&context (major-mode subed-tsv-mode))
  "Move point to next subtitle's start time.
Use the format-specific function for MAJOR-MODE."
    (when (subed-forward-subtitle-id)
    (subed-jump-to-subtitle-time-start)))

(cl-defmethod subed--backward-subtitle-time-start (&context (major-mode subed-tsv-mode))
  "Move point to previous subtitle's start time.
Use the format-specific function for MAJOR-MODE."
    (when (subed-backward-subtitle-id)
    (subed-jump-to-subtitle-time-start)))

(cl-defmethod subed--forward-subtitle-time-stop (&context (major-mode subed-tsv-mode))
  "Move point to next subtitle's stop time.
Use the format-specific function for MAJOR-MODE."
    (when (subed-forward-subtitle-id)
    (subed-jump-to-subtitle-time-stop)))

(cl-defmethod subed--backward-subtitle-time-stop (&context (major-mode subed-tsv-mode))
  "Move point to previous subtitle's stop time.
Use the format-specific function for MAJOR-MODE."
    (when (subed-backward-subtitle-id)
      (subed-jump-to-subtitle-time-stop)))

;;; Manipulation

(cl-defmethod subed--set-subtitle-time-start (msecs &context (major-mode subed-tsv-mode) &optional sub-id)
  "Set subtitle start time to MSECS milliseconds.

If SUB-ID is not given, set the start of the current subtitle.

Return the new subtitle start time in milliseconds.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (when (or (not sub-id)
              (and sub-id (subed-jump-to-subtitle-id sub-id)))
      (subed-jump-to-subtitle-time-start)
      (when (looking-at subed-tsv--regexp-timestamp)
        (replace-match (subed-msecs-to-timestamp msecs))))))

(cl-defmethod subed--set-subtitle-time-stop (msecs &context (major-mode subed-tsv-mode) &optional sub-id)
  "Set subtitle stop time to MSECS milliseconds.

If SUB-ID is not given, set the stop of the current subtitle.

Return the new subtitle stop time in milliseconds.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (when (or (not sub-id)
              (and sub-id (subed-jump-to-subtitle-id sub-id)))
      (subed-jump-to-subtitle-time-stop)
      (when (looking-at subed-tsv--regexp-timestamp)
        (replace-match (subed-msecs-to-timestamp msecs))))))

(cl-defmethod subed--make-subtitle (&context (major-mode subed-tsv-mode) &optional id start stop text comment)
  "Generate new subtitle string.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT is ignored.

A newline is appended to TEXT, meaning you'll get two trailing
newlines if TEXT is nil or empty.
Use the format-specific function for MAJOR-MODE."
  (format "%s\t%s\t%s\n"
          (subed-msecs-to-timestamp (or start 0))
          (subed-msecs-to-timestamp (or stop (+ (or start 0)
                                                subed-default-subtitle-length)))
          (replace-regexp-in-string "\n" " " (or text ""))))

(cl-defmethod subed--prepend-subtitle (&context (major-mode subed-tsv-mode) &optional id start stop text comment)
  "Insert new subtitle before the subtitle at point.

ID and START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT is ignored.

Move point to the text of the inserted subtitle.
Return new point.
Use the format-specific function for MAJOR-MODE."
  (subed-jump-to-subtitle-id)
  (insert (subed-make-subtitle id start stop text comment))
  (forward-line -1)
  (subed-jump-to-subtitle-text))

(cl-defmethod subed--append-subtitle (&context (major-mode subed-tsv-mode) &optional id start stop text comment)
  "Insert new subtitle after the subtitle at point.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
COMMENT is ignored.

Move point to the text of the inserted subtitle.
Return new point.
Use the format-specific function for MAJOR-MODE."
  (unless (subed-forward-subtitle-id)
    ;; Point is on last subtitle or buffer is empty
    (subed-jump-to-subtitle-end)
    (unless (bolp) (insert "\n")))
  (insert (subed-make-subtitle id start stop text comment))
  (forward-line -1)
  (subed-jump-to-subtitle-text))

(cl-defmethod subed--kill-subtitle (&context (major-mode subed-tsv-mode))
  "Remove subtitle at point.
Use the format-specific function for MAJOR-MODE."
    (let ((beg (save-excursion (subed-jump-to-subtitle-id)
                             (point)))
        (end (save-excursion (subed-jump-to-subtitle-id)
                             (when (subed-forward-subtitle-id)
                               (point)))))
    (if (not end)
        ;; Removing the last subtitle because forward-subtitle-id returned nil
        (setq beg (save-excursion (goto-char beg)
                                  (subed-backward-subtitle-end)
                                  (1+ (point)))
              end (save-excursion (goto-char (point-max)))))
    (delete-region beg end)))

(cl-defmethod subed--merge-with-next (&context (major-mode subed-tsv-mode))
  "Merge the current subtitle with the next subtitle.
Update the end timestamp accordingly.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (subed-jump-to-subtitle-end)
    (let ((pos (point)) new-end)
      (if (subed-forward-subtitle-time-stop)
          (progn
            (when (looking-at subed-tsv--regexp-timestamp)
              (setq new-end (subed-timestamp-to-msecs (match-string 0))))
            (subed-jump-to-subtitle-text)
            (delete-region pos (point))
            (insert " ")
            (let ((subed-enforce-time-boundaries nil))
              (subed-set-subtitle-time-stop new-end)))
        (error "No subtitle to merge into")))))


;;; Initialization

(define-derived-mode subed-tsv-mode subed-mode
  "Subed-TSV"
  "Tab-separated subtitles, such as from exporting text labels from Audacity."
  (setq-local subed--subtitle-format "tsv")
  (setq-local subed--regexp-timestamp subed-tsv--regexp-timestamp)
  (setq-local subed--regexp-separator subed-tsv--regexp-separator)
  (setq-local font-lock-defaults '(subed-tsv-font-lock-keywords)))

(provide 'subed-tsv)
;;; subed-tsv.el ends here
