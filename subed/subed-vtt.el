;;; subed-vtt.el --- WebVTT implementation for subed  -*- lexical-binding: t; -*-

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

;; WebVTT implementation for subed-mode.
;; Since WebVTT doesn't use IDs, we'll use the starting timestamp.

;;; Code:

(require 'subed)
(require 'subed-config)
(require 'subed-debug)
(require 'subed-common)

;;; Syntax highlighting

(defconst subed-vtt-font-lock-keywords
  (list
   '("\\([0-9]+:\\)?[0-9]+:[0-9]+\\.[0-9]+" . 'subed-time-face)
   '("\\.[0-9]+ +\\(-->\\) +[0-9]+:" 1 'subed-time-separator-face t))
  "Highlighting expressions for `subed-mode'.")


;;; Parsing

(defconst subed-vtt--regexp-timestamp "\\(?:\\(?:[0-9]+\\):\\)?\\(?:[0-9]+\\):\\(?:[0-9]+\\)\\(?:\\.\\(?:[0-9]+\\)\\)?")
(defconst subed-vtt--regexp-separator "\\(?:\\(?:[ \t]*\n\\)+\\(?:NOTE[ \t\n]*[ \t]*\n[ \t]*\n\\)?\\)"
  "Blank lines and possibly a comment.")
(defconst subed-vtt--regexp-blank-separator "\\(?:[ \t]*\n[ \t]*\n\\|\\`\\(?:[ \t\n]*\\)\\)")
(defconst subed-vtt--regexp-note "\\(NOTE[ \t\n]\\)")
(defconst subed-vtt--regexp-identifier
  ;; According to https://developer.mozilla.org/en-US/docs/Web/API/WebVTT_API
  ;; Cues can start with an identifier which is a non empty line that does
  ;; not contain "-->".
  "[ \t]*[^ \t\n-]\\(?:[^\n-]\\|-[^\n-]\\|--[^\n>]\\)*[ \t]*")
(defconst subed-vtt--regexp-start-of-line "\\(?:\\`\\|\n\\)")
(defconst subed-vtt--regexp-timing
  (concat "[ \t]*" subed-vtt--regexp-timestamp "[ \t]*-->[ \t]*" subed-vtt--regexp-timestamp))
(defconst subed-vtt--regexp-maybe-identifier-and-timing
  (format "%s\\(%s%s\\|%s\\)"
          subed-vtt--regexp-blank-separator
          subed-vtt--regexp-identifier
          subed-vtt--regexp-timing
          subed-vtt--regexp-timing))

(cl-defmethod subed--timestamp-to-msecs (time-string &context (major-mode subed-vtt-mode))
  "Find HH:MM:SS.MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern.
Use the format-specific function for MAJOR-MODE."
  (when (string-match "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?"
                      time-string)
    (let ((hours (string-to-number (or (match-string 2 time-string) "0")))
          (mins  (string-to-number (match-string 3 time-string)))
          (secs  (string-to-number (match-string 4 time-string)))
          (msecs (string-to-number (subed--right-pad (or (match-string 5 time-string) "0")  3 ?0))))
      (+ (* (truncate hours) 3600000)
         (* (truncate mins) 60000)
         (* (truncate secs) 1000)
         (truncate msecs)))))

(cl-defmethod subed--msecs-to-timestamp (msecs &context (major-mode subed-vtt-mode))
  "Convert MSECS to string in the format HH:MM:SS.MS.
Use the format-specific function for MAJOR-MODE."
  (concat (format-seconds "%02h:%02m:%02s" (/ (floor msecs) 1000))
          "." (format "%03d" (mod (floor msecs) 1000))))

(cl-defmethod subed--subtitle-id (&context (major-mode subed-vtt-mode))
  "Return the ID of the subtitle at point or nil if there is no ID.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (when (subed-jump-to-subtitle-id)
      (cond
       ((looking-at (concat "[ \t]*\\(" subed-vtt--regexp-timestamp "\\)"))
        (match-string 1))
       ((looking-at subed-vtt--regexp-identifier)
        (string-trim (match-string 0)))))))

(cl-defmethod subed--subtitle-id-at-msecs (msecs &context (major-mode subed-vtt-mode))
  "Return the ID of the subtitle at MSECS milliseconds.
Return nil if there is no subtitle at MSECS.  Use the
format-specific function for MAJOR-MODE."
  (save-excursion
    (goto-char (point-min))
    (unless (subed-subtitle-id)
      (subed-forward-subtitle-time-start))
    ;; Move to first subtitle that starts at or after MSECS
    (catch 'subtitle-id
      (while (<= (or (subed-subtitle-msecs-start) -1) msecs)
        ;; If stop time is >= MSECS, we found a match
        (let ((cur-sub-end (subed-subtitle-msecs-stop)))
          (when (and cur-sub-end (>= cur-sub-end msecs))
            (throw 'subtitle-id (subed-subtitle-id))))
        (unless (subed-forward-subtitle-id)
          (throw 'subtitle-id nil))))))

;;; Traversing

(cl-defmethod subed--in-header-p (&context (major-mode subed-vtt-mode))
  "Return non-nil if the point is in the file header.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (let ((orig-point (point)))
      (goto-char (line-beginning-position))
      (cond
       ((and (looking-at subed-vtt--regexp-timing)
             (looking-back subed-vtt--regexp-start-of-line))
        nil)
       ;; not looking right at a timestamp; check the previous line to see if it's blank
       ((and
         (looking-back subed-vtt--regexp-blank-separator)
         (looking-at (format "[ \t]*\\(%s\n%s\\|%s\\)"
                             subed-vtt--regexp-identifier
                             subed-vtt--regexp-timing
                             subed-vtt--regexp-note)))
        nil)
       ((re-search-backward (concat "^" subed-vtt--regexp-timing) nil t) nil)
       ((re-search-backward (concat subed-vtt--regexp-blank-separator subed-vtt--regexp-note) nil t) nil)
       (t t)))))

(cl-defmethod subed--in-comment-p (&context (major-mode subed-vtt-mode))
  "Return non-nil if the point is in a comment.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (goto-char (line-beginning-position))
    (cond
     ((looking-at subed-vtt--regexp-timing)
      nil)
     ((and (looking-at (format "%s\n%s"
                               subed-vtt--regexp-identifier
                               subed-vtt--regexp-timing))
           (looking-back subed-vtt--regexp-blank-separator))
      nil)
     ;; looking at a comment
     ((and
       (looking-back "^[ \t]*\n\\|\\`\\(?:[ \t\n]*\\)")
       (looking-at (concat "[ \t]*" subed-vtt--regexp-note)))
      t)
     ((re-search-backward (format "\\(%s%s\\)\\|%s%s"
                                  subed-vtt--regexp-blank-separator
                                  subed-vtt--regexp-note
                                  subed-vtt--regexp-start-of-line
                                  subed-vtt--regexp-timing)
                          nil t)
      ;;  found the note instead of the timestamp
      (when (match-string 1)
        t)))))

(cl-defmethod subed--jump-to-subtitle-id (&context (major-mode subed-vtt-mode) &optional sub-id)
  "Move to the ID of a subtitle and return point.
If SUB-ID is not given, focus the current subtitle's ID.
Return point or nil if no subtitle ID could be found.
WebVTT IDs are optional.  If an ID is not specified, use the timestamp instead.
Use the format-specific function for MAJOR-MODE."
  (let ((orig-point (point))
        found
        (timestamp-line
         (format "\\(?:\\`\\|\n[ \t]*\n+\\)\\(\\(%s\\)\n%s\\|\\(%s\\)\\)"
                 subed-vtt--regexp-identifier
                 subed-vtt--regexp-timing
                 subed-vtt--regexp-timing)))
    (if (stringp sub-id)
        ;; Look for a line that contains the timestamp, preceded by one or more
        ;; blank lines or the beginning of the buffer.
        (let* ((regex (concat "\\(" subed-vtt--regexp-blank-separator "\\|\\`\\)\\("
                              (regexp-quote sub-id) "\\)[ \n]")))
          (goto-char (point-min))
          (setq found (re-search-forward regex nil t))
          (if found
              (goto-char (match-beginning 2))
            (goto-char orig-point)
            nil))
      (catch 'done
        (goto-char (line-beginning-position))
        (cond
         ((subed-in-header-p) (throw 'done nil))
         ;; are we looking at a timestamp? Stay here, check for ID later
         ((looking-at subed-vtt--regexp-timing) (point))
         ;; are we at an ID? return that
         ((and (or (looking-back subed-vtt--regexp-blank-separator) (bobp))
               (not (looking-at "[ \t]*\n"))
               (not (save-excursion (re-search-forward "-->" (line-end-position) t)))
               (save-excursion
                 (forward-line)
                 (looking-at subed-vtt--regexp-timing)))
          (throw 'done (point)))
         ;; are we in a comment? Search forward
         ((subed-in-comment-p)
          (if (re-search-forward (concat subed-vtt--regexp-start-of-line "\\(" subed-vtt--regexp-timing "\\)") nil t)
              (goto-char (match-beginning 1))
            (throw 'done nil)))
         ;; scan backwards for a timestamp
         ((re-search-backward (concat subed-vtt--regexp-start-of-line "\\(" subed-vtt--regexp-timing "\\)") nil t)
          (goto-char (match-beginning 1))))
        ;; We are at a timestamp; check backwards for an ID
        (cond
         ((bobp) (throw 'done (point)))
         ((progn
            (previous-line)
            (goto-char (line-beginning-position))
            (and (or (looking-back subed-vtt--regexp-blank-separator) (bobp))
                 (not (looking-at "[ \t]*\n"))
                 (save-excursion (not (re-search-forward "-->" (line-end-position) t)))))
          (throw 'done (point)))
         (t
          ;; go back
          (forward-line 1)
          (point)))))))

(cl-defmethod subed--jump-to-subtitle-time-start (&context (major-mode subed-vtt-mode) &optional sub-id)
  "Move point to subtitle's start time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no start time could be found.
Use the format-specific function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-id sub-id)
    (if (looking-at subed-vtt--regexp-timing)
        (point)
      (when (re-search-forward subed-vtt--regexp-timing nil t)
        (goto-char (match-beginning 0))
        (point)))))

(cl-defmethod subed--jump-to-subtitle-time-stop (&context (major-mode subed-vtt-mode) &optional sub-id)
  "Move point to subtitle's stop time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no stop time could be found.
Use the format-specific function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-time-start sub-id)
    (re-search-forward "[ \t]*-->[ \t]*" (line-end-position) t)
    (when (looking-at subed--regexp-timestamp)
      (point))))

(cl-defmethod subed--jump-to-subtitle-text (&context (major-mode subed-vtt-mode) &optional sub-id)
  "Move point on the first character of subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if a the subtitle's text can't be found.
Use the format-specific function for MAJOR-MODE."
  (when (subed-jump-to-subtitle-time-start sub-id)
    (forward-line 1)
    (point)))

(cl-defmethod subed--jump-to-subtitle-end (&context (major-mode subed-vtt-mode) &optional sub-id)
  "Move point after the last character of the subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if point did not change or if no subtitle end
can be found.  Use the format-specific function for MAJOR-MODE."
  (let* ((orig-point (point))
				 (case-fold-search nil)
         text-point)
    (catch 'done
      ;; go back to the text
      (if (subed-in-comment-p)
          (if (re-search-forward (concat subed-vtt--regexp-start-of-line subed-vtt--regexp-timing) nil t)
              (progn
                (unless (eobp) (forward-line 1) (goto-char (line-beginning-position)))
                (setq text-point (point)))
            (throw 'done nil))
        (setq text-point
              (or (subed-jump-to-subtitle-text sub-id)
                  (point))))
      (goto-char (line-beginning-position))

      (cond
       ;; are we in the header?
       ((subed-in-header-p)
        (goto-char orig-point)
        (throw 'done nil))
       ;; are we looking at the next timing?
       ((looking-at (concat "[ \t\n]*" subed-vtt--regexp-timing)) (point))
       ;; are we looking at an ID and then the next timing?
       ((looking-at (format "\\(?:[ \t]*\n\\)+%s\n%s"
                            subed-vtt--regexp-identifier
                            subed-vtt--regexp-timing))
        (point))
       ((re-search-forward (format "\\(%s%s\\)\\|\\(%s%s\n%s\\)\\|\\(%s%s\\)\\|%s.*-->"
                                   subed-vtt--regexp-blank-separator
                                   subed-vtt--regexp-note
                                   subed-vtt--regexp-blank-separator
                                   subed-vtt--regexp-identifier
                                   subed-vtt--regexp-timing
                                   subed-vtt--regexp-start-of-line
                                   subed-vtt--regexp-timing
                                   subed-vtt--regexp-start-of-line)
                           nil t)
        (goto-char (max text-point (match-beginning 0))))
       (t (goto-char (point-max))))
      (skip-chars-backward " \t\n")
      (goto-char (max (point) text-point))
      (unless (= (point) orig-point)
        (point)))))

(cl-defmethod subed--forward-subtitle-id (&context (major-mode subed-vtt-mode))
  "Move point to next subtitle's ID.
Return point or nil if there is no next subtitle.  Use the
format-specific function for MAJOR-MODE."
  (let ((orig-point (point)))
    (when (subed-subtitle-id)
      (subed-jump-to-subtitle-end))
    (if (re-search-forward (format "\\(?:%s\\(%s\n%s\\)\\|%s\\(%s\\)\\)"
                                   subed-vtt--regexp-blank-separator
                                   subed-vtt--regexp-identifier
                                   subed-vtt--regexp-timing
                                   subed-vtt--regexp-start-of-line
                                   subed-vtt--regexp-timing)
                           nil t)
        (goto-char (or (match-beginning 1)
                       (match-beginning 2)))
      (goto-char orig-point)
      nil)))

(cl-defmethod subed--backward-subtitle-id (&context (major-mode subed-vtt-mode))
  "Move point to previous subtitle's ID.
Return point or nil if there is no previous subtitle.  Use the
format-specific function for MAJOR-MODE."
  (let ((orig-point (point)))
    (subed-jump-to-subtitle-id)
    (or
     (catch 'found
       (while (re-search-backward
               (concat "^\\(.*?\\n\\)?"
                       subed--regexp-timestamp
                       " *--> *" subed--regexp-timestamp)
               nil t)
         (when (subed-jump-to-subtitle-id)
           (throw 'found (point)))))
     (progn (goto-char orig-point) nil))))

;;; Manipulation

(defun subed-vtt--format-comment (comment)
  "Return COMMENT formatted for insertion.
If COMMENT starts with NOTE, keep it as is.  If not, add a NOTE header to it.
Make sure COMMENT ends with a blank line."
  (cond ((null comment) "")
        ((string-match "\\`NOTE"
                       (concat comment
                               (if (string-match "\n\n\\'" comment)
                                   "" "\n\n")))
         comment)
        ((string-match "\n" comment) (concat "NOTE\n" comment "\n\n"))
        (t (concat "NOTE " comment "\n\n"))))

(cl-defmethod subed--jump-to-subtitle-comment (&context (major-mode subed-vtt-mode)
                                                        &optional sub-id)
  "Move point to subtitle's comment.
If SUB-ID is not given, use subtitle on point.
Return point, or nil if no comment could be found.
Use the format-specific function for MAJOR-MODE."
  (let ((pos (point)))
    (if (and (subed-jump-to-subtitle-id sub-id)
             (re-search-backward
              (concat subed-vtt--regexp-blank-separator
                      "\\(NOTE[ \t\n]\\)")
              (or (save-excursion (subed-backward-subtitle-end)) (point-min)) t))
        (progn (goto-char (match-beginning 1))
               (point))
      (goto-char pos)
      nil)))

(cl-defmethod subed--subtitle-comment (&context (major-mode subed-vtt-mode)
                                                &optional sub-id)
  "Return subtitle comment or nil if none.
If SUB-ID is not given, use the subtitle on point.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (let ((comment-start (subed-jump-to-subtitle-comment sub-id)))
      (if comment-start
          (string-trim
           (replace-regexp-in-string "^NOTE[ \n]+" ""
                                     (buffer-substring comment-start (subed-jump-to-subtitle-id sub-id))))
        nil))))

(cl-defmethod subed--set-subtitle-comment (comment &context (major-mode subed-vtt-mode)
                                                   &optional sub-id)
  "Set the current subtitle's comment to COMMENT.
If COMMENT is nil or the empty string, remove the comment.
If SUB-ID is not given, use the subtitle on point.
Use the format-specific function for MAJOR-MODE."
  (let ((comment-start (subed-jump-to-subtitle-comment sub-id)))
    ;; remove previous comment
    (if comment-start
        (delete-region comment-start (subed-jump-to-subtitle-id sub-id))
      (subed-jump-to-subtitle-id sub-id))
    (when (and comment (not (string= comment "")))
      (insert (subed-vtt--format-comment comment)))))

(cl-defmethod subed--make-subtitle (&context (major-mode subed-vtt-mode)
                                             &optional _ start stop text comment)
  "Generate new subtitle string.

START defaults to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.
ID is ignored.

A newline is appended to TEXT, meaning you'll get two trailing
newlines if TEXT is nil or empty.  Use the format-specific
function for MAJOR-MODE."
  (format "%s%s --> %s\n%s\n"
          (subed-vtt--format-comment comment)
          (subed-msecs-to-timestamp (or start 0))
          (subed-msecs-to-timestamp (or stop (+ (or start 0)
                                                subed-default-subtitle-length)))
          (or text "")))

(cl-defmethod subed--prepend-subtitle (&context (major-mode subed-vtt-mode) &optional id start stop text comment)
  "Insert new subtitle before the subtitle at point.

ID and START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.

Move point to the text of the inserted subtitle.  Return new
point.  Use the format-specific function for MAJOR-MODE."
  (or (subed-jump-to-subtitle-comment) (subed-jump-to-subtitle-id))
  (insert (subed-make-subtitle id start stop text comment))
  (when (looking-at (concat "\\([[:space:]]*\\|^\\)" subed--regexp-timestamp))
    (insert "\n"))
  (skip-chars-backward " \t\n")
  (subed-jump-to-subtitle-text)
  (point))

(cl-defmethod subed--append-subtitle (&context (major-mode subed-vtt-mode) &optional id start stop text comment)
  "Insert new subtitle after the subtitle at point.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.

Move point to the text of the inserted subtitle.  Return new
point.  Use the format-specific function for MAJOR-MODE."
  (unless (or (subed-forward-subtitle-comment) (subed-forward-subtitle-id))
    ;; Point is on last subtitle or buffer is empty
    (subed-jump-to-subtitle-end)
    (when (looking-at "[[:space:]]+")
      (replace-match ""))
    ;; Moved point to end of last subtitle; ensure separator exists
    (while (not (looking-at "\\(\\`\\|[[:blank:]]*\n[[:blank:]]*\n\\)"))
      (save-excursion (insert ?\n)))
    ;; Move to end of separator
    (goto-char (match-end 0)))
  (insert (subed-make-subtitle id start stop text comment))
  (unless (eolp)
    ;; Complete separator with another newline unless we inserted at the end
    (insert ?\n))
  (forward-line -2)
  (subed-jump-to-subtitle-text))

(cl-defmethod subed--merge-with-next (&context (major-mode subed-vtt-mode))
  "Merge the current subtitle with the next subtitle.
Update the end timestamp accordingly.
Use the format-specific function for MAJOR-MODE."
  (save-excursion
    (subed-jump-to-subtitle-end)
    (let ((pos (point))
          cur-comment
          next-comment
          new-end)
      (if (subed-forward-subtitle-time-stop)
          (progn
            (setq next-comment (subed-subtitle-comment))
            (when (looking-at subed--regexp-timestamp)
              (setq new-end (subed-timestamp-to-msecs (match-string 0))))
            (subed-jump-to-subtitle-text)
            (delete-region pos (point))
            (insert " ")
            (when next-comment
              (setq cur-comment (subed-subtitle-comment))
              (subed-set-subtitle-comment (if cur-comment
                                              (concat cur-comment "\n" next-comment)
                                            next-comment)))
            (let ((subed-enforce-time-boundaries nil))
              (subed-set-subtitle-time-stop new-end))
            (run-hooks 'subed-subtitle-merged-hook))
        (error "No subtitle to merge into")))))

;;; Maintenance


(cl-defmethod subed--sanitize-format (&context (major-mode subed-vtt-mode))
  "Remove surplus newlines and whitespace.
Use the format-specific function for MAJOR-MODE."
  (atomic-change-group
    (subed-save-excursion
     ;; Remove trailing whitespace from each line
     (delete-trailing-whitespace (point-min) (point-max))

     ;; Remove leading spaces and tabs from each line
     (goto-char (point-min))
     (while (re-search-forward "^[[:blank:]]+" nil t)
       (replace-match ""))

     ;; Remove leading newlines
     (goto-char (point-min))
     (while (looking-at "\\`\n+")
       (replace-match ""))

     ;; Replace blank separators between subtitles with double newlines
     (goto-char (point-min))
     (while (subed-forward-subtitle-id)
       (let ((prev-sub-end (save-excursion (when (subed-backward-subtitle-end)
                                             (point)))))
         (when (and prev-sub-end
                    (not (string= (buffer-substring prev-sub-end (point)) "\n\n"))
                    (string-match "\\`\n+\\'" (buffer-substring prev-sub-end (point))))
           (delete-region prev-sub-end (point))
           (insert "\n\n"))))

     ;; Two trailing newline if last subtitle text is empty, one trailing
     ;; newline otherwise; do nothing in empty buffer (no graphical
     ;; characters)
     (goto-char (point-min))
     (when (re-search-forward "[[:graph:]]" nil t)
       (goto-char (point-max))
       (skip-chars-backward " \t\n")
       (subed-jump-to-subtitle-end)
       (unless (looking-at "\n\\'")
         (delete-region (point) (point-max))
         (insert "\n")))

     ;; One space before and after " --> "
     (goto-char (point-min))
     (while (re-search-forward (format "^%s" subed--regexp-timestamp) nil t)
       (when (looking-at "[[:blank:]]*-->[[:blank:]]*")
         (unless (= (length (match-string 0)) 5)
           (replace-match " --> ")))))))

(cl-defmethod subed--validate-format (&context (major-mode subed-vtt-mode))
  "Move point to the first invalid subtitle and report an error.
Use the format-specific function for MAJOR-MODE."
  (when (> (buffer-size) 0)
    (atomic-change-group
      (let ((orig-point (point)))
        (goto-char (point-min))
        (while (subed-forward-subtitle-id)
          ;; This regex is stricter than `subed--regexp-timestamp'
          (unless (looking-at "^\\([0-9]\\{2\\}:\\)?[0-9]\\{2\\}:[0-9]\\{2\\}\\(\\.[0-9]\\{0,3\\}\\)")
            (error "Found invalid start time: %S"  (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
          (when (re-search-forward "[[:blank:]]" (line-end-position) t)
            (goto-char (match-beginning 0)))
          (unless (looking-at " --> ")
            (error "Found invalid separator between start and stop time: %S"
                   (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
          (condition-case nil
              (forward-char 5)
            (error nil))
          (unless (looking-at "\\([0-9]\\{2\\}:\\)?[0-9]\\{2\\}:[0-9]\\{2\\}\\(\\.[0-9]\\{0,3\\}\\)$")
            (error "Found invalid stop time: %S" (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1))))
        (goto-char orig-point)))))

(cl-defmethod subed--auto-insert (&context (major-mode subed-vtt-mode))
  "Set up an empty WebVTT file.
Use the format-specific function for MAJOR-MODE."
  (insert "WEBVTT\n"))

;;;###autoload
(define-derived-mode subed-vtt-mode subed-mode "Subed-VTT"
  "Major mode for editing WebVTT subtitle files."
  (setq-local subed--subtitle-format "vtt")
  (setq-local subed--regexp-timestamp subed-vtt--regexp-timestamp)
  (setq-local subed--regexp-separator subed-vtt--regexp-separator)
  (setq-local font-lock-defaults '(subed-vtt-font-lock-keywords))
  (setq-local syntax-propertize-function
              (syntax-propertize-rules
               ("^\n" (0 ">"))
               ("^\\(N\\)\\(O\\)TE\\_>" (1 "w 1") (2 "w 2"))))
  ;; Support for fill-paragraph (M-q)
  (let ((timestamps-regexp (concat subed--regexp-timestamp
                                   " *--> *"
                                   subed--regexp-timestamp)))
    (setq-local paragraph-separate
                (concat "^\\("
                        (mapconcat #'identity `("[[:blank:]]*"
                                                "[[:digit:]]+"
                                                ,timestamps-regexp)
                                   "\\|")
                        "\\)$"))
    (setq-local paragraph-start
                (concat "\\("
                        ;; Multiple speakers in the same
                        ;; subtitle are often distinguished with
                        ;; a "-" at the start of the line.
                        (mapconcat #'identity '("^-" "[[:graph:]]*$")
                                   "\\|")
                        "\\)"))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vtt\\'" . subed-vtt-mode))

(provide 'subed-vtt)
;;; subed-vtt.el ends here
