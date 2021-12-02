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

(require 'subed-config)
(require 'subed-debug)
(require 'subed-common)

;;; Syntax highlighting

(defconst subed-ass-font-lock-keywords
  (list
   '("\\([0-9]+:\\)?[0-9]+:[0-9]+\\.[0-9]+" . 'subed-ass-time-face)
   '(",[0-9]+ \\(-->\\) [0-9]+:" 1 'subed-ass-time-separator-face t)
   '("^.*$" . 'subed-ass-text-face))
  "Highlighting expressions for `subed-mode'.")


;;; Parsing

(defconst subed-ass--regexp-timestamp "\\(\\([0-9]+\\):\\)?\\([0-9]+\\):\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?")
(defconst subed-ass--regexp-start "\\(?:Dialogue\\|Comment\\|Picture\\|Sound\\|Movie\\|Command\\): [0-9]+,")
(defconst subed-ass--regexp-separator "\n")

(defun subed-ass--timestamp-to-msecs (time-string)
  "Find HH:MM:SS.MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern."
  (save-match-data
    (when (string-match subed-ass--regexp-timestamp time-string)
      (let ((hours (string-to-number (or (match-string 2 time-string) "0")))
            (mins  (string-to-number (match-string 3 time-string)))
            (secs  (string-to-number (match-string 4 time-string)))
            (msecs (if (match-string 6 time-string) (string-to-number (subed--right-pad (match-string 6 time-string) 3 ?0)) 0)))
        (+ (* (truncate hours) 3600000)
           (* (truncate mins) 60000)
           (* (truncate secs) 1000)
           (truncate msecs))))))

(defun subed-ass--msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format H:MM:SS.CS."
  ;; We need to wrap format-seconds in save-match-data because it does regexp
  ;; stuff and we need to preserve our own match-data.
  (concat (save-match-data (format-seconds "%h:%02m:%02s" (/ msecs 1000)))
          "." (format "%02d" (/ (mod msecs 1000) 10))))

(defun subed-ass--subtitle-id ()
  "Return the ID of the subtitle at point or nil if there is no ID."
  (save-excursion
    (when (subed-ass--jump-to-subtitle-id)
      (when (looking-at subed-ass--regexp-timestamp)
        (match-string 0)))))

(defun subed-ass--subtitle-id-max ()
  "Return the ID of the last subtitle or nil if there are no subtitles."
  (save-excursion
    (goto-char (point-max))
    (subed-ass--subtitle-id)))

(defun subed-ass--subtitle-id-at-msecs (msecs)
  "Return the ID of the subtitle at MSECS milliseconds.
Return nil if there is no subtitle at MSECS."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let* ((secs       (/ msecs 1000))
             (only-hours (truncate (/ secs 3600)))
             (only-mins  (truncate (/ (- secs (* only-hours 3600)) 60))))
        ;; Move to first subtitle in the relevant hour
        (when (re-search-forward (format "\\(%s\\|\\`\\)%02d:" subed-ass--regexp-separator only-hours) nil t)
          (beginning-of-line)
          ;; Move to first subtitle in the relevant hour and minute
          (re-search-forward (format "\\(\n\n\\|\\`\\)%02d:%02d" only-hours only-mins) nil t)))
      ;; Move to first subtitle that starts at or after MSECS
      (catch 'subtitle-id
        (while (<= (or (subed-ass--subtitle-msecs-start) -1) msecs)
          ;; If stop time is >= MSECS, we found a match
          (let ((cur-sub-end (subed-ass--subtitle-msecs-stop)))
            (when (and cur-sub-end (>= cur-sub-end msecs))
              (throw 'subtitle-id (subed-ass--subtitle-id))))
          (unless (subed-ass--forward-subtitle-id)
            (throw 'subtitle-id nil)))))))

(defun subed-ass--subtitle-msecs-start (&optional sub-id)
  "Subtitle start time in milliseconds or nil if it can't be found.
If SUB-ID is not given, use subtitle on point."
  (let ((timestamp (save-excursion
                     (when (subed-ass--jump-to-subtitle-time-start sub-id)
                       (when (looking-at subed-ass--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-ass--timestamp-to-msecs timestamp))))

(defun subed-ass--subtitle-msecs-stop (&optional sub-id)
  "Subtitle stop time in milliseconds or nil if it can't be found.
If SUB-ID is not given, use subtitle on point."
  (let ((timestamp (save-excursion
                     (when (subed-ass--jump-to-subtitle-time-stop sub-id)
                       (when (looking-at subed-ass--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-ass--timestamp-to-msecs timestamp))))

(defun subed-ass--subtitle-text (&optional sub-id)
  "Return subtitle's text or an empty string.
If SUB-ID is not given, use subtitle on point."
  (or (save-excursion
        (let ((beg (subed-ass--jump-to-subtitle-text sub-id))
              (end (subed-ass--jump-to-subtitle-end sub-id)))
          (when (and beg end)
            (buffer-substring beg end))))
      ""))

(defun subed-ass--subtitle-relative-point ()
  "Point relative to subtitle's ID or nil if ID can't be found."
  (let ((start-point (save-excursion
                       (when (subed-ass--jump-to-subtitle-id)
                         (point)))))
    (when start-point
      (- (point) start-point))))

;;; Traversing

(defun subed-ass--jump-to-subtitle-id (&optional sub-id)
  "Move to the ID of a subtitle and return point.
If SUB-ID is not given, focus the current subtitle's ID.
Return point or nil if no subtitle ID could be found.
ASS doesn't use IDs, so we use the starting timestamp instead."
  (interactive)
  (save-match-data
    (if (stringp sub-id)
        (let* ((orig-point (point))
               (find-ms (subed-ass--timestamp-to-msecs sub-id))
               (regex (concat "^\\(?:" subed-ass--regexp-start "\\)\\(" subed-ass--regexp-timestamp "\\)"))
               done)
          (goto-char (point-min))
          (while (not done)
            (if (re-search-forward regex nil t)
                (when (= (subed-ass--timestamp-to-msecs (match-string 1)) find-ms)
                  (setq done 'found)
                  (goto-char (match-beginning 1)))
              (setq done 'not-found)
              (goto-char orig-point)))
          (when (eq done 'found)
            (beginning-of-line)
            (point)))
      (end-of-line)
      (let* ((regex (concat "^\\(?:" subed-ass--regexp-start "\\)\\(" subed-ass--regexp-timestamp "\\)"))
             (match-found (re-search-backward regex nil t)))
        (when (or match-found (re-search-forward regex nil t))           ;; maybe at the beginning?
          (goto-char (match-beginning 0))
          (point))))))

(defun subed-ass--jump-to-subtitle-id-at-msecs (msecs)
  "Move point to the ID of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-ass--subtitle-id-at-msecs'."
  (let ((current-sub-id (subed-ass--subtitle-id))
        (target-sub-id (subed-ass--subtitle-id-at-msecs msecs)))
    (when (and target-sub-id current-sub-id (not (equal target-sub-id current-sub-id)))
      (subed-ass--jump-to-subtitle-id target-sub-id))))

(defun subed-ass--jump-to-subtitle-text-at-msecs (msecs)
  "Move point to the text of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-ass--subtitle-id-at-msecs'."
  (when (subed-ass--jump-to-subtitle-id-at-msecs msecs)
    (subed-ass--jump-to-subtitle-text)))

(defun subed-ass--jump-to-subtitle-time-start (&optional sub-id)
  "Move point to subtitle's start time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no start time could be found."
  (interactive)
  (save-match-data
    (when (subed-ass--jump-to-subtitle-id sub-id)
      (when (re-search-forward subed-ass--regexp-timestamp (line-end-position) t)
        (goto-char (match-beginning 0))
        (point)))))

(defun subed-ass--jump-to-subtitle-time-stop (&optional sub-id)
  "Move point to subtitle's stop time.
If SUB-ID is not given, use subtitle on point.
Return point or nil if no stop time could be found."
  (interactive)
  (save-match-data
    (when (subed-ass--jump-to-subtitle-id sub-id)
      (re-search-forward (concat "\\(?:" subed-ass--regexp-timestamp "\\),") (point-at-eol) t)
      (when (looking-at subed-ass--regexp-timestamp)
        (point)))))

(defun subed-ass--jump-to-subtitle-text (&optional sub-id)
  "Move point on the first character of subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if a the subtitle's text can't be found."
  (interactive)
  (when (subed-ass--jump-to-subtitle-id sub-id)
    (beginning-of-line)
    (when (looking-at ".*?,.*?,.*?,.*?,.*?,.*?,.*?,.*?,.*?,")
      (goto-char (match-end 0)))
    (point)))

(defun subed-ass--jump-to-subtitle-end (&optional sub-id)
  "Move point after the last character of the subtitle's text.
If SUB-ID is not given, use subtitle on point.
Return point or nil if point did not change or if no subtitle end
can be found."
  (interactive)
  (save-match-data
    (let ((orig-point (point)))
      (when (subed-ass--jump-to-subtitle-text sub-id)
        (end-of-line)
        (unless (= orig-point (point))
          (point))))))

(defun subed-ass--forward-subtitle-id ()
  "Move point to next subtitle's ID.
Return point or nil if there is no next subtitle."
  (interactive)
  (save-match-data
    (let ((pos (point)))
      (forward-line 1)
      (beginning-of-line)
      (while (not (or (eobp) (looking-at subed-ass--regexp-start)))
        (forward-line 1))
      (if (looking-at subed-ass--regexp-start)
          (point)
        (goto-char pos)
        nil))))

(defun subed-ass--backward-subtitle-id ()
  "Move point to previous subtitle's ID.
Return point or nil if there is no previous subtitle."
  (interactive)
  (let ((orig-point (point)))
    (when (subed-ass--jump-to-subtitle-id)
      (forward-line -1)
      (while (not (or (bobp) (looking-at subed-ass--regexp-start)))
        (forward-line -1))
      (if (looking-at subed-ass--regexp-start)
          (point)
        (goto-char orig-point)
        nil))))

(defun subed-ass--forward-subtitle-text ()
  "Move point to next subtitle's text.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-ass--forward-subtitle-id)
    (subed-ass--jump-to-subtitle-text)))

(defun subed-ass--backward-subtitle-text ()
  "Move point to previous subtitle's text.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-ass--backward-subtitle-id)
    (subed-ass--jump-to-subtitle-text)))

(defun subed-ass--forward-subtitle-end ()
  "Move point to end of next subtitle.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-ass--forward-subtitle-id)
    (subed-ass--jump-to-subtitle-end)))

(defun subed-ass--backward-subtitle-end ()
  "Move point to end of previous subtitle.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-ass--backward-subtitle-id)
    (subed-ass--jump-to-subtitle-end)))

(defun subed-ass--forward-subtitle-time-start ()
  "Move point to next subtitle's start time."
  (interactive)
  (when (subed-ass--forward-subtitle-id)
    (subed-ass--jump-to-subtitle-time-start)))

(defun subed-ass--backward-subtitle-time-start ()
  "Move point to previous subtitle's start time."
  (interactive)
  (when (subed-ass--backward-subtitle-id)
    (subed-ass--jump-to-subtitle-time-start)))

(defun subed-ass--forward-subtitle-time-stop ()
  "Move point to next subtitle's stop time."
  (interactive)
  (when (subed-ass--forward-subtitle-id)
    (subed-ass--jump-to-subtitle-time-stop)))

(defun subed-ass--backward-subtitle-time-stop ()
  "Move point to previous subtitle's stop time."
  (interactive)
  (when (subed-ass--backward-subtitle-id)
    (subed-ass--jump-to-subtitle-time-stop)))

;;; Manipulation

(defun subed-ass--set-subtitle-time-start (msecs &optional sub-id)
  "Set subtitle start time to MSECS milliseconds.

If SUB-ID is not given, set the start of the current subtitle.

Return the new subtitle start time in milliseconds."
  (save-excursion
    (when (or (not sub-id)
              (and sub-id (subed-ass--jump-to-subtitle-id sub-id)))
      (subed-ass--jump-to-subtitle-time-start)
      (when (looking-at subed-ass--regexp-timestamp)
        (replace-match (subed-ass--msecs-to-timestamp msecs))))))

(defun subed-ass--set-subtitle-time-stop (msecs &optional sub-id)
  "Set subtitle stop time to MSECS milliseconds.

If SUB-ID is not given, set the stop of the current subtitle.

Return the new subtitle stop time in milliseconds."
  (save-excursion
    (when (or (not sub-id)
              (and sub-id (subed-ass--jump-to-subtitle-id sub-id)))
      (subed-ass--jump-to-subtitle-time-stop)
      (when (looking-at subed-ass--regexp-timestamp)
        (replace-match (subed-ass--msecs-to-timestamp msecs))))))

(defun subed-ass--make-subtitle (&optional id start stop text)
  "Generate new subtitle string.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.

A newline is appended to TEXT, meaning you'll get two trailing
newlines if TEXT is nil or empty."
  (interactive "P")
  (format "Dialogue: 0,%s,%s,Default,,0,0,0,,%s\n"
          (subed-ass--msecs-to-timestamp (or start 0))
          (subed-ass--msecs-to-timestamp (or stop (+ (or start 0)
                                                     subed-default-subtitle-length)))
          (replace-regexp-in-string "\n" "\\n" (or text ""))))

(defun subed-ass--prepend-subtitle (&optional id start stop text)
  "Insert new subtitle before the subtitle at point.

ID and START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.

Move point to the text of the inserted subtitle.
Return new point."
  (interactive "P")
  (subed-ass--jump-to-subtitle-id)
  (insert (subed-ass--make-subtitle id start stop text))
  (forward-line -1)
  (subed-ass--jump-to-subtitle-text))

(defun subed-ass--append-subtitle (&optional id start stop text)
  "Insert new subtitle after the subtitle at point.

ID, START default to 0.
STOP defaults to (+ START `subed-subtitle-spacing')
TEXT defaults to an empty string.

Move point to the text of the inserted subtitle.
Return new point."
  (interactive "P")
  (unless (subed-ass--forward-subtitle-id)
    ;; Point is on last subtitle or buffer is empty
    (subed-ass--jump-to-subtitle-end)
    (unless (bolp) (insert "\n")))
  (insert (subed-ass--make-subtitle id start stop text))
  (forward-line -1)
  (subed-ass--jump-to-subtitle-text))

(defun subed-ass--kill-subtitle ()
  "Remove subtitle at point."
  (interactive)
  (let ((beg (save-excursion (subed-ass--jump-to-subtitle-id)
                             (point)))
        (end (save-excursion (subed-ass--jump-to-subtitle-id)
                             (when (subed-ass--forward-subtitle-id)
                               (point)))))
    (if (not end)
        ;; Removing the last subtitle because forward-subtitle-id returned nil
        (setq beg (save-excursion (goto-char beg)
                                  (subed-ass--backward-subtitle-end)
                                  (1+ (point)))
              end (save-excursion (goto-char (point-max)))))
    (delete-region beg end)))

(defun subed-ass--merge-with-next ()
  "Merge the current subtitle with the next subtitle.
Update the end timestamp accordingly."
  (interactive)
  (save-excursion
    (subed-ass--jump-to-subtitle-end)
    (let ((pos (point)) new-end)
      (if (subed-ass--forward-subtitle-time-stop)
          (progn
            (when (looking-at subed-ass--regexp-timestamp)
              (setq new-end (subed-ass--timestamp-to-msecs (match-string 0))))
            (subed-ass--jump-to-subtitle-text)
            (delete-region pos (point))
            (insert " ")
            (subed-ass--set-subtitle-time-stop new-end))
        (error "No subtitle to merge into")))))


;;; Maintenance

(defun subed-ass--regenerate-ids ()
  "Not applicable to ASS."
  (interactive))

(defvar-local subed-ass--regenerate-ids-soon-timer nil)
(defun subed-ass--regenerate-ids-soon ()
  "Not applicable to ASS."
  (interactive))

(defun subed-ass--sanitize ()
  "Not yet implemented."
  (interactive))

(defun subed-ass--validate ()
  "Not yet implemented."
  (interactive))

(defun subed-ass--sort ()
  "Not yet implemented."
  (interactive))

(defun subed-ass--init ()
  "This function is called when subed-mode is entered for a ASS file."
  (setq-local subed--subtitle-format "ass")
  (setq-local font-lock-defaults '(subed-ass-font-lock-keywords)))

(provide 'subed-ass)
;;; subed-ass.el ends here
