;;; subed-srt.el --- SubRip/srt implementation for subed  -*- lexical-binding: t; -*-

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

;;; Syntax highlighting

(defconst subed-srt-font-lock-keywords
  (list
   '("^[0-9]+$" . 'subed-srt-id-face)
   '("[0-9]+:[0-9]+:[0-9]+,[0-9]+" . 'subed-srt-time-face)
   '(",[0-9]+ \\(-->\\) [0-9]+:" 1 'subed-srt-time-separator-face t)
   '("^.*$" . 'subed-srt-text-face))
  "Highlighting expressions for subed-mode")


;;; Parsing

(defconst subed-srt--regexp-timestamp "\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\),\\([0-9]+\\)")
(defconst subed-srt--regexp-separator "\\([[:blank:]]*\n\\)+[[:blank:]]*\n")

(defun subed-srt--timestamp-to-msecs (time-string)
  "Find HH:MM:SS,MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern."
  (save-match-data
    (when (string-match subed-srt--regexp-timestamp time-string)
      (let ((hours (string-to-number (match-string 1 time-string)))
            (mins  (string-to-number (match-string 2 time-string)))
            (secs  (string-to-number (match-string 3 time-string)))
            (msecs (string-to-number (subed--right-pad (match-string 4 time-string) 3 ?0))))
        (+ (* (truncate hours) 3600000)
           (* (truncate mins) 60000)
           (* (truncate secs) 1000)
           (truncate msecs))))))

(defun subed-srt--msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS,MS."
  (save-match-data
    (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
            "," (format "%03d" (mod msecs 1000)))))

(defun subed-srt--subtitle-id ()
  "Return the ID of subtitle at point or nil if there is no ID."
  (save-excursion
    (when (subed-srt-jump-to-subtitle-id)
      (string-to-number (current-word)))))

(defun subed-srt--subtitle-id-at-msecs (msecs)
  "Return the ID of the subtitle at MSECS milliseconds.
If MSECS is between subtitles, return the subtitle that starts
after MSECS if there is one and its start time is >= MSECS +
1000.  Otherwise return the closest subtitle before MSECS."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (let* ((secs       (/ msecs 1000))
             (only-hours (truncate (/ secs 3600)))
             (only-mins  (truncate (/ (- secs (* only-hours 3600)) 60))))
        ;; Move to first subtitle in the relevant hour
        (when (re-search-forward (format "\\(%s\\|\\`\\)[0-9]+\n%02d:" subed-srt--regexp-separator only-hours) nil t)
          (beginning-of-line)
          ;; Move to first subtitle in the relevant hour and minute
          (re-search-forward (format "\\(\n\n\\|\\`\\)[0-9]+\n%02d:%02d" only-hours only-mins) nil t)))
      ;; Move to first subtitle that starts at or after MSECS
      (catch 'last-subtitle-reached
        (while (<= (or (subed-srt--subtitle-msecs-start) -1) msecs)
          (unless (subed-srt-forward-subtitle-id)
            (throw 'last-subtitle-reached nil))))
      ;; Move back to previous subtitle if start of current subtitle is in the
      ;; future (i.e. MSECS is between subtitles)
      (when (> (or (subed-srt--subtitle-msecs-start) -1) msecs)
        (subed-srt-backward-subtitle-id))
      (subed-srt--subtitle-id))))

(defun subed-srt--subtitle-msecs-start (&optional sub-id)
  "Subtitle start time in milliseconds or nil if it can't be found."
  (let ((timestamp (save-excursion
                     (when (subed-srt-jump-to-subtitle-time-start sub-id)
                       (when (looking-at subed-srt--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-srt--timestamp-to-msecs timestamp))))

(defun subed-srt--subtitle-msecs-stop (&optional sub-id)
  "Subtitle stop time in milliseconds or nil if it can't be found."
  (let ((timestamp (save-excursion
                     (when (subed-srt-jump-to-subtitle-time-stop sub-id)
                       (when (looking-at subed-srt--regexp-timestamp)
                         (match-string 0))))))
    (when timestamp
      (subed-srt--timestamp-to-msecs timestamp))))

(defun subed-srt--subtitle-text (&optional sub-id)
  "Return subtitle's text."
  (or (save-excursion
        (let ((beg (subed-srt-jump-to-subtitle-text sub-id))
              (end (subed-srt-jump-to-subtitle-end sub-id)))
          (when (and beg end)
            (buffer-substring beg end)))) ""))

(defun subed-srt--subtitle-relative-point ()
  "Point relative to subtitle's ID or nil if ID can't be found."
  (let ((start-point (save-excursion
                       (when (subed-srt-jump-to-subtitle-id)
                         (point)))))
    (when start-point
      (- (point) start-point))))


;;; Traversing

(defun subed-srt-jump-to-subtitle-id (&optional sub-id)
  "Move to the ID of a subtitle and return point.
If SUBTITLE-ID is not given, focus the current subtitle's ID.
Return point or nil if no subtitle ID could be found."
  (interactive)
  (save-match-data
    (if sub-id
        (progn
          ;; Look for a line that contains only the ID, preceded by one or more
          ;; blank lines or the beginning of the buffer.
          (let* ((orig-point (point))
                 (regex (format "\\(%s\\|\\`\\)\\(%d\\)$" subed-srt--regexp-separator sub-id))
                 (match-found (progn (goto-char (point-min))
                                     (re-search-forward regex nil t))))
            (goto-char orig-point)
            (when match-found
              (goto-char (match-beginning 3)))))
      (progn
        ;; Find one or more blank lines.
        (re-search-forward "\\([[:blank:]]*\n\\)+" nil t)
        ;; Find two or more blank lines or the beginning of the buffer, followed
        ;; by line composed of only digits.
        (let* ((regex (concat "\\(" subed-srt--regexp-separator "\\|\\`\\)\\([0-9]+\\)$"))
               (match-found (re-search-backward regex nil t)))
          (when match-found
            (goto-char (match-beginning 3))))))
    ;; Make extra sure we're on an ID, return nil if we're not
    (when (looking-at "^\\([0-9]+\\)$")
      (point))))

(defun subed-srt-jump-to-subtitle-id-at-msecs (msecs)
  "Move point to the ID of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-srt--subtitle-id-at-msecs'."
  (let ((current-sub-id (subed-srt--subtitle-id))
        (target-sub-id (subed-srt--subtitle-id-at-msecs msecs)))
    (when (and target-sub-id current-sub-id (not (= target-sub-id current-sub-id)))
      (subed-srt-jump-to-subtitle-id target-sub-id))))

(defun subed-srt-jump-to-subtitle-time-start (&optional sub-id)
  "Move point to subtitle's start time.
Return point or nil if no start time could be found."
  (interactive)
  (save-match-data
    (when (subed-srt-jump-to-subtitle-id sub-id)
      (forward-line)
      (when (looking-at subed-srt--regexp-timestamp)
        (point)))))

(defun subed-srt-jump-to-subtitle-time-stop (&optional sub-id)
  "Move point to subtitle's stop time.
Return point or nil if no stop time could be found."
  (interactive)
  (save-match-data
    (when (subed-srt-jump-to-subtitle-id sub-id)
      (forward-line 1)
      (re-search-forward " *--> *" (point-at-eol) t)
      (when (looking-at subed-srt--regexp-timestamp)
        (point)))))

(defun subed-srt-jump-to-subtitle-text (&optional sub-id)
  "Move point on the first character of subtitle's text.
Return point."
  (interactive)
  (when (subed-srt-jump-to-subtitle-id sub-id)
    (forward-line 2)
    (point)))

(defun subed-srt-jump-to-subtitle-text-at-msecs (msecs)
  "Move point to the text of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-srt--subtitle-id-at-msecs'."
  (when (subed-srt-jump-to-subtitle-id-at-msecs msecs)
    (subed-srt-jump-to-subtitle-text)))

(defun subed-srt-jump-to-subtitle-end (&optional sub-id)
  "Move point after the last character of the subtitle's text.
Return point or nil if point did not change or if no subtitle end
can be found."
  (interactive)
  (save-match-data
    (let ((orig-point (point)))
      (subed-srt-jump-to-subtitle-text sub-id)
      ;; Look for next separator or end of buffer.  We can't use
      ;; `subed-srt--regexp-separator' here because if subtitle text is empty,
      ;; it may be the only empty line in the separator, i.e. there's only one
      ;; "\n".
      (let ((regex (concat "\\([[:blank:]]*\n+[0-9]+\n\\|\\([[:blank:]]*\n*\\)\\'\\)")))
        (when (re-search-forward regex nil t)
          (goto-char (match-beginning 0))))
      (unless (= (point) orig-point)
        (point)))))

(defun subed-srt-forward-subtitle-id ()
  "Move point to next subtitle's ID.
Return point or nil if there is no next subtitle."
  (interactive)
  (save-match-data
    (when (re-search-forward (concat subed-srt--regexp-separator "[0-9]+\n") nil t)
      (subed-srt-jump-to-subtitle-id))))

(defun subed-srt-backward-subtitle-id ()
  "Move point to previous subtitle's ID.
Return point or nil if there is no previous subtitle."
  (interactive)
  (let ((orig-point (point))
        (orig-sub-id (subed-srt--subtitle-id)))
    (when (subed-srt-jump-to-subtitle-id)
      (forward-line -1)
      (if (= (subed-srt--subtitle-id) orig-sub-id)
          (progn (goto-char orig-point)
                 nil)
        (subed-srt-jump-to-subtitle-id)))))

(defun subed-srt-forward-subtitle-text ()
  "Move point to next subtitle's text.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-srt-forward-subtitle-id)
    (subed-srt-jump-to-subtitle-text)))

(defun subed-srt-backward-subtitle-text ()
  "Move point to previous subtitle's text.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-srt-backward-subtitle-id)
    (subed-srt-jump-to-subtitle-text)))

(defun subed-srt-forward-subtitle-end ()
  "Move point to end of next subtitle.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-srt-forward-subtitle-id)
    (subed-srt-jump-to-subtitle-end)))

(defun subed-srt-backward-subtitle-end ()
  "Move point to end of previous subtitle.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-srt-backward-subtitle-id)
    (subed-srt-jump-to-subtitle-end)))

(defun subed-srt-forward-subtitle-time-start ()
  "Move point to next subtitle's start time."
  (interactive)
  (when (subed-srt-forward-subtitle-id)
    (subed-srt-jump-to-subtitle-time-start)))

(defun subed-srt-backward-subtitle-time-start ()
  "Move point to previous subtitle's start time."
  (interactive)
  (when (subed-srt-backward-subtitle-id)
    (subed-srt-jump-to-subtitle-time-start)))

(defun subed-srt-forward-subtitle-time-stop ()
  "Move point to next subtitle's stop time."
  (interactive)
  (when (subed-srt-forward-subtitle-id)
    (subed-srt-jump-to-subtitle-time-stop)))

(defun subed-srt-backward-subtitle-time-stop ()
  "Move point to previous subtitle's stop time."
  (interactive)
  (when (subed-srt-backward-subtitle-id)
    (subed-srt-jump-to-subtitle-time-stop)))


;;; Manipulation

(defun subed-srt--adjust-subtitle-start-relative (msecs)
  "Add MSECS milliseconds to start time (use negative value to subtract)."
  (let* ((msecs-start (subed-srt--subtitle-msecs-start))
         (msecs-new (when msecs-start (+ msecs-start msecs))))
    (when msecs-new
      (save-excursion
        (subed-srt-jump-to-subtitle-time-start)
        (when (looking-at subed-srt--regexp-timestamp)
          (replace-match (subed-srt--msecs-to-timestamp msecs-new))))
      (subed-srt--run-subtitle-time-adjusted-hook))))

(defun subed-srt--adjust-subtitle-stop-relative (msecs)
  "Add MSECS milliseconds to stop time (use negative value to subtract)."
  (let* ((msecs-stop (subed-srt--subtitle-msecs-stop))
         (msecs-new (when msecs-stop (+ msecs-stop msecs))))
    (when msecs-new
      (save-excursion
        (subed-srt-jump-to-subtitle-time-stop)
        (when (looking-at subed-srt--regexp-timestamp)
          (replace-match (subed-srt--msecs-to-timestamp msecs-new))))
      (subed-srt--run-subtitle-time-adjusted-hook))))

(defun subed-srt--run-subtitle-time-adjusted-hook ()
  "Run `subed-subtitle-time-adjusted-hook' functions with
subtitle ID and start time of the adjusted subtitle."
  (when subed-subtitle-time-adjusted-hook
    (run-hook-with-args 'subed-subtitle-time-adjusted-hook
                        (subed-srt--subtitle-id)
                        (subed-srt--subtitle-msecs-start))))

(defun subed-srt-increase-start-time (&optional arg)
  "Add `subed-milliseconds-adjust' milliseconds to start time of current subtitle.

If a prefix argument is given, it is used to set
`subed-milliseconds-adjust' before moving subtitles.  If the
prefix argument is given but not numerical,
`subed-milliseconds-adjust' is reset to its default value.

Example usage:
  \\[universal-argument] 1000 \\[subed-increase-start-time]  Increase start time by 1000ms
           \\[subed-increase-start-time]  Increase start time by 1000ms again
   \\[universal-argument] 500 \\[subed-increase-start-time]  Increase start time by 500ms
           \\[subed-increase-start-time]  Increase start time by 500ms again
       \\[universal-argument] \\[subed-increase-start-time]  Increase start time by 100ms (the default)
           \\[subed-increase-start-time]  Increase start time by 100ms (the default) again"
  (interactive "P")
  (subed-srt--adjust-subtitle-start-relative (subed--get-milliseconds-adjust arg)))

(defun subed-srt-decrease-start-time (&optional arg)
  "Subtract `subed-milliseconds-adjust' milliseconds from start time of current subtitle.

See also `subed-increase-start-time'."
  (interactive "P")
  (subed-srt--adjust-subtitle-start-relative (* -1 (subed--get-milliseconds-adjust arg))))

(defun subed-srt-increase-stop-time (&optional arg)
  "Add `subed-milliseconds-adjust' milliseconds to stop time of current subtitle.

See also `subed-increase-start-time'."
  (interactive "P")
  (subed-srt--adjust-subtitle-stop-relative (subed--get-milliseconds-adjust arg)))

(defun subed-srt-decrease-stop-time (&optional arg)
  "Subtract `subed-milliseconds-adjust' milliseconds from stop time of current subtitle.

See also `subed-increase-start-time'."
  (interactive "P")
  (subed-srt--adjust-subtitle-stop-relative (* -1 (subed--get-milliseconds-adjust arg))))

(defun subed-srt-subtitle-insert (&optional arg)
  "Insert subtitle(s).
`universal-argument' is used in the following manner:
          \\[subed-subtitle-insert]   Insert 1 subtitle after the current subtitle
    \\[universal-argument] - \\[subed-subtitle-insert]   Insert 1 subtitle before the current subtitle
    \\[universal-argument] 5 \\[subed-subtitle-insert]   Insert 5 subtitles after the current subtitle
  \\[universal-argument] - 5 \\[subed-subtitle-insert]   Insert 5 subtitles before the current subtitle
      \\[universal-argument] \\[subed-subtitle-insert]   Insert 1 subtitle before the current subtitle
  \\[universal-argument] \\[universal-argument] \\[subed-subtitle-insert]   Insert 2 subtitles before the current subtitle"
  (interactive "P")
  (save-match-data
    (let* ((number-of-subs (cond ((eq arg nil) 1)      ;; M-i
                                 ((integerp arg) arg)  ;; C-u N M-i  /  C-u - N M-i
                                 ;; C-u [C-u ...] M-i  /  C-u - [C-u ...] M-i
                                 ((consp arg) (* (truncate (log (abs (car arg)) 4)) ;; ([-]64) -> 3
                                                 (/ (car arg) (abs (car arg)))))    ;; Restore sign
                                 (t 1)))            ;; C-u - M-i (Is there anything else is left?)
           (insert-before (or (< number-of-subs 0)  ;; C-u - N M-i
                              (eq arg '-)           ;; C-u - M-i
                              (consp arg)))         ;; C-u [C-u ...] M-i
           ;; Ensure number-of-subs is positive, now that we figured out `insert-before'
           (number-of-subs (abs number-of-subs)))
      (subed-debug "Inserting %s subtitle(s) %s the current" number-of-subs (if insert-before "before" "after"))
      (subed-srt-jump-to-subtitle-id)
      ;; Move to the ID of the subtitle we're prepending subtitles to so that we
      ;; can do (insert "<new subtitle>")
      (if insert-before
          (subed-srt-jump-to-subtitle-id)
        (when (and (not (subed-srt-forward-subtitle-id)) ;; Appending after last subtitle
                   (> (buffer-size) 0))                  ;; Buffer is not empty
          ;; There is no ID because we're appending to the last subtitle.  We just
          ;; have to make sure there is a subtitle delimiter ("\n\n") after the
          ;; last subtitle and point is where the new ID will go.
          (subed-srt-jump-to-subtitle-end)
          (forward-line)
          (insert "\n")))
      ;; Insert subtitles
      (save-excursion
        ;; Find out how much time we have per subtitle
        (let*
            ;; nil when there's no previous subtitle
            ((prev-stop-msecs (save-excursion
                                (if (looking-at "^[0-9]$")
                                    ;; We're inserting between subtitles or
                                    ;; before the first one
                                    (when (subed-srt-backward-subtitle-id)
                                           (subed-srt--subtitle-msecs-stop))
                                  ;; We're append after the last subtitle
                                  (subed-srt--subtitle-msecs-stop))))
             ;; nil when there's no next subtitle
             (next-start-msecs (when (looking-at "^[0-9]$")
                                 (subed-srt--subtitle-msecs-start)))
             ;; nil when there's no next subtitle
             (available-msecs (when next-start-msecs
                                (- next-start-msecs (or prev-stop-msecs 0))))
             ;; Calculate milliseconds per inserted subtitle or use default value
             ;; if we're appending to the last subtitle
             (sub-msecs (if available-msecs (/ available-msecs number-of-subs)
                          (* subed-default-subtitle-length 1000))))
          (dotimes (i number-of-subs)
            (let* ((start-msecs (+ (or prev-stop-msecs 0) (* sub-msecs i)))
                   (stop-msecs (+ start-msecs sub-msecs))
                   ;; Apply `subed-subtitle-spacing'
                   (start-msecs-spaced (if (= i 0)
                                           (+ start-msecs subed-subtitle-spacing)
                                         (+ start-msecs (/ subed-subtitle-spacing 2))))
                   (stop-msecs-spaced (if (= i (1- number-of-subs))
                                          (- stop-msecs subed-subtitle-spacing)
                                        (- stop-msecs (/ subed-subtitle-spacing 2)))))
              (insert (format "0\n%s --> %s\n\n\n"
                              (subed-srt--msecs-to-timestamp start-msecs-spaced)
                              (subed-srt--msecs-to-timestamp stop-msecs-spaced))))))
        ;; If we're not on an ID, that means we added one or more subtitles after
        ;; the last one and we can remove the trailing extra newline
        (when (looking-at "^[[:blank:]]*$")
          (forward-line -1)
          (kill-whole-line)))
      (subed-srt-regenerate-ids)
      (subed-srt-jump-to-subtitle-text))))

(defun subed-srt-subtitle-kill ()
  "Remove subtitle at point."
  (interactive)
  (let ((beg (save-excursion
               (subed-srt-jump-to-subtitle-id)
               (point)))
        (end (save-excursion
               (subed-srt-jump-to-subtitle-id)
               (when (subed-srt-forward-subtitle-id)
                 (point)))))
    (if (not end)
        (progn
          (let ((beg (save-excursion
                       (goto-char beg)
                       (subed-srt-backward-subtitle-text)
                       (subed-srt-jump-to-subtitle-end)
                       (1+ (point))))
                (end (save-excursion
                       (goto-char (point-max)))))
            (delete-region beg end)))
      (progn
        (delete-region beg end)))))


;;; Maintenance

(defun subed-srt-regenerate-ids ()
  "Ensure subtitle IDs start at 1 and are incremented by 1 for
each subtitle."
  (interactive)
  (atomic-change-group
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (subed-srt-jump-to-subtitle-id)
        (unless (string= (current-word) "1")
          (kill-word 1)
          (insert "1"))
        (let ((id 2))
          (while (subed-srt-forward-subtitle-id)
            (let ((id-str (number-to-string id)))
              (unless (string= (current-word) id-str)
                (kill-word 1)
                (insert id-str)))
            (setq id (1+ id))))))))

(defun subed-srt-sanitize ()
  "Remove surplus newlines and whitespace."
  (interactive)
  (atomic-change-group
    (save-match-data
      (subed--save-excursion
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

       ;; Replace separators between subtitles with double newlines
       (goto-char (point-min))
       (while (subed-srt-forward-subtitle-id)
         (let ((prev-sub-end (save-excursion (when (subed-srt-backward-subtitle-end)
                                               (point)))))
           (when (and prev-sub-end
                      (not (string= (buffer-substring prev-sub-end (point)) "\n\n")))
             (delete-region prev-sub-end (point))
             (insert "\n\n"))))

       ;; Two trailing newline if last subtitle text is empty,
       ;; one trailing newline otherwise
       (goto-char (point-max))
       (subed-srt-jump-to-subtitle-end)
       (unless (looking-at "\n\\'")
         (delete-region (point) (point-max))
         (insert "\n"))

       ;; One space before and after " --> "
       (goto-char (point-min))
       (while (re-search-forward (format "^%s" subed-srt--regexp-timestamp) nil t)
         (when (looking-at "[[:blank:]]*-->[[:blank:]]*")
           (unless (= (length (match-string 0)) 5)
             (replace-match " --> "))))))))

(defun subed-srt-validate ()
  "Move point to the first invalid subtitle and report an error."
  (interactive)
  (when (> (buffer-size) 0)
    (atomic-change-group
      (save-match-data
        (let ((orig-point (point)))
          (goto-char (point-min))
          (while (re-search-forward (format "\\(%s[[^\\']]\\|\\`\\)" subed-srt--regexp-separator) nil t)
            (unless (looking-at "^[0-9]+$")
              (error "Found invalid subtitle ID: %S" (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
            (forward-line)
            ;; This regex is stricter than `subed-srt--regexp-timestamp'
            (unless (looking-at "^[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\},[0-9]\\{3\\}")
              (error "Found invalid start time: %S"  (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
            (when (re-search-forward "[[:blank:]]" (point-at-eol) t)
              (goto-char (match-beginning 0)))
            (unless (looking-at " --> ")
              (error "Found invalid separator between start and stop time: %S"
                     (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1)))
            (condition-case nil
                (forward-char 5)
              (error nil))
            (unless (looking-at "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\},[0-9]\\{3\\}$")
              (error "Found invalid stop time: %S" (substring (or (thing-at-point 'line :no-properties) "\n") 0 -1))))
          (goto-char orig-point))))))

(defun subed-srt-sort ()
  "Sanitize, then sort subtitles by start time and re-number them."
  (interactive)
  (atomic-change-group
    (subed-srt-sanitize)
    (subed-srt-validate)
    (subed--save-excursion
     (goto-char (point-min))
     (sort-subr nil
                ;; nextrecfun (move to next record/subtitle or to end-of-buffer
                ;; if there are no more records)
                (lambda () (unless (subed-srt-forward-subtitle-id)
                             (goto-char (point-max))))
                ;; endrecfun (move to end of current record/subtitle)
                'subed-srt-jump-to-subtitle-end
                ;; startkeyfun (return sort value of current record/subtitle)
                'subed-srt--subtitle-msecs-start))
    (subed-srt-regenerate-ids)))

(provide 'subed-srt)
;;; subed-srt.el ends here
