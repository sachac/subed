;;; subed-srt.el --- SubRip/srt implementation for subed

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
(defconst subed-srt--regexp-duration (concat subed-srt--regexp-timestamp "[ ]+\\(-->\\)[ ]+"
                                             subed-srt--regexp-timestamp))
(defconst subed-srt--regexp-separator "\\([[:blank:]]*\n\\)+[[:blank:]]*\n")
(defconst subed-srt--length-timestamp 12)  ;; String length of "01:45:32,091"

(defun subed-srt--timestamp-to-msecs (time-string)
  "Find HH:MM:SS,MS pattern in TIME-STRING and convert it to milliseconds.
Return nil if TIME-STRING doesn't match the pattern."
  (when (string-match subed-srt--regexp-timestamp time-string)
    (let ((hours (string-to-number (match-string 1 time-string)))
          (mins  (string-to-number (match-string 2 time-string)))
          (secs  (string-to-number (match-string 3 time-string)))
          (msecs (string-to-number (match-string 4 time-string))))
      (+ (* (truncate hours) 3600000)
         (* (truncate mins) 60000)
         (* (truncate secs) 1000)
         (truncate msecs)))))

(defun subed-srt--msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS,MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "," (format "%03d" (mod msecs 1000))))

(defun subed-srt--subtitle-id ()
  "Return the ID of subtitle at point or nil if there is no ID."
  (save-excursion
    (when (subed-srt-move-to-subtitle-id)
      (string-to-number (current-word)))))

(defun subed-srt--subtitle-id-at-msecs (msecs)
  "Return the ID of the subtitle at MSECS milliseconds.
If MSECS is between subtitles, return the subtitle that starts
after MSECS if there is one and its start time is >= MSECS +
1000.  Otherwise return the closest subtitle before MSECS."
  (save-excursion
    (goto-char (point-min))
    (let* ((secs       (/ msecs 1000))
           (only-hours (truncate (/ secs 3600)))
           (only-mins  (truncate (/ (- secs (* only-hours 3600)) 60))))
      ;; Move to first subtitle in the relevant hour
      (when (re-search-forward (format "\\(\n\n\\|\\`\\)[0-9]+\n%02d:" only-hours) nil t)
        (beginning-of-line)
        ;; Move to first subtitle in the relevant hour and minute
        (re-search-forward (format "\\(\n\n\\|\\`\\)[0-9]+\n%02d:%02d" only-hours only-mins) nil t)))
    ;; Move to first subtitle that starts at or after MSECS
    (catch 'last-subtitle-reached
      (while (<= (subed-srt--subtitle-msecs-start) msecs)
        (unless (subed-srt-forward-subtitle-id)
          (throw 'last-subtitle-reached nil))))
    ;; Move back to previous subtitle if start of current subtitle is in the
    ;; future (i.e. MSECS is between subtitles)
    (when (> (subed-srt--subtitle-msecs-start) msecs)
      (subed-srt-backward-subtitle-id))
    (subed-srt--subtitle-id)))

(defun subed-srt--subtitle-msecs-start (&optional sub-id)
  "Subtitle start time in milliseconds or nil if it can't be found."
  (let ((timestamp (save-excursion
                     (when (subed-srt-move-to-subtitle-time-start sub-id)
                       (buffer-substring (point) (+ (point) subed-srt--length-timestamp))))))
    (when timestamp
      (subed-srt--timestamp-to-msecs timestamp))))

(defun subed-srt--subtitle-msecs-stop (&optional sub-id)
  "Subtitle stop time in milliseconds or nil if it can't be found."
  (let ((timestamp (save-excursion
                     (when (subed-srt-move-to-subtitle-time-stop sub-id)
                       (buffer-substring (point) (+ (point) subed-srt--length-timestamp))))))
    (when timestamp
      (subed-srt--timestamp-to-msecs timestamp))))

(defun subed-srt--subtitle-relative-point ()
  "Point relative to subtitle's ID or nil if ID can't be found."
  (let ((start-point (save-excursion
                       (when (subed-srt-move-to-subtitle-id)
                         (point)))))
    (when start-point
      (- (point) start-point))))


;;; Traversing

(defun subed-srt-move-to-subtitle-id (&optional sub-id)
  "Move to the ID of a subtitle and return point.
If SUBTITLE-ID is not given, focus the current subtitle's ID.
Return point or nil if no subtitle ID could be found."
  (interactive)
  (if sub-id
      (progn
        ;; Start on the first ID and search forward for a line that contains
        ;; only the ID, preceded by one or more blank lines.
        (save-excursion
          (goto-char (point-min))
          (setq regex (format "\\(\\([[:blank:]]*\n\\)+[[:blank:]]*\n\\|\\`\\)%d$" sub-id))
          (setq match-found (re-search-forward regex nil t)))
        (when match-found
          (goto-char (match-end 0))
          (beginning-of-line)
          (point)))
    (progn
      ;; Find one or more blank lines.
      (re-search-forward "\\([[:blank:]]*\n\\)+" nil t)
      ;; Find two or more blank lines or the beginning of the buffer, followed
      ;; by line composed of only digits.
      (re-search-backward (concat "\\(" subed-srt--regexp-separator "\\|\\`\\)[0-9]+$") nil t)
      (goto-char (match-end 0))
      (beginning-of-line)
      (when (looking-at "^\\([0-9]+\\)$")
        (point)))))

(defun subed-srt-move-to-subtitle-time-start (&optional sub-id)
  "Move point to subtitle's start time.
Return point or nil if no start time could be found."
  (interactive)
  (when (subed-srt-move-to-subtitle-id sub-id)
    (forward-line)
    (when (looking-at subed-srt--regexp-timestamp)
      (point))))

(defun subed-srt-move-to-subtitle-time-stop (&optional sub-id)
  "Move point to subtitle's stop time.
Return point or nil if no stop time could be found."
  (interactive)
  (when (subed-srt-move-to-subtitle-id sub-id)
    (search-forward " --> " nil t)
    (when (looking-at subed-srt--regexp-timestamp)
      (point))))

(defun subed-srt-move-to-subtitle-text (&optional sub-id)
  "Move point on the first character of subtitle's text.
Return point."
  (interactive)
  (when (subed-srt-move-to-subtitle-id sub-id)
    (forward-line 2)
    (point)))

(defun subed-srt-move-to-subtitle-id-at-msecs (msecs)
  "Move point to the ID of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-srt--subtitle-id-at-msecs'."
  (let ((current-sub-id (subed-srt--subtitle-id))
        (target-sub-id (subed-srt--subtitle-id-at-msecs msecs)))
    (when (and target-sub-id current-sub-id (not (= target-sub-id current-sub-id)))
      (subed-srt-move-to-subtitle-id target-sub-id))))

(defun subed-srt-move-to-subtitle-text-at-msecs (msecs)
  "Move point to the text of the subtitle that is playing at MSECS.
Return point or nil if point is still on the same subtitle.
See also `subed-srt--subtitle-id-at-msecs'."
  (when (subed-srt-move-to-subtitle-id-at-msecs msecs)
    (subed-srt-move-to-subtitle-text)))

(defun subed-srt-move-to-subtitle-end (&optional sub-id)
  "Move point after the last character of the subtitle's text.
Return point unless point did not change."
  (interactive)
  (when (not (looking-at "\\([[:blank:]]*\n\\)*\\'"))
    (subed-srt-move-to-subtitle-text sub-id)
    (re-search-forward (concat "\\(" subed-srt--regexp-separator "\\|\\([[:blank:]]*\n\\)+\\'\\)") nil t)
    (goto-char (match-beginning 0))))

(defun subed-srt-forward-subtitle-id ()
  "Move point to next subtitle's ID.
Return point or nil if point didn't change (e.g. if called on the
last subtitle)."
  (interactive)
  (when (re-search-forward (concat subed-srt--regexp-separator "[[:alnum:]]") nil t)
    (subed-srt-move-to-subtitle-id)))

(defun subed-srt-backward-subtitle-id ()
  "Move point to previous subtitle's ID.
Return point or nil if point didn't change (e.g. if called on the
first subtitle)."
  (interactive)
  (when (re-search-backward subed-srt--regexp-separator nil t)
    (subed-srt-move-to-subtitle-id)))

(defun subed-srt-forward-subtitle-text ()
  "Move point to next subtitle's text.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-srt-forward-subtitle-id)
    (subed-srt-move-to-subtitle-text)))

(defun subed-srt-backward-subtitle-text ()
  "Move point to previous subtitle's text.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-srt-backward-subtitle-id)
    (subed-srt-move-to-subtitle-text)))

(defun subed-srt-forward-subtitle-end ()
  "Move point to end of next subtitle.
Return point or nil if there is no next subtitle."
  (interactive)
  (when (subed-srt-forward-subtitle-id)
    (subed-srt-move-to-subtitle-end)))

(defun subed-srt-backward-subtitle-end ()
  "Move point to end of previous subtitle.
Return point or nil if there is no previous subtitle."
  (interactive)
  (when (subed-srt-backward-subtitle-id)
    (subed-srt-move-to-subtitle-end)))

(defun subed-srt-forward-subtitle-time-start ()
  "Move point to next subtitle's start time."
  (interactive)
  (subed-srt-forward-subtitle-id)
  (subed-srt-move-to-subtitle-time-start))

(defun subed-srt-backward-subtitle-time-start ()
  "Move point to previous subtitle's start time."
  (interactive)
  (subed-srt-backward-subtitle-id)
  (subed-srt-move-to-subtitle-time-start))

(defun subed-srt-forward-subtitle-time-stop ()
  "Move point to next subtitle's stop time."
  (interactive)
  (subed-srt-forward-subtitle-id)
  (subed-srt-move-to-subtitle-time-stop))

(defun subed-srt-backward-subtitle-time-stop ()
  "Move point to previous subtitle's stop time."
  (interactive)
  (subed-srt-backward-subtitle-id)
  (subed-srt-move-to-subtitle-time-stop))


;;; Manipulation

(defun subed-srt--adjust-subtitle-start-relative (msecs)
  "Add MSECS milliseconds to start time (use negative value to subtract)."
  (let ((msecs-new (+ (subed-srt--subtitle-msecs-start) msecs)))
    (save-excursion
      (subed-srt-move-to-subtitle-time-start)
      (delete-region (point) (+ (point) subed-srt--length-timestamp))
      (insert (subed-srt--msecs-to-timestamp msecs-new)))
    (when subed-subtitle-time-adjusted-hook
      (let ((sub-id (subed-srt--subtitle-id)))
        (run-hook-with-args 'subed-subtitle-time-adjusted-hook sub-id msecs-new)))))

(defun subed-srt--adjust-subtitle-stop-relative (msecs)
  "Add MSECS milliseconds to stop time (use negative value to subtract)."
  (let ((msecs-new (+ (subed-srt--subtitle-msecs-stop) msecs)))
    (save-excursion
      (subed-srt-move-to-subtitle-time-stop)
      (delete-region (point) (+ (point) subed-srt--length-timestamp))
      (insert (subed-srt--msecs-to-timestamp msecs-new)))
    (when subed-subtitle-time-adjusted-hook
      (let ((sub-id (subed-srt--subtitle-id)))
        (run-hook-with-args 'subed-subtitle-time-adjusted-hook sub-id msecs-new)))))

(defun subed-srt-increase-start-time-100ms ()
  "Add 100 milliseconds to start time of current subtitle."
  (interactive)
  (subed-srt--adjust-subtitle-start-relative 100))

(defun subed-srt-decrease-start-time-100ms ()
  "Subtract 100 milliseconds from start time of current subtitle."
  (interactive)
  (subed-srt--adjust-subtitle-start-relative -100))

(defun subed-srt-increase-stop-time-100ms ()
  "Add 100 milliseconds to stop time of current subtitle."
  (interactive)
  (subed-srt--adjust-subtitle-stop-relative 100))

(defun subed-srt-decrease-stop-time-100ms ()
  "Subtract 100 milliseconds from stop time of current subtitle."
  (interactive)
  (subed-srt--adjust-subtitle-stop-relative -100))

;; TODO: Write tests
;; TODO: Implement support for prefix argument to
;;       - insert n subtitles with C-u n M-i.
;;       - insert 1 subtitle before the current one with C-u M-i.
(defun subed-srt-subtitle-insert ()
  "Insert a subtitle after the current."
  (interactive)
  (let ((start-time (+ (subed-srt--subtitle-msecs-stop) 100))
        (stop-time (- (save-excursion
                        (subed-srt-forward-subtitle-id)
                        (subed-srt--subtitle-msecs-start)) 100)))
    (subed-srt-forward-subtitle-id)
    (insert (format "1\n%s --> %s\n\n\n"
                    (subed-srt--msecs-to-timestamp start-time)
                    (subed-srt--msecs-to-timestamp stop-time))))
  (previous-line 2))

;; TODO: Implement support for prefix argument to
;;       kill n subtitles with C-u n M-k.
(defun subed-srt-subtitle-kill ()
  "Remove subtitle at point."
  (interactive)
  (let ((beg (save-excursion
               (subed-srt-move-to-subtitle-id)
               (point)))
        (end (save-excursion
               (subed-srt-move-to-subtitle-id)
               (when (subed-srt-forward-subtitle-id)
                 (point)))))
    (if (not end)
        (progn
          (let ((beg (save-excursion
                       (goto-char beg)
                       (subed-srt-backward-subtitle-text)
                       (subed-srt-move-to-subtitle-end)
                       (1+ (point))))
                (end (save-excursion
                       (goto-char (point-max)))))
            (delete-region beg end)))
      (progn
        (delete-region beg end)))))


;;; Maintenance

(defun subed-srt--regenerate-ids ()
  "Ensure subtitle IDs start at 1 and are incremented by 1 for
each subtitle."
  (save-excursion
    (goto-char (point-min))
    (let ((id 1))
      (while (looking-at "^[0-9]+$")
        (kill-word 1)
        (insert (format "%d" id))
        (setq id (1+ id))
        (subed-srt-forward-subtitle-id)))))

(defun subed-srt-sanitize ()
  "Remove surplus newlines and whitespace"
  (interactive)
  (subed--save-excursion
   ;; Remove trailing whitespace from lines and empty lines from end of buffer
   (delete-trailing-whitespace (point-min) nil)

   ;; Remove leading whitespace lines
   (goto-char (point-min))
   (while (re-search-forward "^[[:blank:]]+" nil t)
     (replace-match ""))

   ;; Remove excessive newlines between subtitles
   (goto-char (point-min))
   (while (re-search-forward subed-srt--regexp-separator nil t)
     (replace-match "\n\n"))

   ;; Remove any newlines from beginning of buffer
   (goto-char (point-min))
   (while (re-search-forward "\\`\n+" nil t)
     (replace-match ""))

   ;; Ensure single newline at end of buffer
   (goto-char (point-max))
   (when (not (looking-back "\n"))
     (insert "\n"))
   ))

(defun subed-srt-sort ()
  "Sanitize, then sort subtitles by start time and re-number them."
  (interactive)
  (subed-srt-sanitize)
  (subed--save-excursion
   (goto-char (point-min))
   (sort-subr nil
              ;; nextrecfun (move to next record/subtitle or to end-of-buffer
              ;; if there are no more records)
              (lambda () (unless (subed-srt-forward-subtitle-id)
                           (goto-char (point-max))))
              ;; endrecfun (move to end of current record/subtitle)
              'subed-srt-move-to-subtitle-end
              ;; startkeyfun (return sort value of current record/subtitle)
              'subed-srt--subtitle-msecs-start))
  (subed-srt--regenerate-ids))

(provide 'subed-srt)
;;; subed-srt.el ends here
