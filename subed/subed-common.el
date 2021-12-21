;;; subed-common.el --- Subtitle-format agnostic functions  -*- lexical-binding: t; -*-

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
;;
;; The functions in this file do not expect any particular subtitle format.
;; Instead, they expect certain functions to exist that provide navigation and
;; manipulation for whatever format the current buffer contains.

;;; Code:

(require 'cl-macs)
(require 'subed-config)
(require 'subed-debug)
(require 'subed-mpv)


;;; Utilities

(defun subed-msecs-to-timestamp (msecs)
  "Convert MSECS to human-readable string."
  ;; We need to wrap format-seconds in save-match-data because it does regexp
  ;; stuff and we need to preserve our own match-data.
  (concat (save-match-data (format-seconds "%02h:%02m:%02s" (/ msecs 1000)))
          "," (format "%03d" (mod msecs 1000))))

(defmacro subed-save-excursion (&rest body)
  "Restore relative point within current subtitle after executing BODY.
This also works if the buffer changes (e.g. when sorting
subtitles) as long the subtitle IDs don't change."
  (save-excursion
    `(let ((sub-id (subed-subtitle-id))
           (sub-pos (subed-subtitle-relative-point)))
       (progn ,@body)
       (subed-jump-to-subtitle-id sub-id)
       ;; Subtitle text may have changed and we may not be able to move to the
       ;; exact original position
       (condition-case nil
           (forward-char sub-pos)
         ('beginning-of-buffer nil)
         ('end-of-buffer nil)))))

(defmacro subed-for-each-subtitle (beg end reverse &rest body)
  "Run BODY for each subtitle between the region specified by BEG and END.
If END is nil, it defaults to `point-max'.
If BEG and END are both nil, run BODY only on the subtitle at point.
If REVERSE is non-nil, start on the subtitle at END and move backwards.
Before BODY is run, point is placed on the subtitle's ID."
  (declare (indent defun))
  `(atomic-change-group
     (if (not ,beg)
         ;; Run body on subtitle at point
         (save-excursion (subed-jump-to-subtitle-id)
                         ,@body)
       (let ((begm (make-marker))
             (endm (make-marker)))
         (set-marker begm ,beg)
         (set-marker endm (or ,end (point-max)))
         ;; Run body on multiple subtitles
         (if ,reverse
             ;; Iterate backwards
             (save-excursion (goto-char endm)
                             (subed-jump-to-subtitle-id)
                             (catch 'first-subtitle-reached
                               (while t
                                 ;; The subtitle includes every character up to the next subtitle's ID (or eob)
                                 (let ((sub-end (save-excursion (subed-jump-to-subtitle-end))))
                                   (when (< sub-end begm)
                                     (throw 'first-subtitle-reached t)))
                                 (progn ,@body)
                                 (unless (subed-backward-subtitle-id)
                                   (throw 'first-subtitle-reached t)))))
           ;; Iterate forwards
           (save-excursion (goto-char begm)
                           (subed-jump-to-subtitle-id)
                           (catch 'last-subtitle-reached
                             (while t
                               (when (> (point) endm)
                                 (throw 'last-subtitle-reached t))
                               (progn ,@body)
                               (unless (subed-forward-subtitle-id)
                                 (throw 'last-subtitle-reached t))))))))))

(defmacro subed-with-subtitle-replay-disabled (&rest body)
  "Run BODY while automatic subtitle replay is disabled."
  (declare (indent defun))
  `(let ((replay-was-enabled-p (subed-replay-adjusted-subtitle-p)))
     (subed-disable-replay-adjusted-subtitle :quiet)
     (progn ,@body)
     (when replay-was-enabled-p
       (subed-enable-replay-adjusted-subtitle :quiet))))

(defun subed--right-pad (string length fillchar)
  "Use FILLCHAR to make STRING LENGTH characters long."
  (concat string (make-string (- length (length string)) fillchar)))


;;; Hooks for point motion and subtitle motion

(defvar-local subed--current-point -1)
(defvar-local subed--current-subtitle-id -1)
(defun subed--post-command-handler ()
  "Detect point motion and user entering text and signal hooks."
  ;; Check for point motion first to avoid expensive calls to subed-subtitle-id
  ;; as often as possible.
  (let ((new-point (point)))
    (when (and new-point subed--current-point
               (not (= new-point subed--current-point)))

      ;; If point is synced to playback position, temporarily disable that so
      ;; that manual moves aren't cancelled immediately by automated moves.
      (subed-disable-sync-point-to-player-temporarily)

      ;; Store new point and fire signal.
      (setq subed--current-point new-point)
      (run-hooks 'subed-point-motion-hook)

      ;; Check if point moved across subtitle boundaries.
      (let ((new-sub-id (subed-subtitle-id)))
        (when (and new-sub-id subed--current-subtitle-id
                   (not (funcall (if (stringp subed--current-subtitle-id) 'string= 'equal)
                                 new-sub-id subed--current-subtitle-id)))
          ;; Store new ID and fire signal.
          (setq subed--current-subtitle-id new-sub-id)
          (run-hooks 'subed-subtitle-motion-hook))))))


;;; Adjusting start/stop time individually

(defun subed-adjust-subtitle-time-start (msecs &optional
                                               ignore-negative-duration
                                               ignore-overlap)
  "Add MSECS milliseconds to start time (use negative value to subtract).

Unless either IGNORE-NEGATIVE-DURATION or
`subed-enforce-time-boundaries' are non-nil, adjust MSECS so that
the stop time isn't smaller than the start time.  Zero-length
subtitles are always allowed.

Unless either IGNORE-OVERLAP or `subed-enforce-time-boundaries'
are non-nil, ensure that there are no gaps between subtitles
smaller than `subed-subtitle-spacing' milliseconds by adjusting
MSECS if necessary.

Return the number of milliseconds the start time was adjusted or
nil if nothing changed."
  (subed-disable-sync-point-to-player-temporarily)
  (let* ((msecs-start (subed-subtitle-msecs-start))
         (msecs-new (when msecs-start (+ msecs-start msecs))))
    (when msecs-new
      (if (> msecs 0)
          ;; Adding to start time
          (unless (or ignore-negative-duration
                      (not subed-enforce-time-boundaries))
            (let ((msecs-stop (subed-subtitle-msecs-stop)))
              (setq msecs-new (min msecs-new msecs-stop))))
        ;; Subtracting from start time
        (unless (or ignore-overlap
                    (not subed-enforce-time-boundaries))
          (let* ((msecs-prev-stop (save-excursion (when (subed-backward-subtitle-id)
                                                    (subed-subtitle-msecs-stop))))
                 (msecs-min (if msecs-prev-stop
                                (+ msecs-prev-stop subed-subtitle-spacing) 0)))
            (when msecs-min
              (setq msecs-new (max msecs-new msecs-min))))))
      ;; MSECS-NEW must be bigger than the current start time if we are adding
      ;; or smaller if we are subtracting.
      (when (and (>= msecs-new 0)                                  ;; Ignore negative times
                 (or (and (> msecs 0) (> msecs-new msecs-start))   ;; Adding
                     (and (< msecs 0) (< msecs-new msecs-start)))) ;; Subtracting
        (subed-set-subtitle-time-start msecs-new)
        (subed--run-subtitle-time-adjusted-hook)
        (- msecs-new msecs-start)))))

(defun subed-adjust-subtitle-time-stop (msecs &optional
                                              ignore-negative-duration
                                              ignore-overlap)
  "Add MSECS milliseconds to stop time (use negative value to subtract).

Unless either IGNORE-NEGATIVE-DURATION or
`subed-enforce-time-boundaries' are non-nil, adjust MSECS so that
the stop time isn't smaller than the start time.  Zero-length
subtitles are always allowed.

Unless either IGNORE-OVERLAP or `subed-enforce-time-boundaries'
are non-nil, ensure that there are no gaps between subtitles
smaller than `subed-subtitle-spacing' milliseconds by adjusting
MSECS if necessary.

Return the number of milliseconds the stop time was adjusted or
nil if nothing changed."
  (subed-disable-sync-point-to-player-temporarily)
  (let* ((msecs-stop (subed-subtitle-msecs-stop))
         (msecs-new (when msecs-stop (+ msecs-stop msecs))))
    (when msecs-new
      (if (> msecs 0)
          ;; Adding to stop time
          (unless (or ignore-overlap
                      (not subed-enforce-time-boundaries))
            (let* ((msecs-next-start (save-excursion (when (subed-forward-subtitle-id)
                                                       (subed-subtitle-msecs-start))))
                   (msecs-max (when msecs-next-start
                                (- msecs-next-start subed-subtitle-spacing))))
              (when msecs-max
                (setq msecs-new (min msecs-new msecs-max)))))
        ;; Subtracting from stop time
        (unless (or ignore-negative-duration
                    (not subed-enforce-time-boundaries))
          (let ((msecs-start (subed-subtitle-msecs-start)))
            (setq msecs-new (max msecs-new msecs-start)))))
      ;; MSECS-NEW must be bigger than the current stop time if we are adding or
      ;; smaller if we are subtracting.
      (when (and (>= msecs-new 0)                                  ;; Ignore negative times
                 (or (and (> msecs 0) (> msecs-new msecs-stop))    ;; Adding
                     (and (< msecs 0) (< msecs-new msecs-stop))))  ;; Subtracting
        (subed-set-subtitle-time-stop msecs-new)
        (subed--run-subtitle-time-adjusted-hook)
        (- msecs-new msecs-stop)))))

(defun subed-increase-start-time (&optional arg)
  "Add `subed-milliseconds-adjust' milliseconds to start time.

Return new start time in milliseconds or nil if it didn't change.

If prefix argument ARG is given, it is used to set
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
  (subed-adjust-subtitle-time-start (subed-get-milliseconds-adjust arg)))

(defun subed-decrease-start-time (&optional arg)
  "Subtract `subed-milliseconds-adjust' milliseconds from start time.

Return new start time in milliseconds or nil if it didn't change.

See `subed-increase-start-time' about ARG."
  (interactive "P")
  (subed-adjust-subtitle-time-start (* -1 (subed-get-milliseconds-adjust arg))))

(defun subed-increase-stop-time (&optional arg)
  "Add `subed-milliseconds-adjust' milliseconds to stop time.

Return new stop time in milliseconds or nil if it didn't change.

See `subed-increase-start-time' about ARG."
  (interactive "P")
  (subed-adjust-subtitle-time-stop (subed-get-milliseconds-adjust arg)))

(defun subed-decrease-stop-time (&optional arg)
  "Subtract `subed-milliseconds-adjust' milliseconds from stop time.

Return new stop time in milliseconds or nil if it didn't change.

See `subed-increase-start-time' about ARG."
  (interactive "P")
  (subed-adjust-subtitle-time-stop (* -1 (subed-get-milliseconds-adjust arg))))

(defun subed-copy-player-pos-to-start-time ()
  "Replace current subtitle's start time with current playback time."
  (interactive)
  (when (and subed-mpv-playback-position
	         (subed-subtitle-msecs-start))
    (subed-set-subtitle-time-start subed-mpv-playback-position)
    (subed--run-subtitle-time-adjusted-hook)
    subed-mpv-playback-position))

(defun subed-copy-player-pos-to-stop-time ()
  "Replace current subtitle's stop time with current playback time."
  (interactive)
  (when (and subed-mpv-playback-position
	         (subed-subtitle-msecs-stop))
    (subed-set-subtitle-time-stop subed-mpv-playback-position)
    (subed--run-subtitle-time-adjusted-hook)
    subed-mpv-playback-position))


;;; Moving subtitles
;;; (adjusting start and stop time by the same amount)

(defun subed--get-move-subtitle-func (msecs)
  "Return subtitle moving function.

When moving subtitles forward (MSECS > 0), we must adjust the
stop time first and adjust the start time by the same amount the
stop time was adjusted.  This ensures that subtitle length
doesn't change if we can't move MSECS milliseconds forward
because we'd overlap with the next subtitle.

When moving subtitles backward (MSECS < 0), it's the same thing
but we move the start time first."
  (if (> msecs 0)
      ;; Moving forward
      (lambda (msecs &optional ignore-overlap)
        (let ((msecs (subed-adjust-subtitle-time-stop msecs
                                                      :ignore-negative-duration
                                                      ignore-overlap)))
          (when msecs (subed-adjust-subtitle-time-start msecs
                                                        :ignore-negative-duration
                                                        ignore-overlap))))
    ;; Moving backward
    (lambda (msecs &optional ignore-overlap)
      (let ((msecs (subed-adjust-subtitle-time-start msecs
                                                     :ignore-negative-duration
                                                     ignore-overlap)))
        (when msecs (subed-adjust-subtitle-time-stop msecs
                                                     :ignore-negative-duration
                                                     ignore-overlap))))))

(defun subed--move-current-subtitle (msecs)
  "Move subtitle on point by MSECS milliseconds."
  (unless (= msecs 0)
    (subed-with-subtitle-replay-disabled
      (cl-flet ((move-subtitle (subed--get-move-subtitle-func msecs)))
        (move-subtitle msecs)))))

(defun subed--scale-subtitles-in-region (msecs beg end)
  "Scale subtitles in region specified by BEG and END after moving END MSECS milliseconds."
  (let* ((beg-point (save-excursion ; normalized to fixed location over BEG
                      (goto-char beg)
                      (subed-jump-to-subtitle-end)
                      (point)))
         (beg-next-point (save-excursion
                           (goto-char beg-point)
                           (subed-forward-subtitle-end)
                           (point)))
         (end-point (save-excursion ; normalized to fixed location over END
                      (goto-char end)
                      (subed-jump-to-subtitle-end)
                      (point)))
         (end-prev-point (save-excursion
                           (goto-char end-point)
                           (subed-backward-subtitle-end)
                           (point)))
         (beg-start-msecs (save-excursion
                            (goto-char beg-point)
                            (subed-subtitle-msecs-start)))
         (old-end-start-msecs (save-excursion
                                (goto-char end-point)
                                (subed-subtitle-msecs-start))))
    ;; check for improper range (BEG after END)
    (unless (<= beg end)
      (user-error "Can't scale with improper range"))
    ;; check for 0 or 1 subtitle scenario
    (unless (/= beg-point end-point)
      (user-error "Can't scale with fewer than 3 subtitles"))
    ;; check for 2 subtitle scenario
    (unless (/= beg-point end-prev-point)
      (user-error "Can't scale with only 2 subtitles"))
    ;; check for missing timestamps
    (unless beg-start-msecs
      (user-error "Can't scale when first subtitle timestamp missing"))
    (unless old-end-start-msecs
      (user-error "Can't scale when last subtitle timestamp missing"))
    ;; check for range with 0 time interval
    (unless (/= beg-start-msecs old-end-start-msecs)
      (user-error "Can't scale subtitle range with 0 time interval"))

    (unless (= msecs 0)
      (subed-with-subtitle-replay-disabled
        (cl-flet ((move-subtitle (subed--get-move-subtitle-func msecs)))
          (let* ((new-end-start-msecs (+ old-end-start-msecs msecs))
                 (scale-factor (/ (float (- new-end-start-msecs beg-start-msecs))
                                  (float (- old-end-start-msecs beg-start-msecs))))
                 (scale-subtitles
                  (lambda (&optional reverse)
                    (subed-for-each-subtitle beg-next-point end-prev-point reverse
                      (let ((old-start-msecs (subed-subtitle-msecs-start)))
                        (unless old-start-msecs
                          (user-error "Can't scale when subtitle timestamp missing"))
                        (let* ((new-start-msecs
                                (+ beg-start-msecs
                                   (round (* (- old-start-msecs beg-start-msecs) scale-factor))))
                               (delta-msecs (- new-start-msecs old-start-msecs)))
                          (unless (and (<= beg-start-msecs old-start-msecs)
                                       (>= old-end-start-msecs old-start-msecs))
                            (user-error "Can't scale when nonchronological subtitles exist"))
                          (move-subtitle delta-msecs :ignore-negative-duration)))))))
            (atomic-change-group
              (if (> msecs 0)
                  (save-excursion
                    ;; Moving forward - Start on last subtitle to see if we
                    ;; can move forward.
                    (goto-char end)
                    (let ((adjusted-msecs (move-subtitle msecs)))
                      (unless (and adjusted-msecs
                                   (= msecs adjusted-msecs))
                        (user-error "Can't scale when extension would overlap subsequent subtitles")))
                    (funcall scale-subtitles :reverse))
                (save-excursion
                  ;; Moving backward - Make sure the last subtitle will not
                  ;; precede the first subtitle.
                  (unless (> new-end-start-msecs beg-start-msecs)
                    (user-error "Can't scale when contraction would eliminate region"))
                  (goto-char end)
                  (move-subtitle msecs :ignore-negative-duration)
                  (funcall scale-subtitles))))))))))

(defun subed--move-subtitles-in-region (msecs beg end)
  "Move subtitles in region specified by BEG and END by MSECS milliseconds."
  (unless (= msecs 0)
    (subed-with-subtitle-replay-disabled
      (cl-flet ((move-subtitle (subed--get-move-subtitle-func msecs)))
        ;; When moving subtitles forward, the first step is to move the last
        ;; subtitle because:
        ;;     a) We need to check if we can move at all and abort if not.
        ;;     b) We may have to reduce MSECS if we can move but not by the full
        ;;        amount. The goal is that all subtitles are moved by the same
        ;;        amount and the spacing between subtitles doesn't change.
        ;; All other subtitles must be moved without any checks because we only
        ;; ensure that the active region as a whole can be moved, not it's
        ;; individual parts, which may be too close together or even overlap.
        ;; Moving subtitles backward is basically the same thing but vice versa.
        (catch 'bumped-into-subtitle
          (if (> msecs 0)
              (save-excursion
                ;; Moving forward - Start on last subtitle to see if/how far
                ;; we can move forward.
                (goto-char end)
                (unless (setq msecs (move-subtitle msecs))
                  (throw 'bumped-into-subtitle t))
                (subed-backward-subtitle-id)
                (subed-for-each-subtitle beg (point) :reverse
                  (move-subtitle msecs :ignore-negative-duration)))
            ;; Start on first subtitle to see if/how far we can move backward.
            (save-excursion
              (goto-char beg)
              (unless (setq msecs (move-subtitle msecs))
                (throw 'bumped-into-subtitle t))
              (subed-forward-subtitle-id)
              (subed-for-each-subtitle (point) end nil
                (move-subtitle msecs :ignore-negative-duration)))))))))

(defun subed-scale-subtitles (msecs &optional beg end)
  "Scale subtitles between BEG and END after moving END MSECS.
Use a negative MSECS value to move END backward.
If END is nil, END will be the last subtitle in the buffer.
If BEG is nil, BEG will be the first subtitle in the buffer."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
    (subed--scale-subtitles-in-region msecs beg end)
    (when (subed-replay-adjusted-subtitle-p)
      (save-excursion
        (goto-char end)
        (subed-jump-to-subtitle-id)
        (subed-mpv-jump (subed-subtitle-msecs-start))))))

(defun subed-scale-subtitles-forward (&optional arg)
  "Scale subtitles after region is extended `subed-milliseconds-adjust'.

Scaling adjusts start and stop by the same amount, preserving
subtitle duration.

All subtitles that are fully or partially in the active region
are moved so they are placed proportionally in the new range.

If prefix argument ARG is given, it is used to extend the end of the region
`subed-milliseconds-adjust' before proportionally adjusting subtitles.  If the
prefix argument is given but not numerical,
`subed-milliseconds-adjust' is reset to its default value.

Example usage:
  \\[universal-argument] 1000 \\[subed-scale-subtitles-forward] Extend region 1000ms forward in time and scale subtitles in region
           \\[subed-scale-subtitles-forward] Extend region another 1000ms forward in time and scale subtitles again
   \\[universal-argument] 500 \\[subed-scale-subtitles-forward] Extend region 500ms forward in time and scale subtitles in region
           \\[subed-scale-subtitles-forward] Extend region another 500ms forward in time and scale subtitles again
       \\[universal-argument] \\[subed-scale-subtitles-forward] Extend region 100ms (the default) forward in time and scale subtitles in region
           \\[subed-scale-subtitles-forward] Extend region another 100ms (the default) forward in time and scale subtitles again"
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (subed-get-milliseconds-adjust arg))
        (beg (when mark-active (region-beginning)))
        (end (when mark-active (region-end))))
    (subed-scale-subtitles msecs beg end)))

(defun subed-scale-subtitles-backward (&optional arg)
  "Scale subtitles after region is shortened `subed-milliseconds-adjust'.

See `subed-scale-subtitles-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (* -1 (subed-get-milliseconds-adjust arg)))
        (beg (when mark-active (region-beginning)))
        (end (when mark-active (region-end))))
    (subed-scale-subtitles msecs beg end)))

(defun subed-move-subtitles (msecs &optional beg end)
  "Move subtitles between BEG and END MSECS milliseconds forward.
Use a negative MSECS value to move subtitles backward.
If END is nil, move all subtitles from BEG to end of buffer.
If BEG is nil, move only the current subtitle.
After subtitles are moved, replay the first moved subtitle if
replaying is enabled."
  (cond ((and beg end) (subed--move-subtitles-in-region msecs beg end))
        (beg (subed--move-subtitles-in-region msecs beg (point-max)))
        (t (subed--move-current-subtitle msecs)))
  (when (subed-replay-adjusted-subtitle-p)
    (save-excursion
      (when beg (goto-char beg))
      (subed-mpv-jump (subed-subtitle-msecs-start)))))

(defun subed-move-subtitle-forward (&optional arg)
  "Move subtitle `subed-milliseconds-adjust' forward.

Moving adjusts start and stop time by the same amount, preserving
subtitle duration.

All subtitles that are fully or partially in the active region
are moved.

If prefix argument ARG is given, it is used to set
`subed-milliseconds-adjust' before moving subtitles.  If the
prefix argument is given but not numerical,
`subed-milliseconds-adjust' is reset to its default value.

Example usage:
  \\[universal-argument] 1000 \\[subed-move-subtitle-forward]  Move subtitle 1000ms forward in time
           \\[subed-move-subtitle-forward]  Move subtitle 1000ms forward in time again
   \\[universal-argument] 500 \\[subed-move-subtitle-forward]  Move subtitle 500ms forward in time
           \\[subed-move-subtitle-forward]  Move subtitle 500ms forward in time again
       \\[universal-argument] \\[subed-move-subtitle-forward]  Move subtitle 100ms (the default) forward in time
           \\[subed-move-subtitle-forward]  Move subtitle 100ms (the default) forward in time again"
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (subed-get-milliseconds-adjust arg))
        (beg (when mark-active (region-beginning)))
        (end (when mark-active (region-end))))
    (subed-move-subtitles msecs beg end)))

(defun subed-move-subtitle-backward (&optional arg)
  "Move subtitle `subed-milliseconds-adjust' backward.

See `subed-move-subtitle-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (* -1 (subed-get-milliseconds-adjust arg)))
        (beg (when mark-active (region-beginning)))
        (end (when mark-active (region-end))))
    (subed-move-subtitles msecs beg end)))


;;; Shifting subtitles
;;; (same as moving, but follow-up subtitles are also moved)

(defun subed-shift-subtitle-forward (&optional arg)
  "Shift subtitle `subed-milliseconds-adjust' backward.

Shifting is like moving, but it always moves the subtitles
between point and the end of the buffer.

See `subed-move-subtitle-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (subed-get-milliseconds-adjust arg)))
    (subed-move-subtitles msecs (point))))

(defun subed-shift-subtitle-backward (&optional arg)
  "Shift subtitle `subed-milliseconds-adjust' backward.

Shifting is like moving, but it always moves the subtitles
between point and the end of the buffer.

See `subed-move-subtitle-forward' about ARG."
  (interactive "P")
  (let ((deactivate-mark nil)
        (msecs (* -1 (subed-get-milliseconds-adjust arg))))
    (subed-move-subtitles msecs (point))))


;;; Inserting

(defun subed--insert-subtitle-info (arg)
  "Provide information for inserting subtitles.
ARG is the user-given argument.
Return a list of values in the following order:
  number-of-subs insert-before-current (t or nil) buffer-is-empty
  msecs-min msecs-max msecs-avail msecs-per-sub msecs-between
  insert-subtitle-func"
  (let* ((number-of-subs (cond ((not arg) 1)         ;; M-i
                               ((integerp arg) arg)  ;; C-u N M-i  /  C-u - N M-i
                               ;; C-u [C-u ...] M-i  /  C-u - [C-u ...] M-i
                               ((consp arg) (* (truncate (log (abs (car arg)) 4)) ;; ([-]64) -> 3
                                               (/ (car arg) (abs (car arg)))))    ;; Restore sign
                               (t 1)))  ;; C-u - M-i
         (insert-before-current (or (< number-of-subs 0)  ;; C-u - N M-i
                                    (eq arg '-)           ;; C-u - M-i
                                    (consp arg)))         ;; C-u [C-u ...] M-i
         ;; Ensure number-of-subs is positive, now that we figured out `insert-before-current'
         (number-of-subs (abs number-of-subs))
         (buffer-is-empty (if (subed-subtitle-id) nil t))
         ;; Find out how much time there is available
         (msecs-min (save-excursion (if insert-before-current
                                        (if (subed-backward-subtitle-id)
                                          (subed-subtitle-msecs-stop) 0)
                                      (subed-subtitle-msecs-stop))))
         (msecs-max (save-excursion (if insert-before-current
                                        (subed-subtitle-msecs-start)
                                      (when (subed-forward-subtitle-id)
                                        (subed-subtitle-msecs-start)))))
         (msecs-avail (cond ((and msecs-min msecs-max) (- msecs-max msecs-min))
                            (msecs-max msecs-max)
                            (t nil)))  ;; Unlimited
         (msecs-per-sub (if msecs-avail
                            (min subed-default-subtitle-length
                                 (max 0 (/ (- msecs-avail (* (1+ number-of-subs) subed-subtitle-spacing))
                                           number-of-subs)))
                          subed-default-subtitle-length))
         (msecs-between (if (or (not msecs-avail)
                                (>= msecs-avail (* (1+ number-of-subs) subed-subtitle-spacing)))
                            subed-subtitle-spacing
                          0))
         (insert-subtitle-func (if insert-before-current
                                   #'subed-prepend-subtitle
                                 #'subed-append-subtitle)))
    (subed-debug "Inserting %s subtitle(s) %s the current in %sempty buffer"
                 number-of-subs
                 (if insert-before-current "before" "after")
                 (if buffer-is-empty "" "non-"))
    (subed-debug "  Available time: min=%S max=%S avail=%S sublen=%S/%S"
                 msecs-min msecs-max msecs-avail msecs-per-sub subed-default-subtitle-length)
    (list number-of-subs insert-before-current buffer-is-empty
          msecs-min msecs-max msecs-avail msecs-per-sub msecs-between
          insert-subtitle-func)))

(defun subed-insert-subtitle (&optional arg)
  "Insert subtitle(s) evenly spaced.

The inserted subtitles are `subed-default-subtitle-length'
milliseconds long.

Subtitles are spread out evenly over the available time.

ARG, usually provided by `universal-argument', is used in the
following manner:
          \\[subed-insert-subtitle]   Insert 1 subtitle after the current subtitle
      \\[universal-argument] \\[subed-insert-subtitle]   Insert 1 subtitle before the current subtitle
    \\[universal-argument] 5 \\[subed-insert-subtitle]   Insert 5 subtitles after the current subtitle
  \\[universal-argument] - 5 \\[subed-insert-subtitle]   Insert 5 subtitles before the current subtitle
  \\[universal-argument] \\[universal-argument] \\[subed-insert-subtitle]   Insert 2 subtitles before the current subtitle"
  (interactive "P")
  (atomic-change-group
    (cl-multiple-value-bind (number-of-subs insert-before-current _buffer-is-empty
                             msecs-min msecs-max msecs-avail msecs-per-sub msecs-between
                             insert-subtitle-func)
                            (subed--insert-subtitle-info arg)
      (dotimes (i number-of-subs)
        ;; Value constellations:
        ;;     empty buffer, append           : min=0       max=nil     avail=nil
        ;;     empty buffer, prepend          : min=0       max=nil     avail=nil
        ;; non-empty buffer, prepend, betwixt : min=non-nil max=non-nil avail=non-nil
        ;; non-empty buffer, append,  betwixt : min=non-nil max=non-nil avail=non-nil
        ;; non-empty buffer, prepend to first : min=0       max=non-nil avail=non-nil
        ;; non-empty buffer, append  to last  : min=non-nil max=nil     avail=nil
        (let* ((multiplier (if insert-before-current
                               (- number-of-subs i)
                             (1+ i)))
               (msecs-start (if msecs-avail
                                ;; Inserting anywhere before the last subtitle
                                (+ msecs-min
                                   (if (< msecs-per-sub subed-default-subtitle-length)
                                       ;; Use all available space between subtitles
                                       (+ msecs-between
                                          (* (1- multiplier) (+ msecs-between msecs-per-sub)))
                                     ;; Leave extra space between subtitles
                                     (* multiplier (/ msecs-avail (1+ number-of-subs)))))
                              (if (and msecs-min (not msecs-max))
                                  ;; Appending to last subtitle
                                  (+ msecs-min
                                     ;; If buffer is empty, start first subtitle at 0
                                     (if (> msecs-min 0) msecs-between 0)
                                     (* (1- multiplier) (+ msecs-per-sub msecs-between)))
                                ;; Appending in empty buffer
                                (* i (+ msecs-per-sub msecs-between)))))
               (msecs-stop (+ msecs-start msecs-per-sub)))
          (subed-debug "  Inserting new subtitle at %S - %S" msecs-start msecs-stop)
          (funcall insert-subtitle-func nil msecs-start msecs-stop nil)))
      (unless insert-before-current
        (dotimes (_ (1- number-of-subs))
          (subed-backward-subtitle-text))))
    (subed-regenerate-ids-soon))
  (point))

(defun subed-insert-subtitle-adjacent (&optional arg)
  "Insert subtitle(s) close to each other.

The inserted subtitles are `subed-default-subtitle-length'
milliseconds long.

Subtitles are inserted `subed-subtitle-spacing' milliseconds
before or after the current subtitle.  When inserting multiple
subtitles, the gap between them is also `subed-subtitle-spacing'
milliseconds long.

ARG, usually provided by `universal-argument', is used in the
following manner:
          \\[subed-insert-subtitle]   Insert 1 subtitle after the current subtitle
      \\[universal-argument] \\[subed-insert-subtitle]   Insert 1 subtitle before the current subtitle
    \\[universal-argument] 5 \\[subed-insert-subtitle]   Insert 5 subtitles after the current subtitle
  \\[universal-argument] - 5 \\[subed-insert-subtitle]   Insert 5 subtitles before the current subtitle
  \\[universal-argument] \\[universal-argument] \\[subed-insert-subtitle]   Insert 2 subtitles before the current subtitle"
  (interactive "P")
  (atomic-change-group
    (cl-multiple-value-bind (number-of-subs insert-before-current buffer-is-empty
                             msecs-min msecs-max _msecs-avail msecs-per-sub msecs-between
                             insert-subtitle-func)
                            (subed--insert-subtitle-info arg)
      (dotimes (i number-of-subs)
        ;; Value constellations:
        ;;     empty buffer, append           : min=0       max=nil     avail=nil
        ;;     empty buffer, prepend          : min=0       max=nil     avail=nil
        ;; non-empty buffer, prepend, betwixt : min=non-nil max=non-nil avail=non-nil
        ;; non-empty buffer, append,  betwixt : min=non-nil max=non-nil avail=non-nil
        ;; non-empty buffer, prepend to first : min=0       max=non-nil avail=non-nil
        ;; non-empty buffer, append  to last  : min=non-nil max=nil     avail=nil
        (let* ((multiplier (if insert-before-current
                               (- number-of-subs i 1)
                             i))
               (msecs-start (if buffer-is-empty
                                (* multiplier (+ msecs-between msecs-per-sub))
                              (if insert-before-current
                                  (- msecs-max (* (1+ i) (+ msecs-between msecs-per-sub)))
                                (+ msecs-min
                                   msecs-between
                                   (* i (+ msecs-per-sub msecs-between))))))
               (msecs-stop (+ msecs-start msecs-per-sub)))
          (subed-debug "  Inserting new subtitle at %S - %S" msecs-start msecs-stop)
          (funcall insert-subtitle-func nil msecs-start msecs-stop nil)))
      (unless insert-before-current
        (dotimes (_ (1- number-of-subs))
          (subed-backward-subtitle-text))))
    (subed-regenerate-ids-soon))
  (point))

(defun subed-split-subtitle (&optional offset)
  "Split current subtitle at point.

The subtitle text after point is moved to a new subtitle that is
inserted after the current subtitle.

If OFFSET is a number, it is used as the offset in milliseconds
from the starting timestamp if positive or from the ending
timestamp if negative.  Otherwise, if
`subed-mpv-playback-position' is within the current subtitle, it
is used as the new stop time of the current subtitle.  Otherwise,
the timestamp proportional to the point's position between start
and stop timestamp of the current subtitle is used.

If called interactively, calling it with one prefix
argument (e.g. \\[universal-argument] \\[subed-split-subtitle])
prompts for the offset in milliseconds.  Calling it with two
prefix arguments (e.g. \\[universal-argument]
\\[universal-argument] \\[subed-split-subtitle]) uses the
relative position of the point even if the video is playing in
MPV.

The newly inserted subtitle starts `subed-subtitle-spacing'
milliseconds after the current subtitle's new end timestamp.

Move to the beginning of the new subtitle's text and return the
position of the point."
  (interactive (list
                (cond
                 ((equal current-prefix-arg '(4))
                  (read-number "Offset (ms): "))
                 ((equal current-prefix-arg '(16)) t))))
  (let ((text-beg (save-excursion (subed-jump-to-subtitle-text)))
        (text-end (save-excursion (or (subed-jump-to-subtitle-end) (point)))))
    ;; Ensure point is on subtitle text
    (unless (and text-beg (>= (point) text-beg))
      (subed-jump-to-subtitle-text))
    (let* ((orig-start (subed-subtitle-msecs-start))
           (orig-end (subed-subtitle-msecs-stop))
           (text-fraction (if (= text-beg text-end) 1 (/ (* 1.0 (- (point) text-beg)) (- text-end text-beg))))
           (time-fraction (floor (* text-fraction (- orig-end orig-start))))
           (split-timestamp
            (cond
             ((and (numberp offset) (> offset 0)) (+ orig-start offset))
             ((and (numberp offset) (< offset 0)) (+ orig-end offset))
             ((or (equal offset t)
                  (null subed-mpv-playback-position)
                  (> subed-mpv-playback-position orig-end)
                  (< subed-mpv-playback-position orig-start))
              (+ orig-start time-fraction))
             (subed-mpv-playback-position subed-mpv-playback-position)))
           (new-text (string-trim (buffer-substring (point) text-end)))
           (new-start-timestamp (+ split-timestamp subed-subtitle-spacing)))
      (subed-set-subtitle-time-stop split-timestamp)
      (skip-chars-backward "\n")
      (delete-region (point) (progn (subed-jump-to-subtitle-end) (skip-chars-forward " \t") (point)))
      (when (looking-at "[ \t]+") (replace-match ""))
      (subed-append-subtitle nil new-start-timestamp orig-end (string-trim new-text)))
    (subed-regenerate-ids-soon)
    (point)))

;;; Merging

(defun subed-merge-with-previous ()
  "Merge the current subtitle with the previous subtitle.
Update the end timestamp accordingly."
  (interactive)
  (if (subed-backward-subtitle-id)
      (subed-merge-with-next)
    (error "No previous subtitle to merge into")))

;;; Replay time-adjusted subtitle

(defun subed-replay-adjusted-subtitle-p ()
  "Whether the player jumps to start time when start or stop time is adjusted."
  (member #'subed--replay-adjusted-subtitle subed-subtitle-time-adjusted-hook))

(defun subed-enable-replay-adjusted-subtitle (&optional quiet)
  "Automatically replay a subtitle when its start/stop time is adjusted.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (unless (subed-replay-adjusted-subtitle-p)
    (add-hook 'subed-subtitle-time-adjusted-hook #'subed--replay-adjusted-subtitle :append :local)
    (subed-debug "Enabled replaying adjusted subtitle: %s" subed-subtitle-time-adjusted-hook)
    (unless quiet
      (message "Enabled replaying adjusted subtitle"))))

(defun subed-disable-replay-adjusted-subtitle (&optional quiet)
  "Do not replay a subtitle automatically when its start/stop time is adjusted.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (when (subed-replay-adjusted-subtitle-p)
    (remove-hook 'subed-subtitle-time-adjusted-hook #'subed--replay-adjusted-subtitle :local)
    (subed-debug "Disabled replaying adjusted subtitle: %s" subed-subtitle-time-adjusted-hook)
    (unless quiet
      (message "Disabled replaying adjusted subtitle"))))

(defun subed-toggle-replay-adjusted-subtitle ()
  "Enable/disable subtitle replay when start/stop time is adjusted."
  (interactive)
  (if (subed-replay-adjusted-subtitle-p)
      (subed-disable-replay-adjusted-subtitle)
    (subed-enable-replay-adjusted-subtitle)))

(defun subed--replay-adjusted-subtitle (msecs-start)
  "Seek player to MSECS-START."
  (subed-debug "Replaying subtitle at: %s" (subed-msecs-to-timestamp msecs-start))
  (subed-mpv-jump msecs-start))


;;; Sync point-to-player

(defun subed-sync-point-to-player-p ()
  "Whether point is automatically moved to currently playing subtitle."
  (member #'subed--sync-point-to-player subed-mpv-playback-position-hook))

(defun subed-enable-sync-point-to-player (&optional quiet)
  "Automatically move point to the currently playing subtitle.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (unless (subed-sync-point-to-player-p)
    (add-hook 'subed-mpv-playback-position-hook #'subed--sync-point-to-player :append :local)
    (subed-debug "Enabled syncing point to playback position: %s" subed-mpv-playback-position-hook)
    (unless quiet
      (message "Enabled syncing point to playback position"))))

(defun subed-disable-sync-point-to-player (&optional quiet)
  "Do not move point automatically to the currently playing subtitle.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (when (subed-sync-point-to-player-p)
    (remove-hook 'subed-mpv-playback-position-hook #'subed--sync-point-to-player :local)
    (subed-debug "Disabled syncing point to playback position: %s" subed-mpv-playback-position-hook)
    (unless quiet
      (message "Disabled syncing point to playback position"))))

(defun subed-toggle-sync-point-to-player ()
  "Enable/disable moving point to the currently playing subtitle."
  (interactive)
  (if (subed-sync-point-to-player-p)
      (subed-disable-sync-point-to-player)
    (subed-enable-sync-point-to-player)))

(defun subed--sync-point-to-player (msecs)
  "Move point to subtitle at MSECS."
  (when (and (not (use-region-p)) ;; Don't sync with active-mark in transient-mark-mode
             (subed-jump-to-subtitle-text-at-msecs msecs))
    (subed-debug "Synchronized point to playback position: %s -> #%s"
                 (subed-msecs-to-timestamp msecs) (subed-subtitle-id))
    ;; post-command-hook is not triggered because we didn't move interactively,
    ;; but there shouldn't be a difference between automatic movement and manual
    ;; movement.  E.g. the minor mode `hl-line' breaks because its post-command
    ;; function is not called.
    ;; But it's also important NOT to call our own post-command function because
    ;; that causes player-to-point syncing, which would get hairy.
    (remove-hook 'post-command-hook #'subed--post-command-handler)
    (run-hooks 'post-command-hook)
    (add-hook 'post-command-hook #'subed--post-command-handler :append :local)))

(defvar-local subed--point-sync-delay-after-motion-timer nil)
(defun subed-disable-sync-point-to-player-temporarily ()
  "Temporarily disable syncing point to player.

After `subed-point-sync-delay-after-motion' seconds point is re-synced."
  (if subed--point-sync-delay-after-motion-timer
      (cancel-timer subed--point-sync-delay-after-motion-timer)
    (setq subed--point-was-synced (subed-sync-point-to-player-p)))
  (when subed--point-was-synced
    (subed-disable-sync-point-to-player :quiet))
  (when subed--point-was-synced
    (setq subed--point-sync-delay-after-motion-timer
          (run-at-time subed-point-sync-delay-after-motion nil
                       (lambda ()
                         (setq subed--point-sync-delay-after-motion-timer nil)
                         (subed-enable-sync-point-to-player :quiet))))))


;;; Sync player-to-point

(defun subed-sync-player-to-point-p ()
  "Whether playback position jumps to subtitle at point."
  (member #'subed--sync-player-to-point subed-subtitle-motion-hook))

(defun subed-enable-sync-player-to-point (&optional quiet)
  "Automatically seek player to subtitle at point.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (unless (subed-sync-player-to-point-p)
    (subed--sync-player-to-point)
    (add-hook 'subed-subtitle-motion-hook #'subed--sync-player-to-point :append :local)
    (subed-debug "Enabled syncing playback position to point: %s" subed-subtitle-motion-hook)
    (unless quiet
      (message "Enabled syncing playback position to point"))))

(defun subed-disable-sync-player-to-point (&optional quiet)
  "Do not automatically seek player to subtitle at point.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (when (subed-sync-player-to-point-p)
    (remove-hook 'subed-subtitle-motion-hook #'subed--sync-player-to-point :local)
    (subed-debug "Disabled syncing playback position to point: %s" subed-subtitle-motion-hook)
    (unless quiet
      (message "Disabled syncing playback position to point"))))

(defun subed-toggle-sync-player-to-point ()
  "Enable or disable automatically seeking player to subtitle at point."
  (interactive)
  (if (subed-sync-player-to-point-p)
      (subed-disable-sync-player-to-point)
    (subed-enable-sync-player-to-point)))

(defun subed--sync-player-to-point ()
  "Seek player to currently focused subtitle."
  (subed-debug "Seeking player to subtitle at point %s" (point))
  (let ((cur-sub-start (subed-subtitle-msecs-start))
        (cur-sub-stop (subed-subtitle-msecs-stop)))
    (when (and subed-mpv-playback-position cur-sub-start cur-sub-stop
               (or (< subed-mpv-playback-position cur-sub-start)
                   (> subed-mpv-playback-position cur-sub-stop)))
      (subed-mpv-jump cur-sub-start)
      (subed-debug "Synchronized playback position to point: #%s -> %s"
                   (subed-subtitle-id) cur-sub-start))))


;;; Loop over single subtitle

(defun subed-loop-over-current-subtitle-p ()
  "Whether the player is looping over the current subtitle."
  (or subed--subtitle-loop-start subed--subtitle-loop-stop))

(defvar-local subed--enable-point-to-player-sync-after-disabling-loop nil)

(defun subed-enable-loop-over-current-subtitle (&optional quiet)
  "Enable looping over the current subtitle in the player.

If enabled, point-to-player synchronization is disabled and
re-enabled again when `subed-disable-loop-over-current-subtitle'
is called.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (unless (subed-loop-over-current-subtitle-p)
    (subed--set-subtitle-loop (subed-subtitle-id))
    (add-hook 'subed-mpv-playback-position-hook #'subed--ensure-subtitle-loop :append :local)
    (add-hook 'subed-subtitle-motion-hook #'subed--set-subtitle-loop :append :local)
    (subed-debug "Enabling loop: %s - %s" subed--subtitle-loop-start subed--subtitle-loop-stop)
    (when (subed-sync-point-to-player-p)
      (subed-disable-sync-point-to-player)
      (setq subed--enable-point-to-player-sync-after-disabling-loop t))
    (unless quiet
      (message "Enabled looping over current subtitle"))))

(defun subed-disable-loop-over-current-subtitle (&optional quiet)
  "Disable looping over the current subtitle in the player.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (when (subed-loop-over-current-subtitle-p)
    (remove-hook 'subed-mpv-playback-position-hook #'subed--ensure-subtitle-loop :local)
    (remove-hook 'subed-subtitle-motion-hook #'subed--set-subtitle-loop :local)
    (setq subed--subtitle-loop-start nil
          subed--subtitle-loop-stop nil)
    (subed-debug "Disabling loop: %s - %s" subed--subtitle-loop-start subed--subtitle-loop-stop)
    (when subed--enable-point-to-player-sync-after-disabling-loop
      (subed-enable-sync-point-to-player)
      (setq subed--enable-point-to-player-sync-after-disabling-loop nil))
    (unless quiet
      (message "Disabled looping over current subtitle"))))

(defun subed-toggle-loop-over-current-subtitle (&optional quiet)
  "Enable or disable looping over the current subtitle in the player.

If QUIET is non-nil, do not display a message in the minibuffer."
  (interactive)
  (if (subed-loop-over-current-subtitle-p)
      (subed-disable-loop-over-current-subtitle quiet)
    (subed-enable-loop-over-current-subtitle quiet)))

(defun subed--set-subtitle-loop (&optional sub-id)
  "Set loop positions to start/stop time of SUB-ID or current subtitle."
  (let ((msecs-start (subed-subtitle-msecs-start sub-id))
        (msecs-stop (subed-subtitle-msecs-stop sub-id)))
    (when (and msecs-start msecs-stop)
      (setq subed--subtitle-loop-start (- msecs-start (* subed-loop-seconds-before 1000))
            subed--subtitle-loop-stop (+ msecs-stop (* subed-loop-seconds-after 1000)))
      (subed-debug "Set loop: %s - %s"
                   (subed-msecs-to-timestamp subed--subtitle-loop-start)
                   (subed-msecs-to-timestamp subed--subtitle-loop-stop))
      (message "Looping over %s - %s"
               (subed-msecs-to-timestamp subed--subtitle-loop-start)
               (subed-msecs-to-timestamp subed--subtitle-loop-stop)))))

(defun subed--ensure-subtitle-loop (cur-msecs)
  "Jump to current subtitle start time if CUR-MSECS is after stop time."
  (when (and subed--subtitle-loop-start subed--subtitle-loop-stop
             subed-mpv-is-playing)
    (when (or (< cur-msecs subed--subtitle-loop-start)
              (> cur-msecs subed--subtitle-loop-stop))
      (subed-debug "%s -> Looping over %s - %s"
                   (subed-msecs-to-timestamp cur-msecs)
                   (subed-msecs-to-timestamp subed--subtitle-loop-start)
                   (subed-msecs-to-timestamp subed--subtitle-loop-stop))
      (subed-mpv-jump subed--subtitle-loop-start))))


;;; Pause player while the user is editing

(defun subed-pause-while-typing-p ()
  "Whether player is automatically paused or slowed down during editing.

See `subed-playback-speed-while-typing' and
`subed-playback-speed-while-not-typing'."
  (member #'subed--pause-while-typing after-change-functions))

(defun subed-enable-pause-while-typing (&optional quiet)
  "Pause player while the user is editing a subtitle.

After `subed-unpause-after-typing-delay' seconds, playback is
resumed automatically unless the player was paused already.

If QUIET is non-nil, do not display a message in the minibuffer."
  (unless (subed-pause-while-typing-p)
    (add-hook 'after-change-functions #'subed--pause-while-typing :append :local)
    (unless quiet
      (subed-debug "%S" subed-playback-speed-while-typing)
      (if (<= subed-playback-speed-while-typing 0)
          (message "Playback will pause while subtitle texts are edited")
        (message "Playback will slow down by %s while subtitle texts are edited"
                 subed-playback-speed-while-typing)))))

(defun subed-disable-pause-while-typing (&optional quiet)
  "Do not automatically pause player while the user is editing the buffer.

If QUIET is non-nil, do not display a message in the minibuffer."
  (when (subed-pause-while-typing-p)
    (remove-hook 'after-change-functions #'subed--pause-while-typing :local)
    (unless quiet
      (message "Playback speed will not change while subtitle texts are edited"))))

(defun subed-toggle-pause-while-typing ()
  "Enable or disable auto-pausing while the user is editing the buffer."
  (interactive)
  (if (subed-pause-while-typing-p)
      (subed-disable-pause-while-typing)
    (subed-enable-pause-while-typing)))

(defvar-local subed--unpause-after-typing-timer nil)
(defun subed--pause-while-typing (&rest _args)
  "Pause or slow down playback for `subed-unpause-after-typing-delay' seconds.

This function is meant to be an item in `after-change-functions'
and therefore gets ARGS, which is ignored."
  (when subed--unpause-after-typing-timer
    (cancel-timer subed--unpause-after-typing-timer))
  (when (or subed-mpv-is-playing subed--player-is-auto-paused)
    (if (<= subed-playback-speed-while-typing 0)
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
  "Find video file with same base name as the opened file in the buffer.

The file extension of the return value of the function
`buffer-file-name' is replaced with each item in
`subed-video-extensions' and the first existing file is returned.

Language codes are also handled; e.g. \"foo.en.srt\" or
\"foo.estonian.srt\" -> \"foo.{mkv,mp4,...}\" (this actually
simply removes the extension from the extension-stripped file
name).

Return nil if function `buffer-file-name' returns nil."
  (when (buffer-file-name)
    (catch 'found-videofile
      (let* ((file-base (file-name-sans-extension (buffer-file-name)))
	         (file-stem (file-name-sans-extension file-base)))
	    (dolist (extension subed-video-extensions)
	      (let ((file-base-video (format "%s.%s" file-base extension))
		        (file-stem-video (format "%s.%s" file-stem extension)))
	        (when (file-exists-p file-base-video)
	          (throw 'found-videofile file-base-video))
	        (when (file-exists-p file-stem-video)
	          (throw 'found-videofile file-stem-video))))))))

;;; Inserting HTML-like tags

(defvar subed--html-tag-history nil
  "History of HTML-like tags in subtitles.")
(defvar subed--html-attr-history nil
  "History of HTML-like attributes in subtitles.")

(defun subed-insert-html-tag (begin end tag &optional attributes)
  "Insert a pair of HTML-like tags around the region.
If region is not active, insert a pair of tags and put the point
between them.  If called with a prefix argument, also ask for
attribute(s)."
  (interactive (let* ((region-p (use-region-p))
		      (begin (if region-p (region-beginning) (point)))
		      (end (if region-p (region-end) (point)))
		      (tag (read-string "Tag: " nil 'subed--html-tag-history))
		      (attributes (when current-prefix-arg
				    (read-string "Attribute(s): " nil 'subed--html-attr-history))))
		 (list begin end tag attributes)))
  (save-excursion
    (push (point) buffer-undo-list)
    (goto-char end)
    (insert "</" tag ">")
    (goto-char begin)
    (insert-before-markers "<" tag)
    (when attributes (insert-before-markers " " attributes))
    (insert-before-markers ">")))

(defun subed-insert-html-tag-italic (begin end)
  "Insert a pair of <i> tags at point or around the region."
  (interactive (let* ((region-p (use-region-p))
		      (begin (if region-p (region-beginning) (point)))
		      (end (if region-p (region-end) (point))))
		 (list begin end)))
  (subed-insert-html-tag begin end "i"))

(defun subed-insert-html-tag-bold (begin end)
  "Insert a pair of <b> tags at point or around the region."
  (interactive (let* ((region-p (use-region-p))
		      (begin (if region-p (region-beginning) (point)))
		      (end (if region-p (region-end) (point))))
		 (list begin end)))
  (subed-insert-html-tag begin end "b"))

;;; Characters per second computation

(defun subed-show-cps-p ()
  "Whether CPS is shown for the current subtitle."
  (member #'subed--update-cps-overlay after-change-functions))

(defun subed-enable-show-cps (&optional quiet)
  "Enable showing CPS next to the subtitle heading."
  (interactive "p")
  (add-hook 'after-change-functions #'subed--update-cps-overlay nil t)
  (add-hook 'subed-subtitle-motion-hook #'subed--move-cps-overlay-to-current-subtitle nil t)
  (add-hook 'after-save-hook #'subed--move-cps-overlay-to-current-subtitle nil t)
  (unless quiet
    (message "Enabled showing characters per second")))

(defun subed-disable-show-cps (&optional quiet)
  "Disable showing CPS next to the subtitle heading."
  (interactive)
  (remove-hook 'after-change-functions #'subed--update-cps-overlay t)
  (remove-hook 'subed-subtitle-motion-hook #'subed--move-cps-overlay-to-current-subtitle t)
  (remove-hook 'after-save-hook #'subed--move-cps-overlay-to-current-subtitle t)
  (unless quiet
    (message "Disabled showing characters per second")))

(defun subed-toggle-show-cps ()
  "Enable or disable showing CPS next to the subtitle heading."
  (interactive)
  (if (subed-show-cps-p)
      (subed-disable-show-cps)
    (subed-enable-show-cps)))

(defvar subed-transform-for-cps #'subed--strip-tags)

(defun subed--strip-tags (string)
  "Strip HTML-like tags from STRING."
  (with-temp-buffer
    (insert string)
    (goto-char 1)
    (while (re-search-forward "</?[^>]+>" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (buffer-string)))

(defun subed-calculate-cps (&optional print-message)
  "Calculate characters per second of the current subtitle."
  (interactive "p")
  (save-match-data
    (let* ((msecs-start (ignore-errors (subed-subtitle-msecs-start)))
	   (msecs-stop (ignore-errors (subed-subtitle-msecs-stop)))
	   (text (if (fboundp subed-transform-for-cps)
		     (funcall subed-transform-for-cps (subed-subtitle-text))
		   (subed-subtitle-text)))
	   (length (length text))
	   (cps (when (and (numberp msecs-stop)
			   (numberp msecs-start))
		  (/ length 0.001 (- msecs-stop msecs-start)))))
      (if (and print-message cps)
	  (message "%.1f characters per second" cps)
	cps))))

(defvar-local subed--cps-overlay nil)

(defun subed--move-cps-overlay-to-current-subtitle ()
  "Move the CPS overlay to the current subtitle."
  (let* ((begin (save-excursion
		  (subed-jump-to-subtitle-time-start)
		  (point)))
	 (end (save-excursion
		(goto-char begin)
		(line-end-position))))
    (if (overlayp subed--cps-overlay)
	(move-overlay subed--cps-overlay begin end (current-buffer))
      (setq subed--cps-overlay (make-overlay begin end)))
    (subed--update-cps-overlay)))

(defun subed--update-cps-overlay (&rest _rest)
  "Update the CPS overlay.
This accepts and ignores any number of arguments so that it can
be run in `after-change-functions'."
  (when (overlayp subed--cps-overlay)
    (let ((cps (subed-calculate-cps)))
      (when (numberp cps)
        (overlay-put
         subed--cps-overlay
         'after-string
         (propertize (format " %.1f CPS" cps) 'face 'shadow 'display '(height 0.9)))))))

;;; handle overlapping subtitle timecodes

;; NB: runs in a hook, so this version cannot send prefix arg to
;; subed-sanitize-overlaps
(defun subed-check-overlaps ()
  "Test all subtitles for overlapping timecodes.

Creates a list of the ids of overlapping subtitles, moves point
to the to the end time of the first one, and prompts to trim
them. Designed to be run as a subed-mode-hook."
  (interactive)
  (let ((overlap-ids ()))
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion (subed-forward-subtitle-time-start))
        (let ((next-sub-start-time (save-excursion
                                     (subed-forward-subtitle-time-start)
                                     (subed-subtitle-msecs-start))))
          (if (>= (subed-subtitle-msecs-stop) next-sub-start-time)
              (progn (subed-jump-to-subtitle-id)
                     (push (string-to-number (word-at-point))
                           overlap-ids))))
        (subed-forward-subtitle-time-start)))
    (when overlap-ids
      (setq overlap-ids (nreverse overlap-ids))
      (subed-jump-to-subtitle-id (car overlap-ids))
      (subed-jump-to-subtitle-time-stop)
      (when (yes-or-no-p "Overlapping subtitles found. Trim them? ")
        (subed-sanitize-overlaps)))))

(defun subed-sanitize-overlaps (&optional arg)
  "Adjust all overlapping times in current file.

Uses either `subed-trim-overlap-start-times' or
`subed-trim-overlapping-end-times', the latter being the default.
See `subed-trim-overlapping-subtitle-trim-start' to customize
this option.

With a non-numerical prefix ARG, or if
`subed-trim-overlapping-use-subed-subtitle-spacing' is t, make a gap the
between subtitles the length of `subed-subtitle-spacing'. With a numerical
prefix ARG, make the gap that many milliseconds."
  (interactive "P")
  (let ((cpsp (subed-show-cps-p)))
    (when cpsp
      (subed-disable-show-cps t))
    (switch-to-buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion (subed-forward-subtitle-time-start))
        (if subed-trim-overlapping-subtitle-trim-start
            (subed-trim-overlap-start-time arg)
          (subed-trim-overlap-end-time arg))
        (subed-forward-subtitle-time-start)))
    (when cpsp
      (subed-enable-show-cps t))))

(defun subed-trim-overlap-end-time (&optional arg)
  "Check if end time of current subtitle is after start time of next.

If so, trim the end time of current subtitle to 1 millisecond
less than the start time of the next one.

With a non-numerical prefix ARG, or if
`subed-trim-overlapping-use-subed-subtitle-spacing' is t, make a
gap the between subtitles the length of `subed-subtitle-spacing'.
With a numerical prefix ARG, make the gap that many
milliseconds."
  (interactive "P")
  (let ((next-sub-start-time (save-excursion
                               (subed-forward-subtitle-time-start)
                               (subed-subtitle-msecs-start))))
    (if (>= (subed-subtitle-msecs-stop) next-sub-start-time)
        (subed-set-subtitle-time-stop
         (cond
          ;; if numeric prefix arg, use it as gap in ms:
          ((integerp arg)
           (- next-sub-start-time arg))
          ;; if plain C-u or custom option set:
          ((or (consp arg)
               subed-trim-overlapping-use-subed-subtitle-spacing)
           (- next-sub-start-time subed-subtitle-spacing))
          ;; else just make 1 ms difference:
          (t
           (1- next-sub-start-time)))))))

(defun subed-trim-overlap-start-time (&optional arg)
  "Check if end time of current subtitle is after start time of next.

If so, trim the start time of current subtitle to 1 millisecond
less than the end time of the current one.

With a non-numerical prefix ARG, or if
`subed-trim-overlapping-use-subed-subtitle-spacing' is t, make a
gap the between subtitles the length of `subed-subtitle-spacing'.
With a numerical prefix ARG make the gap that many miliseconds."
  (interactive "P")
  (let ((this-sub-stop-time (subed-subtitle-msecs-stop))
        (next-sub-start-time (save-excursion
                               (subed-forward-subtitle-time-start)
                               (subed-subtitle-msecs-start))))
    (if (>= this-sub-stop-time next-sub-start-time)
        (save-excursion
          (subed-forward-subtitle-time-start)
          (subed-set-subtitle-time-start
           (cond
            ;; if numeric prefix arg, use it as gap in ms:
            ((integerp arg)
             (+ this-sub-stop-time arg))
            ;; if plain C-u or custom option set:
            ((or (consp arg)
                 subed-trim-overlapping-use-subed-subtitle-spacing)
             (+ this-sub-stop-time subed-subtitle-spacing))
            ;; else just make 1 ms difference:
            (t
             (1+ this-sub-stop-time))))))))

(defun subed-sort ()
  "Sanitize, then sort subtitles by start time and re-number them."
  (interactive)
  (atomic-change-group
    (subed-sanitize)
    (when subed-trim-overlapping-subtitle-times-on-save
      (subed-sanitize-overlaps))
    (subed-validate)
    (subed-save-excursion
     (goto-char (point-min))
     (sort-subr nil
                ;; nextrecfun (move to next record/subtitle or to end-of-buffer
                ;; if there are no more records)
                (lambda () (unless (subed-forward-subtitle-id)
                             (goto-char (point-max))))
                ;; endrecfun (move to end of current record/subtitle)
                #'subed-jump-to-subtitle-end
                ;; startkeyfun (return sort value of current record/subtitle)
                #'subed-subtitle-msecs-start))
    (subed-regenerate-ids)))

(provide 'subed-common)
;;; subed-common.el ends here
