;;; subed-debug.el --- Debugging functions  -*- lexical-binding: t; -*-

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
;; Debugging-related functions.

;;; Code:

(require 'subed-config)

(defun subed-enable-debugging ()
  "Hide debugging messages and set `debug-on-error' to nil."
  (interactive)
  (unless subed-debugging-enabled-p
    (setq subed-debugging-enabled-p t
          debug-on-error t)
    (let ((debug-buffer (get-buffer-create subed-debug-buffer))
          (debug-window (or (get-buffer-window subed-debug-buffer)
                            (split-window-horizontally (max 40 (floor (* 0.3 (window-width))))))))
      (set-window-buffer debug-window debug-buffer)
      (with-current-buffer debug-buffer
        (buffer-disable-undo)
        (setq-local buffer-read-only t)))
    (add-hook 'kill-buffer-hook #'subed-disable-debugging :append :local)))

(defun subed-disable-debugging ()
  "Display debugging messages in separate window and set `debug-on-error' to t."
  (interactive)
  (when subed-debugging-enabled-p
    (setq subed-debugging-enabled-p nil
          debug-on-error nil)
    (let ((debug-window (get-buffer-window subed-debug-buffer)))
      (when debug-window
        (delete-window debug-window)))
    (remove-hook 'kill-buffer-hook #'subed-disable-debugging :local)))

(defun subed-toggle-debugging ()
  "Display or hide debugging messages in separate window.
Set `debug-on-error' to t or nil."
  (interactive)
  (if subed-debugging-enabled-p
      (subed-disable-debugging)
    (subed-enable-debugging)))

(defun subed-debug (msg &rest args)
  "Pass MSG and ARGS to `format'.
Show the result in debugging buffer if it exists."
  (when (get-buffer subed-debug-buffer)
    (with-current-buffer (get-buffer-create subed-debug-buffer)
      (setq-local buffer-read-only nil)
      (insert (apply #'format (concat msg "\n") args))
      (setq-local buffer-read-only t)
      (let ((debug-window (get-buffer-window subed-debug-buffer)))
        (when debug-window
          (set-window-point debug-window (goto-char (point-max))))))))

(provide 'subed-debug)
;;; subed-debug.el ends here
