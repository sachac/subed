;;; subed.el --- A major mode for editing subtitles  -*- lexical-binding: t; -*-

;; Version: 0.0.1
;; Keywords: convenience, files, hypermedia, multimedia
;; URL: https://github.com/rndusr/subed
;; Package-Requires: ((emacs "25.1"))

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
;; subed is a major mode for editing subtitles with Emacs and mpv.  See C-h f
;; subed-mode, the README.org file or https://github.com/rndusr/subed for more
;; information.

;;; Code:

(require 'subed-config)
(require 'subed-debug)
(require 'subed-common)
(require 'subed-srt)
(require 'subed-mpv)

(setq subed-mode-map
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
    (define-key subed-mode-map (kbd "M-i") #'subed-insert-subtitle)
    (define-key subed-mode-map (kbd "C-M-i") #'subed-insert-subtitle-adjacent)
    (define-key subed-mode-map (kbd "M-k") #'subed-kill-subtitle)
    (define-key subed-mode-map (kbd "M-s") #'subed-sort)
    (define-key subed-mode-map (kbd "M-SPC") #'subed-mpv-toggle-pause)
    (define-key subed-mode-map (kbd "C-c C-d") #'subed-toggle-debugging)
    (define-key subed-mode-map (kbd "C-c C-v") #'subed-mpv-find-video)
    (define-key subed-mode-map (kbd "C-c C-p") #'subed-toggle-pause-while-typing)
    (define-key subed-mode-map (kbd "C-c C-l") #'subed-toggle-loop-over-current-subtitle)
    (define-key subed-mode-map (kbd "C-c C-r") #'subed-toggle-replay-adjusted-subtitle)
    (define-key subed-mode-map (kbd "C-c [") #'subed-copy-player-pos-to-start-time)
    (define-key subed-mode-map (kbd "C-c ]") #'subed-copy-player-pos-to-stop-time)
    (define-key subed-mode-map (kbd "C-c .") #'subed-toggle-sync-point-to-player)
    (define-key subed-mode-map (kbd "C-c ,") #'subed-toggle-sync-player-to-point)
    subed-mode-map))


;;; Abstraction hack to support different subtitle formats
;;
;; We need subtitle format-specific functions for each individual buffer so it
;; is possible to open a .srt and a .sub file in the same Emacs session.
;; Buffer-local functions don't exist in Elisp, but we can store the format in a
;; buffer-local variable.
;;
;; `subed-mode-enable' runs a format-specific init function based on the file
;; extension.  The init function sets the buffer-local variable
;; `subed--subtitle-format' which is then used by generic functions to assemble
;; the names of format-specific functions on the fly (e.g. (concat "subed-"
;; subed-subtitle-format "--subtitle-id")).

(defvar subed--generic-function-suffixes
  (list "subtitle-id" "subtitle-id-max" "subtitle-id-at-msecs"
        "subtitle-msecs-start" "subtitle-msecs-stop"
        "subtitle-text" "subtitle-relative-point"
        "jump-to-subtitle-id" "jump-to-subtitle-id-at-msecs"
        "jump-to-subtitle-time-start" "jump-to-subtitle-time-stop"
        "jump-to-subtitle-text" "jump-to-subtitle-text-at-msecs"
        "jump-to-subtitle-end"
        "forward-subtitle-id" "backward-subtitle-id"
        "forward-subtitle-text" "backward-subtitle-text"
        "forward-subtitle-end" "backward-subtitle-end"
        "forward-subtitle-time-start" "backward-subtitle-time-start"
        "forward-subtitle-time-stop" "backward-subtitle-time-stop"
        "set-subtitle-time-start" "set-subtitle-time-stop"
        "prepend-subtitle" "append-subtitle" "kill-subtitle"
        "regenerate-ids" "regenerate-ids-soon"
        "sanitize" "validate" "sort"))

(defun subed--get-generic-func (func-suffix)
  "Return the generic/public function for FUNC-SUFFIX."
  (intern (concat "subed-" func-suffix)))

(defun subed--get-specific-func (func-suffix)
  "Return the format-specific function for the current buffer for FUNC-SUFFIX."
  (intern (concat "subed-" subed--subtitle-format "--" func-suffix)))

(defun subed--init ()
  "Call subtitle format-specific init function and (re-)alias generic functions."
  ;; Call format-specific init function based on file extension and
  ;; `subed--init-alist'.
  (let* ((file-ext (when (buffer-file-name)
                     (file-name-extension (buffer-file-name))))
         (init-func (alist-get file-ext subed--init-alist nil nil 'equal)))
    (if (functionp init-func)
        (funcall init-func)
      (error "Missing init function: %S" init-func))
    (unless subed--subtitle-format
      (error "%S failed to set buffer-local variable: subed--subtitle-format"
             init-func)))
  ;; Define generic functions like `subed-subtitle-text'.
  (cl-loop for func-suffix in subed--generic-function-suffixes do
           (let ((generic-func (subed--get-generic-func func-suffix))
                 (specific-func (subed--get-specific-func func-suffix)))
             (unless (functionp specific-func)
               (error "Missing subtitle format-specific function: %s" specific-func))
             (if (functionp specific-func)
               (let* ((argspec (help-function-arglist specific-func))
                      (argvars (seq-filter (lambda (argvar)
                                             (let ((first-char (substring (symbol-name argvar) 0 1)))
                                               (not (equal first-char "&"))))
                                           argspec)))
                 (defalias generic-func
                   `(lambda ,argspec
                      ,(interactive-form specific-func) ;; (interactive ...) or nil
                      (let (;; Get the format-specific function for the current
                            ;; buffer.  We must do this every time the generic
                            ;; function is called because the result depends on
                            ;; the buffer-local variable `subed--subtitle-format'.
                            (specific-func (subed--get-specific-func ,func-suffix))
                            ;; Turn the list of variable names into a list of
                            ;; corresponding values.
                            (argvals (mapcar 'eval ',argvars)))
                        (apply specific-func argvals)))
                   (documentation specific-func t)))))))

;;;###autoload
(define-derived-mode subed-mode text-mode "subed"
  "Major mode for editing subtitles.

subed uses the following terminology when it comes to changes in
subtitles' timestamps:

Adjust - Increase or decrease start or stop time of a subtitle
  Move - Increase or decrease start and stop time of a subtitle
         by the same amount
 Shift - Increase or decrease start and stop time of the current
         and all following subtitles by the same amount

Key bindings:
\\{subed-mode-map}"
  :group 'subed
  (subed--init)
  (add-hook 'post-command-hook #'subed--post-command-handler :append :local)
  (add-hook 'before-save-hook #'subed-sort :append :local)
  (add-hook 'after-save-hook #'subed-mpv-reload-subtitles :append :local)
  (add-hook 'kill-buffer-hook #'subed-mpv-kill :append :local)
  (add-hook 'kill-emacs-hook #'subed-mpv-kill :append :local)
  (when subed-auto-find-video
    (let ((video-file (subed-guess-video-file)))
      (when video-file
        (subed-debug "Auto-discovered video file: %s" video-file)
        (condition-case err
            (subed-mpv-find-video video-file)
          (error (message "%s -- Set subed-auto-find-video to nil to avoid this error."
                          (car (cdr err))))))))
  (subed-enable-pause-while-typing :quiet)
  (subed-enable-sync-point-to-player :quiet)
  (subed-enable-sync-player-to-point :quiet)
  (subed-enable-replay-adjusted-subtitle :quiet)
  (subed-enable-loop-over-current-subtitle :quiet))

;; Internally, supported formats are listed in `subed--init-alist', which
;; associates file extensions with format-specific init methods (e.g. "srt" ->
;; subed-srt--init).  Here we map each file extension as a regexp to
;; `subed-mode-enable', which will call the format-specific init method and do
;; generic init stuff.
;;;###autoload
(dolist (item subed--init-alist)
  (let ((file-ext-regex (car item)))
    (add-to-list 'auto-mode-alist (cons (concat "\\." file-ext-regex "\\'")
                                        'subed-mode))))

(provide 'subed)
;;; subed.el ends here
