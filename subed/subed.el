;;; subed.el --- A major mode for editing subtitles  -*- lexical-binding: t; -*-

;; Version: 0.0
;; Keywords: convenience, files, hypermedia, multimedia
;; URL: https://github.com/rndusr/subed
;; Package-Requires: ((emacs "25"))

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
;; subed is a major mode for editing subtitles with Emacs and mpv.  See
;; README.org or https://github.com/rndusr/subed for more information.

;;; Code:

(require 'subed-config)
(require 'subed-common)
(require 'subed-srt)
(require 'subed-mpv)


(defalias 'subed-subtitle-id #'subed-srt--subtitle-id)
(defalias 'subed-subtitle-id-max #'subed-srt--subtitle-id-max)
(defalias 'subed-subtitle-msecs-start #'subed-srt--subtitle-msecs-start)
(defalias 'subed-subtitle-msecs-stop #'subed-srt--subtitle-msecs-stop)
(defalias 'subed-subtitle-text #'subed-srt--subtitle-text)
(defalias 'subed-subtitle-relative-point #'subed-srt--subtitle-relative-point)

(defalias 'subed-jump-to-subtitle-id #'subed-srt--jump-to-subtitle-id)
(defalias 'subed-jump-to-subtitle-time-start #'subed-srt--jump-to-subtitle-time-start)
(defalias 'subed-jump-to-subtitle-time-stop #'subed-srt--jump-to-subtitle-time-stop)
(defalias 'subed-jump-to-subtitle-text #'subed-srt--jump-to-subtitle-text)
(defalias 'subed-jump-to-subtitle-end #'subed-srt--jump-to-subtitle-end)
(defalias 'subed-jump-to-subtitle-id-at-msecs #'subed-srt--jump-to-subtitle-id-at-msecs)
(defalias 'subed-jump-to-subtitle-text-at-msecs #'subed-srt--jump-to-subtitle-text-at-msecs)

(defalias 'subed-forward-subtitle-id #'subed-srt--forward-subtitle-id)
(defalias 'subed-backward-subtitle-id #'subed-srt--backward-subtitle-id)
(defalias 'subed-forward-subtitle-text #'subed-srt--forward-subtitle-text)
(defalias 'subed-backward-subtitle-text #'subed-srt--backward-subtitle-text)
(defalias 'subed-forward-subtitle-time-start #'subed-srt--forward-subtitle-time-start)
(defalias 'subed-backward-subtitle-time-start #'subed-srt--backward-subtitle-time-start)
(defalias 'subed-forward-subtitle-time-stop #'subed-srt--forward-subtitle-time-stop)
(defalias 'subed-backward-subtitle-time-stop #'subed-srt--backward-subtitle-time-stop)

(defalias 'subed-set-subtitle-time-start #'subed-srt--set-subtitle-time-start)
(defalias 'subed-set-subtitle-time-stop #'subed-srt--set-subtitle-time-stop)
(defalias 'subed-prepend-subtitle #'subed-srt--prepend-subtitle)
(defalias 'subed-append-subtitle #'subed-srt--append-subtitle)
(defalias 'subed-kill-subtitle #'subed-srt--kill-subtitle)
(defalias 'subed-sanitize #'subed-srt--sanitize)
(defalias 'subed-regenerate-ids #'subed-srt--regenerate-ids)
(defalias 'subed-regenerate-ids-soon #'subed-srt--regenerate-ids-soon)
(defalias 'subed-sort #'subed-srt--sort)


;;;###autoload
(defun subed-mode-enable ()
  "Enable subed mode."
  (interactive)
  (kill-all-local-variables)
  (setq-local font-lock-defaults '(subed-font-lock-keywords))
  (setq-local paragraph-start "^[[:alnum:]\n]+")
  (setq-local paragraph-separate "\n\n")
  (use-local-map subed-mode-map)
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
  (setq major-mode 'subed-mode
        mode-name "subed")
  (setq subed-mode--enabled-p t)
  (run-mode-hooks 'subed-mode-hook))

(defun subed-mode-disable ()
  "Disable subed mode."
  (interactive)
  (subed-disable-pause-while-typing :quiet)
  (subed-disable-sync-point-to-player :quiet)
  (subed-disable-sync-player-to-point :quiet)
  (subed-disable-replay-adjusted-subtitle :quiet)
  (subed-mpv-kill)
  (subed-disable-debugging)
  (kill-all-local-variables)
  (remove-hook 'post-command-hook #'subed--post-command-handler :local)
  (remove-hook 'before-save-hook #'subed-sort :local)
  (remove-hook 'after-save-hook #'subed-mpv-reload-subtitles :local)
  (remove-hook 'kill-buffer-hook #'subed-mpv-kill :local)
  (setq subed-mode--enabled-p nil))

;;;###autoload
(defun subed-mode ()
  "Major mode for editing subtitles.

This function enables or disables `subed-mode'.  See also
`subed-mode-enable' and `subed-mode-disable'.

Key bindings:
\\{subed-mode-map}"
  (interactive)
  ;; Use 'enabled property of this function to store enabled/disabled status
  (if subed-mode--enabled-p
      (subed-mode-disable)
    (subed-mode-enable)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.srt\\'" . subed-mode-enable))

(provide 'subed)
;;; subed.el ends here
