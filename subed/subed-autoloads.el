;;; subed-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "subed" "subed.el" (0 0 0 0))
;;; Generated autoloads from subed.el

(autoload 'subed-mode "subed" "\
Major mode for editing subtitles.

subed uses the following terminology when it comes to changes in
subtitles' timestamps:

Adjust - Increase or decrease start or stop time of a subtitle
  Move - Increase or decrease start and stop time of a subtitle
         by the same amount
 Shift - Increase or decrease start and stop time of the current
         and all following subtitles by the same amount

Key bindings:
\\{subed-mode-map}

\(fn)" t nil)

(register-definition-prefixes "subed" '("subed-"))

;;;***

;;;### (autoloads nil "subed-ass" "subed-ass.el" (0 0 0 0))
;;; Generated autoloads from subed-ass.el

(autoload 'subed-ass-mode "subed-ass" "\
Major mode for editing Advanced SubStation Alpha subtitle files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.ass\\'" . subed-ass-mode))

(register-definition-prefixes "subed-ass" '("subed-ass-"))

;;;***

;;;### (autoloads nil "subed-common" "subed-common.el" (0 0 0 0))
;;; Generated autoloads from subed-common.el

(register-definition-prefixes "subed-common" '("subed-"))

;;;***

;;;### (autoloads nil "subed-config" "subed-config.el" (0 0 0 0))
;;; Generated autoloads from subed-config.el

(register-definition-prefixes "subed-config" '("subed-"))

;;;***

;;;### (autoloads nil "subed-debug" "subed-debug.el" (0 0 0 0))
;;; Generated autoloads from subed-debug.el

(register-definition-prefixes "subed-debug" '("subed-"))

;;;***

;;;### (autoloads nil "subed-mpv" "subed-mpv.el" (0 0 0 0))
;;; Generated autoloads from subed-mpv.el

(register-definition-prefixes "subed-mpv" '("subed-mpv-"))

;;;***

;;;### (autoloads nil "subed-srt" "subed-srt.el" (0 0 0 0))
;;; Generated autoloads from subed-srt.el

(autoload 'subed-srt-mode "subed-srt" "\
Major mode for editing SubRip subtitle files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.srt\\'" . subed-srt-mode))

(register-definition-prefixes "subed-srt" '("subed-srt-"))

;;;***

;;;### (autoloads nil "subed-vtt" "subed-vtt.el" (0 0 0 0))
;;; Generated autoloads from subed-vtt.el

(autoload 'subed-vtt-mode "subed-vtt" "\
Major mode for editing WebVTT subtitle files.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.vtt\\'" . subed-vtt-mode))

(register-definition-prefixes "subed-vtt" '("subed-vtt-"))

;;;***

(provide 'subed-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; subed-autoloads.el ends here
