(add-to-list 'load-path "./subed")
(add-to-list 'load-path "../subed")
(when (require 'undercover nil t)
  (undercover "./subed/*.el" "../subed/*.el" (:report-format 'simplecov) (:send-report nil)))

;; (load "subed/subed.el")
;; (load "subed/subed-srt.el")
;; (load "subed/subed.el")

