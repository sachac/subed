(add-to-list 'load-path (expand-file-name "./subed")) ;FIXME: ??
(when (require 'undercover nil t)
  (undercover "./subed/*.el" (:report-format 'simplecov) (:send-report nil)))

