(add-to-list 'load-path (expand-file-name "./subed")) ;FIXME: ??
(when (require 'undercover nil t)
	(setq undercover-force-coverage t)
  (undercover "./subed/*.el" (:report-format 'simplecov) (:send-report nil)
							(:merge-report t)))

