;;; ol-subed.el --- Links to subed subtitles         -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: multimedia

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;###autoload
(defun org-subed-store-link ()
  "Store a link to a subtitle."
  (when (derived-mode-p 'subed-mode)
    ;; The value is passed around using variable `org-store-link-plist'.
    (org-link-store-props
     :type       "subed"
     :start-ms   (subed-subtitle-msecs-start)
     :stop-ms    (subed-subtitle-msecs-stop)
     :comment    (subed-subtitle-comment)
     :text       (subed-subtitle-text)
     :description (or (subed-subtitle-comment) (subed-subtitle-text)))
    (org-link-add-props
		 :link (concat "subed:" (buffer-file-name) "::" (number-to-string (subed-subtitle-msecs-start))))
    org-store-link-plist))

;;;###autoload
(defun org-subed-open (path _)
  "Follow a subed link specified by PATH."
  (let ((parts (split-string path "::")))
    (find-file (car parts))
    (when (cadr parts)
      (subed-jump-to-subtitle-id-at-msecs (subed-timestamp-to-msecs (cadr parts))))
    (when (elt parts 2)
      (narrow-to-region
       (point)
       (and
        (subed-jump-to-subtitle-id-at-msecs (subed-timestamp-to-msecs (elt parts 2)))
        (subed-jump-to-subtitle-end))))))

;;;###autoload
(with-eval-after-load 'org
  (org-link-set-parameters "subed"
                           :store  #'org-subed-store-link
                           :follow #'org-subed-open))

(provide 'ol-subed)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ol-subed.el ends here
