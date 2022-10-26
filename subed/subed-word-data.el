;;; subed-word-data.el --- Use word-level timing data when splitting subtitles  -*- lexical-binding: t; -*-

;;; License:
;;
;; Copyright (C) 2022  Sacha Chua

;; Author: Sacha Chua <sacha@sachachua.com>
;; Keywords: multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file parses timing data such as the ones you get from YouTube
;; .srv2 and tries to match the timing data with the remaining text in
;; the current subtitle in order to determine the word timestamp for
;; splitting the subtitle.

;; To try to automatically load word data from a similarly-named file
;; in the buffer, add this to your configuration:

;; (with-eval-after-load 'subed
;;   (add-hook 'subed-mode-hook 'subed-word-data-load-maybe))

;;; Code:

(require 'xml)
(require 'dom)

(defvar-local subed-word-data--cache nil
  "Word-level timing in the form ((start . ms) (end . ms) (text . ms))")

(defun subed-word-data--extract-words-from-srv2 (data)
  "Extract the timing from DATA in SRV2 format.
Return a list of ((start . ?), (end . ?) (text . ?))."
  (when (stringp data)
    (with-temp-buffer (insert data) (setq data (xml-parse-region))))
  (let* ((text-elements (reverse (dom-by-tag data 'text)))
         (last-start
          (and text-elements
               (+ (string-to-number
                   (alist-get 't (xml-node-attributes (car text-elements))))
                  (string-to-number (alist-get 'd (xml-node-attributes (car text-elements))))))))
    (reverse
     (mapcar #'(lambda (element)
                 (let ((rec (list (cons 'start (string-to-number (alist-get 't (xml-node-attributes element))))
                                  (cons 'end last-start)
                                  (cons 'text
                                         (replace-regexp-in-string "&#39;" "'"
                                                                   (car (xml-node-children element)))
                                         ))))
                   (setq last-start (alist-get 'start rec))
                   rec))
               text-elements))))

(defun subed-word-data--load (data)
  "Load word-level timing from DATA.
For now, only SRV2 files are supported."
  (setq-local subed-word-data--cache data)
  (add-hook 'subed-split-subtitle-timestamp-functions #'subed-word-data-split-at-word-timestamp -5 t))

(defun subed-word-data-load-from-file (file)
  "Load word-level timing from FILE.
For now, only SRV2 files are supported."
  (interactive "fFile: ")
  (subed-word-data--load (subed-word-data--extract-words-from-srv2 (xml-parse-file file))))

(defun subed-word-data-load-from-string (string)
  "Load word-level timing from STRING.
For now, only SRV2 files are supported."
  (subed-word-data--load (subed-word-data--extract-words-from-srv2 string)))

(defvar subed-word-data-extensions '(".en.srv2" ".srv2") "Extensions to search for word data.")

(defun subed-word-data-load-maybe ()
  "Load word data if available. Suitable for adding to `subed-mode-hook'."
  (when (buffer-file-name)
    (let (file)
      (catch 'found
        (mapc (lambda (ext)
                (when (file-exists-p (concat (file-name-sans-extension (buffer-file-name)) ext))
                  (setq file (concat (file-name-sans-extension (buffer-file-name)) ext))
                  (throw 'found)))
              subed-word-data-extensions))
      (when file
        (subed-word-data-load-from-file file)
        (message "Word data loaded.")))))

(defvar subed-word-data-normalizing-functions '(subed-word-data-normalize-word-default)
  "Functions to run to normalize words before comparison.")

(defun subed-word-data-normalize-word-default (s)
  "Downcase S and remove non-alphanumeric characters for comparison."
  (replace-regexp-in-string "[^[:alnum:]]" "" (downcase s)))

(defun subed-word-data-normalize-word (word)
  "Normalize WORD to make it easier to compare."
  (mapc (lambda (func)
          (setq word (funcall func word)))
        subed-word-data-normalizing-functions)
  word)

(defun subed-word-data-compare-normalized-string= (word1 word2)
  "Compare two words and return t if they are the same after normalization."
  (string= (subed-word-data-normalize-word word1)
           (subed-word-data-normalize-word word2)))

(defvar subed-word-data-compare 'subed-word-data-compare-normalized-string=
  "Function to use to compare.")

(defun subed-word-data--look-up-word ()
  "Find the word timing that matches the one at point (approximately)."
  (save-excursion
    (skip-syntax-backward "w")
    (let* ((end (subed-subtitle-msecs-stop))
           (start (subed-subtitle-msecs-start))
           (remaining-words (split-string
                             (buffer-substring
                              (point)
                              (or (subed-jump-to-subtitle-end) (point)))))
           (words (if remaining-words
                      (reverse (seq-filter
                                (lambda (o)
                                  (and (<= (alist-get 'end o) end)
                                       (>= (alist-get 'start o) start)
                                       (not (string-match "^\n*$" (alist-get 'text o)))))
                                subed-word-data--cache))))
           (offset 0)
           (done (null remaining-words))
           candidate)
      (while (not done)
        (setq candidate (elt words (+ (1- (length remaining-words)) offset)))
        (cond
         ((and candidate (funcall subed-word-data-compare
                                  (car remaining-words)
                                  (alist-get 'text candidate)))
          (setq done t))
         ((> offset (length words)) (setq done t))
         ((> offset 0) (setq offset (- offset)))
         (t (setq offset (1+ (- offset))))))
      candidate)))

(defun subed-word-data-split-at-word-timestamp ()
  "Return the starting timestamp if the word is found."
  (when subed-word-data--cache
    (let ((time (assoc-default 'start (subed-word-data--look-up-word))))
      (when time (- time subed-subtitle-spacing)))))

(provide 'subed-word-data)
;;; subed-word-data.el ends here
