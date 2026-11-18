;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

(load-file "./tests/undercover-init.el")
(require 'subed)
(require 'subed-vtt)
(require 'org)
(require 'ol-subed)
(require 'subed-common)

(defvar ol-subed-test-data
  '((nil 1000 1999 "A")
    (nil 2000 2999 "B")
    (nil 3000 3999 "C")))

(describe "ol-subed"
  (describe "outside any subtitle"
    (it "stores a link to the whole file."
      (let ((temp-file (make-temp-file "ol-subed" nil ".vtt" )))
        (subed-create-file temp-file ol-subed-test-data t)
        (with-current-buffer (find-file-noselect temp-file)
          (goto-char (point-min))
          (expect (org-store-link nil)
                  :to-equal (format "[[subed:%s]]" temp-file))
          (kill-buffer (current-buffer))
          (delete-file temp-file))))
    (it "opens the file"
      (let ((temp-file (make-temp-file "ol-subed" nil ".vtt" )))
        (subed-create-file temp-file ol-subed-test-data t)
        (org-link-open-from-string (format "[[subed:%s]]" temp-file))
        (expect (buffer-file-name) :to-equal temp-file)
        (delete-file temp-file))))

  (describe "inside a specific subtitle"
    (it "stores a link to the timestamp with the description."
      (let ((temp-file (make-temp-file "ol-subed" nil ".vtt")))
        (subed-create-file temp-file ol-subed-test-data t)
        (with-current-buffer (find-file-noselect temp-file)
          (goto-char (point-min))
          (re-search-forward "C")
          (expect (org-store-link nil)
                  :to-equal (format "[[subed:%s::3000][C]]" temp-file))
          (kill-buffer (current-buffer)))))
    (it "follows a link to a VTT"
      (let ((temp-file (make-temp-file "ol-subed" nil ".vtt")))
        (subed-create-file temp-file ol-subed-test-data t)
        (org-link-open-from-string (format "[[subed:%s::2000][Test]]" temp-file))
        (expect (subed-subtitle-text) :to-equal "B")
        (delete-file temp-file)))
    (it "follows a link to an SRT"
      (let ((temp-file (make-temp-file "ol-subed" nil ".srt")))
        (subed-create-file temp-file ol-subed-test-data t)
        (org-link-open-from-string (format "[[subed:%s::2000][Test]]" temp-file))
        (expect (subed-subtitle-text) :to-equal "B")
        (delete-file temp-file)))
    (it "follows a link to an ASS"
      (let ((temp-file (make-temp-file "ol-subed" nil ".ass")))
        (subed-create-file temp-file ol-subed-test-data t)
        (org-link-open-from-string (format "[[subed:%s::2000][Test]]" temp-file))
        (expect (subed-subtitle-text) :to-equal "B")
        (delete-file temp-file)))
    ))
