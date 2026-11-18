;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

(load-file "./tests/undercover-init.el")
(require 'subed)
(require 'subed-vtt)
(require 'org)
(require 'ol-subed)
(require 'subed-common)

(defvar mock-vtt-data
  "WEBVTT

00:01:01.000 --> 00:01:05.123
Foo.

00:02:02.234 --> 00:02:10.345
Bar.

00:03:03.45 --> 00:03:15.5
Baz.
")

(defmacro with-temp-vtt-buffer (&rest body)
  "Call `subed-vtt--init' in temporary buffer before running BODY."
  `(with-temp-buffer
     (subed-vtt-mode)
     (progn ,@body)))

(describe "ol-subed"
  (describe "outside any subtitle"
    (it "stores a link to the whole file, then follows it."
      (let ((temp-file (make-temp-file "ol-subed" nil ".vtt" mock-vtt-data))
            link)
        (with-current-buffer (find-file-noselect temp-file)
          (expect (setq link (org-store-link nil))
                  :to-equal (format "[[subed:%s]]" temp-file))
          (kill-buffer (current-buffer)))
        (org-link-open-from-string link)
        (expect (buffer-file-name) :to-equal temp-file)
        (delete-file temp-file))))
  (describe "inside a specific subtitle"
    (it "stores a link to the timestamp with the description, then follows it."
      (let ((temp-file (make-temp-file "ol-subed" nil ".vtt" mock-vtt-data))
            link)
        (with-current-buffer (find-file-noselect temp-file)
          (goto-char (point-min))
          (re-search-forward "Foo")
          (expect (setq link (org-store-link nil))
                  :to-equal (format "[[subed:%s::61000][Foo.]]" temp-file))
          (kill-buffer (current-buffer)))
        (org-link-open-from-string link)
        (expect (subed-subtitle-text) :to-equal "Foo.")
        (delete-file temp-file)))))
