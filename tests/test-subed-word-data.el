;; -*- lexical-binding: t; eval: (buttercup-minor-mode) -*-

(load-file "./tests/undercover-init.el")

(require 'subed-word-data)

(describe "subed-word-data"
  (it "gets word data from YouTube VTTs."
    (let ((words (subed-word-data--extract-words-from-youtube-vtt
                  "WEBVTT
Kind: captions
Language: en

00:00:02.459 --> 00:00:05.610 align:start position:0%

hi<00:00:03.459><c> welcome</c><00:00:03.850><c> to</c><00:00:03.999><c> another</c><00:00:04.149><c> episode</c><00:00:04.509><c> of</c><00:00:05.020><c> Emacs</c>

00:00:05.610 --> 00:00:05.620 align:start position:0%
hi welcome to another episode of Emacs


00:00:05.620 --> 00:00:07.860 align:start position:0%
hi welcome to another episode of Emacs
chat<00:00:05.950><c> i'm</c><00:00:06.160><c> sasha</c><00:00:06.520><c> schewe</c><00:00:06.939><c> and</c><00:00:07.149><c> today</c><00:00:07.450><c> we</c><00:00:07.660><c> have</c>

00:00:07.860 --> 00:00:07.870 align:start position:0%
chat i'm sasha schewe and today we have


" t)))
words))
  (describe "Finding approximate matches"
    (it "handles early oopses."
      (let ((result (subed-word-data-find-approximate-match
                     "This is a test"
                     (split-string
                      "This is oops This is a test."
                      " ")
                     "\\<oops\\>")))
        (expect (car result) :to-be-greater-than 0.01)
        (expect (string-join (cdr result) " ") :to-equal "This is oops")))
    (it "handles early oopses in a longer phrase."
      (let ((result (subed-word-data-find-approximate-match
                     "The quick brown fox jumps over the lazy dog"
                     (split-string
                      "The quick, oops, the quick brown fox jumps over the lazy dog and goes all sorts of places"
                      " ")
                     "\\<oops\\>")))
        (expect (car result) :to-be-greater-than 0.01)
        (expect (string-join (cdr result) " ") :to-equal "The quick, oops,")))
    (it "handles misrecognized words."
      (let ((result (subed-word-data-find-approximate-match
                     "Emacs is a text editor."
                     (split-string
                      "Emax is a text editor. More stuff goes here."
                      " "))))
        (expect (car result) :to-be-greater-than 0)
        (expect (string-join (cdr result) " ") :to-equal "Emax is a text editor.")))
    (it "handles split up words."
      (let ((result (subed-word-data-find-approximate-match
                     "Go in to the room."
                     (split-string
                      "Go into the room, more stuff goes here."
                      " "))))
        (expect (car result) :to-be-greater-than 0)
        (expect (string-join (cdr result) " ") :to-equal "Go into the room,")))
    (it "handles exact matches."
      (let ((result (subed-word-data-find-approximate-match
                     "We're lucky if we can get an exact match."
                     (split-string
                      "We're lucky if we can get an exact match. More stuff goes here."
                      " "))))
        (expect (car result) :to-be-less-than 0.001)
        (expect (string-join (cdr result) " ") :to-equal "We're lucky if we can get an exact match.")))

    )

  )
