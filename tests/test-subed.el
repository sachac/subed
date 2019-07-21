;; -*- eval: (buttercup-minor-mode) -*-
(add-to-list 'load-path "./subed")
(require 'subed)
(require 'subed-srt)

(describe "Iterating over subtitles"
  (it "without providing beginning and end."
    (with-temp-buffer
      (insert mock-srt-data)
      (subed-jump-to-subtitle-time-stop 1)
      (subed-for-each-subtitle nil nil nil
        (expect (looking-at "^[0-9]$") :to-be t)
        (forward-line 2)
        (kill-line)
        (insert "Hello."))
      (expect (subed-subtitle-text 1) :to-equal "Hello.")
      (expect (subed-subtitle-text 2) :to-equal "Bar.")
      (expect (subed-subtitle-text 3) :to-equal "Baz.")
      (expect (point) :to-equal 20)
      (subed-jump-to-subtitle-time-stop 2)
      (subed-for-each-subtitle nil nil nil
        (expect (looking-at "^[0-9]$") :to-be t)
        (forward-line 2)
        (kill-line)
        (insert "HEllo."))
      (expect (subed-subtitle-text 1) :to-equal "Hello.")
      (expect (subed-subtitle-text 2) :to-equal "HEllo.")
      (expect (subed-subtitle-text 3) :to-equal "Baz.")
      (expect (point) :to-equal 60)
      (subed-jump-to-subtitle-time-stop 3)
      (subed-for-each-subtitle nil nil nil
        (expect (looking-at "^[0-9]$") :to-be t)
        (forward-line 2)
        (kill-line)
        (insert "HELlo."))
      (expect (subed-subtitle-text 1) :to-equal "Hello.")
      (expect (subed-subtitle-text 2) :to-equal "HEllo.")
      (expect (subed-subtitle-text 3) :to-equal "HELlo.")
      (expect (point) :to-equal 99)))
  (describe "providing only the beginning"
    (it "forwards."
      (with-temp-buffer
        (insert mock-srt-data)
        (subed-jump-to-subtitle-time-start 1)
        (expect (point) :to-equal 3)
        (let ((new-texts (list "A" "B" "C")))
          (subed-for-each-subtitle 71 nil nil
            (expect (looking-at "^[0-9]$") :to-be t)
            (forward-line 2)
            (kill-line)
            (insert (pop new-texts))))
        (expect (subed-subtitle-text 1) :to-equal "Foo.")
        (expect (subed-subtitle-text 2) :to-equal "A")
        (expect (subed-subtitle-text 3) :to-equal "B")
        (expect (point) :to-equal 3)))
    (it "backwards."
      (with-temp-buffer
        (insert mock-srt-data)
        (subed-jump-to-subtitle-time-stop 3)
        (expect (point) :to-equal 95)
        (let ((new-texts (list "A" "B" "C")))
          (subed-for-each-subtitle 75 nil :reverse
            (expect (looking-at "^[0-9]$") :to-be t)
            (forward-line 2)
            (kill-line)
            (insert (pop new-texts))))
        (expect (subed-subtitle-text 1) :to-equal "Foo.")
        (expect (subed-subtitle-text 2) :to-equal "B")
        (expect (subed-subtitle-text 3) :to-equal "A")
        (expect (point) :to-equal 92)))
    )
  (describe "providing beginning and end,"
    (describe "excluding subtitles above"
      (it "forwards."
        (with-temp-buffer
          (insert mock-srt-data)
          (subed-jump-to-subtitle-time-stop 1)
          (expect (point) :to-equal 20)
          (let ((new-texts (list "A" "B" "C")))
            (subed-for-each-subtitle 71 79 nil
              (expect (looking-at "^[0-9]$") :to-be t)
              (forward-line 2)
              (kill-line)
              (insert (pop new-texts))))
          (expect (subed-subtitle-text 1) :to-equal "Foo.")
          (expect (subed-subtitle-text 2) :to-equal "A")
          (expect (subed-subtitle-text 3) :to-equal "B")
          (expect (point) :to-equal 20)))
      (it "backwards."
        (with-temp-buffer
          (insert mock-srt-data)
          (subed-jump-to-subtitle-time-start 3)
          (expect (point) :to-equal 79)
          (let ((new-texts (list "A" "B" "C")))
            (subed-for-each-subtitle 39 77 :reverse
              (expect (looking-at "^[0-9]$") :to-be t)
              (forward-line 2)
              (kill-line)
              (insert (pop new-texts))))
          (expect (subed-subtitle-text 1) :to-equal "Foo.")
          (expect (subed-subtitle-text 2) :to-equal "B")
          (expect (subed-subtitle-text 3) :to-equal "A")
          (expect (point) :to-equal 76)))
      )
    (describe "excluding subtitles below"
      (it "forwards."
        (with-temp-buffer
          (insert mock-srt-data)
          (subed-jump-to-subtitle-text 3)
          (expect (point) :to-equal 106)
          (let ((new-texts (list "A" "B" "C")))
            (subed-for-each-subtitle 5 76 nil
              (expect (looking-at "^[0-9]$") :to-be t)
              (forward-line 2)
              (kill-line)
              (insert (pop new-texts))))
          (expect (subed-subtitle-text 1) :to-equal "A")
          (expect (subed-subtitle-text 2) :to-equal "B")
          (expect (subed-subtitle-text 3) :to-equal "Baz.")
          (expect (point) :to-equal 100)))
      (it "backwards."
        (with-temp-buffer
          (insert mock-srt-data)
          (subed-jump-to-subtitle-time-stop 2)
          (expect (point) :to-equal 58)
          (let ((new-texts (list "A" "B" "C")))
            (subed-for-each-subtitle 20 76 :reverse
              (expect (looking-at "^[0-9]$") :to-be t)
              (forward-line 2)
              (kill-line)
              (insert (pop new-texts))))
          (expect (subed-subtitle-text 1) :to-equal "B")
          (expect (subed-subtitle-text 2) :to-equal "A")
          (expect (subed-subtitle-text 3) :to-equal "Baz.")
          (expect (point) :to-equal 55)))
      )
    )
  )

(describe "Adjusting subtitle start/stop time"
  :var (subed-subtitle-time-adjusted-hook)
  (it "runs the appropriate hook."
    (let ((foo (setf (symbol-function 'foo) (lambda (msecs) ()))))
      (spy-on 'foo)
      (add-hook 'subed-subtitle-time-adjusted-hook 'foo)
      (with-temp-buffer
        (insert mock-srt-data)
        (subed-jump-to-subtitle-id 1)
        (expect (subed-adjust-subtitle-time-start 100) :to-equal 100)
        (expect 'foo :to-have-been-called-with 61100)
        (expect 'foo :to-have-been-called-times 1)
        (expect (subed-adjust-subtitle-time-stop 123) :to-equal 123)
        (expect 'foo :to-have-been-called-with 61100)
        (expect 'foo :to-have-been-called-times 2)
        (subed-jump-to-subtitle-id 2)
        (expect (subed-adjust-subtitle-time-start 6) :to-equal 6)
        (expect 'foo :to-have-been-called-with 122240)
        (expect 'foo :to-have-been-called-times 3)
        (expect (subed-adjust-subtitle-time-stop 123) :to-equal 123)
        (expect 'foo :to-have-been-called-with 122240)
        (expect 'foo :to-have-been-called-times 4))
      (remove-hook 'subed-subtitle-time-adjusted-hook 'foo)))
  (it "adjusts the start/stop time."
    (with-temp-buffer
      (insert mock-srt-data)
      (subed-jump-to-subtitle-id 1)
      (expect (subed-adjust-subtitle-time-start 100) :to-equal 100)
      (expect (save-excursion (subed-jump-to-subtitle-time-start)
                              (thing-at-point 'line)) :to-equal "00:01:01,100 --> 00:01:05,123\n")
      (expect (subed-adjust-subtitle-time-start -200) :to-equal -200)
      (expect (save-excursion (subed-jump-to-subtitle-time-start)
                              (thing-at-point 'line)) :to-equal "00:01:00,900 --> 00:01:05,123\n")
      (expect (subed-adjust-subtitle-time-stop 200) :to-equal 200)
      (expect (save-excursion (subed-jump-to-subtitle-time-start)
                              (thing-at-point 'line)) :to-equal "00:01:00,900 --> 00:01:05,323\n")
      (expect (subed-adjust-subtitle-time-stop -100) :to-equal -100)
      (expect (save-excursion (subed-jump-to-subtitle-time-start)
                              (thing-at-point 'line)) :to-equal "00:01:00,900 --> 00:01:05,223\n")))
  (describe "enforces limits"
    (describe "when decreasing start time"
      (it "of the first subtitle."
        (with-temp-buffer
          (insert (concat "1\n"
                          "00:00:01,000 --> 00:00:02,000\n"
                          "Foo.\n"))
          (expect (subed-adjust-subtitle-time-start -999) :to-be -999)
          (expect (subed-subtitle-msecs-start) :to-be 1)
          (expect (subed-adjust-subtitle-time-start -1) :to-be -1)
          (expect (subed-subtitle-msecs-start) :to-be 0)
          (expect (subed-adjust-subtitle-time-start -1) :to-be nil)
          (expect (subed-subtitle-msecs-start) :to-be 0)))
      (it "of a non-first subtitle."
        (with-temp-buffer
          (insert (concat "1\n"
                          "00:00:01,000 --> 00:00:02,000\n"
                          "Foo.\n\n"
                          "2\n"
                          "00:00:03,000 --> 00:00:04,000\n"
                          "Bar.\n\n"))
          (subed-jump-to-subtitle-id 2)
          (expect (subed-adjust-subtitle-time-start -899) :to-be -899)
          (expect (subed-subtitle-msecs-start) :to-be 2101)
          (expect (subed-adjust-subtitle-time-start -1) :to-be -1)
          (expect (subed-subtitle-msecs-start) :to-be 2100)
          (expect (subed-adjust-subtitle-time-start -1) :to-be nil)
          (expect (subed-subtitle-msecs-start) :to-be 2100)))
      )
    (it "when increasing start time."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:03,000 --> 00:00:04,000\n"
                        "Bar.\n\n"))
        (insert mock-srt-data)
        (subed-jump-to-subtitle-id 2)
        (expect (subed-adjust-subtitle-time-start 999) :to-be 999)
        (expect (subed-subtitle-msecs-start) :to-be 3999)
        (expect (subed-adjust-subtitle-time-start 1) :to-be 1)
        (expect (subed-subtitle-msecs-start) :to-be 4000)
        (expect (subed-adjust-subtitle-time-start 1) :to-be nil)
        (expect (subed-subtitle-msecs-start) :to-be 4000)))
    (it "when decreasing stop time."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:03,000 --> 00:00:04,000\n"
                        "Bar.\n\n"))
        (subed-jump-to-subtitle-id 2)
        (expect (subed-adjust-subtitle-time-stop -999) :to-be -999)
        (expect (subed-subtitle-msecs-stop) :to-be 3001)
        (expect (subed-adjust-subtitle-time-stop -1) :to-be -1)
        (expect (subed-subtitle-msecs-stop) :to-be 3000)
        (expect (subed-adjust-subtitle-time-stop -1) :to-be nil)
        (expect (subed-subtitle-msecs-stop) :to-be 3000)))
    (describe "when increasing stop time"
      (it "of the last subtitle."
        (with-temp-buffer
          (insert (concat "1\n"
                          "00:00:01,000 --> 00:00:02,000\n"
                          "Foo.\n\n"
                          "2\n"
                          "00:00:03,000 --> 00:00:04,000\n"
                          "Bar.\n\n"))
          (subed-jump-to-subtitle-id 3)
          (expect (subed-adjust-subtitle-time-stop 1000000):to-be 1000000)
          (expect (subed-subtitle-msecs-stop) :to-be 1004000)))
      (it "of a non-last subtitle."
        (with-temp-buffer
          (insert (concat "1\n"
                          "00:00:01,000 --> 00:00:02,000\n"
                          "Foo.\n\n"
                          "2\n"
                          "00:00:03,000 --> 00:00:04,000\n"
                          "Bar.\n\n"))
          (subed-jump-to-subtitle-id 1)
          (expect (subed-adjust-subtitle-time-stop 899) :to-be 899)
          (expect (subed-subtitle-msecs-stop) :to-be 2899)
          (expect (subed-adjust-subtitle-time-stop 1) :to-be 1)
          (expect (subed-subtitle-msecs-stop) :to-be 2900)
          (expect (subed-adjust-subtitle-time-stop 1) :to-be nil)
          (expect (subed-subtitle-msecs-stop) :to-be 2900)))
      )
    (it "without undershooting the target time."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:02,000 --> 00:00:03,000\n"
                        "Bar.\n"))
        (subed-jump-to-subtitle-id 1)
        (expect (subed-adjust-subtitle-time-stop 1) :to-be nil)
        (expect (subed-subtitle-msecs-stop) :to-equal 2000)))
    (it "without overshooting the target time."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:02,000 --> 00:00:03,000\n"
                        "Bar.\n"))
        (subed-jump-to-subtitle-id 2)
        (expect (subed-adjust-subtitle-time-start -1) :to-be nil)
        (expect (subed-subtitle-msecs-start) :to-equal 2000)))
    )
  (describe "ignores negative duration if the first argument is truthy"
    (it "when adjusting start time."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"))
        (expect (subed-adjust-subtitle-time-start 2000 t) :to-be 2000)
        (expect (subed-subtitle-msecs-start) :to-be 3000)
        (expect (subed-subtitle-msecs-stop) :to-be 2000)
        (expect (subed-adjust-subtitle-time-start -500 t) :to-be -500)
        (expect (subed-subtitle-msecs-start) :to-be 2500)
        (expect (subed-subtitle-msecs-stop) :to-be 2000)))
    (it "when adjusting stop time."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"))
        (expect (subed-adjust-subtitle-time-stop -1500 t) :to-be -1500)
        (expect (subed-subtitle-msecs-stop) :to-be 500)
        (expect (subed-subtitle-msecs-start) :to-be 1000)
        (expect (subed-adjust-subtitle-time-stop 200 t) :to-be 200)
        (expect (subed-subtitle-msecs-stop) :to-be 700)
        (expect (subed-subtitle-msecs-start) :to-be 1000)))
    )
  (describe "ignores subtitle spacing if the second argument is truthy"
    (it "when adjusting start time."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:02,200 --> 00:00:03,000\n"
                        "Bar.\n"))
        (subed-jump-to-subtitle-id 2)
        (expect (subed-adjust-subtitle-time-start -150 nil t) :to-be -150)
        (expect (subed-subtitle-msecs-start 2) :to-be 2050)
        (expect (subed-subtitle-msecs-stop 1) :to-be 2000)
        (expect (subed-adjust-subtitle-time-start -51 nil t) :to-be -51)
        (expect (subed-subtitle-msecs-start 2) :to-be 1999)
        (expect (subed-subtitle-msecs-stop 1) :to-be 2000)))
    (it "when adjusting stop time."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:02,200 --> 00:00:03,000\n"
                        "Bar.\n"))
        (subed-jump-to-subtitle-id 1)
        (expect (subed-adjust-subtitle-time-stop 150 nil t) :to-be 150)
        (expect (subed-subtitle-msecs-stop 1) :to-be 2150)
        (expect (subed-subtitle-msecs-start 2) :to-be 2200)
        (expect (subed-adjust-subtitle-time-stop 51 nil t) :to-be 51)
        (expect (subed-subtitle-msecs-stop 1) :to-be 2201)
        (expect (subed-subtitle-msecs-start 2) :to-be 2200)))
    )
  (it "does nothing if no timestamp can be found."
    (with-temp-buffer
      (insert "foo")
      (goto-char (point-min))
      (expect (subed-adjust-subtitle-time-start 123) :to-be nil)
      (expect (buffer-string) :to-equal "foo")
      (expect (subed-adjust-subtitle-time-start -123) :to-be nil)
      (expect (buffer-string) :to-equal "foo")))
  )

(describe "Moving"
  (it "adjusts start and stop time by the same amount."
    (with-temp-buffer
      (insert (concat "1\n"
                      "00:00:01,000 --> 00:00:02,000\n"
                      "Foo.\n"))
      (let ((orig-point (subed-jump-to-subtitle-text 1)))
        (subed-move-subtitle-forward 100)
        (expect (subed-subtitle-msecs-start) :to-equal 1100)
        (expect (subed-subtitle-msecs-stop) :to-equal 2100)
        (subed-move-subtitle-backward 200)
        (expect (subed-subtitle-msecs-start) :to-equal 900)
        (expect (subed-subtitle-msecs-stop) :to-equal 1900)
        (expect (point) :to-equal orig-point))))
  (it "adjusts start and stop time by the same amount when bumping into next subtitle."
    (with-temp-buffer
      (insert (concat "1\n"
                      "00:00:01,000 --> 00:00:01,600\n"
                      "Foo.\n\n"
                      "2\n"
                      "00:00:02,000 --> 00:00:03,000\n"
                      "Bar.\n"))
      (let ((orig-point (subed-jump-to-subtitle-id 1)))
        (subed-move-subtitle-forward 1000)
        (expect (subed-subtitle-msecs-start) :to-equal 1300)
        (expect (subed-subtitle-msecs-stop) :to-equal 1900)
        (expect (point) :to-equal orig-point))))
  (it "adjusts start and stop time by the same amount when bumping into previous subtitle."
    (with-temp-buffer
      (insert (concat "1\n"
                      "00:00:01,000 --> 00:00:01,600\n"
                      "Foo.\n\n"
                      "2\n"
                      "00:00:02,000 --> 00:00:03,000\n"
                      "Bar.\n"))
      (let ((orig-point (subed-jump-to-subtitle-id 2)))
        (subed-move-subtitle-backward 1000)
        (expect (subed-subtitle-msecs-start) :to-equal 1700)
        (expect (subed-subtitle-msecs-stop) :to-equal 2700)
        (expect (point) :to-equal orig-point))))
  (it "does not adjust anything if subtitle cannot be moved forward at all."
    (with-temp-buffer
      (insert (concat "1\n"
                      "00:00:01,000 --> 00:00:02,000\n"
                      "Foo.\n\n"
                      "2\n"
                      "00:00:02,000 --> 00:00:03,000\n"
                      "Bar.\n"))
      (let ((orig-point (subed-jump-to-subtitle-id 1)))
        (subed-move-subtitle-forward 1)
        (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
        (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
        (expect (subed-subtitle-msecs-start 2) :to-equal 2000)
        (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
        (expect (point) :to-equal orig-point))))
  (it "does not adjust anything if subtitle cannot be moved backward at all."
    (with-temp-buffer
      (insert (concat "1\n"
                      "00:00:01,000 --> 00:00:02,000\n"
                      "Foo.\n\n"
                      "2\n"
                      "00:00:02,000 --> 00:00:03,000\n"
                      "Bar.\n"))
      (let ((orig-point (subed-jump-to-subtitle-id 2)))
        (subed-move-subtitle-backward 1)
        (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
        (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
        (expect (subed-subtitle-msecs-start 2) :to-equal 2000)
        (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
        (expect (point) :to-equal orig-point))))
  (describe "adjusts subtitles in the active region,"
    (it "excluding the first subtitle."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:03,000 --> 00:00:04,000\n"
                        "Bar.\n\n"
                        "3\n"
                        "00:00:05,000 --> 00:00:06,000\n"
                        "Baz.\n"))
        (spy-on 'use-region-p :and-return-value t)
        (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-text 2))
        (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-time-start 3))
        (let ((orig-point (subed-jump-to-subtitle-text 2)))
          (subed-move-subtitle-forward 100)
          (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
          (expect (subed-subtitle-msecs-start 2) :to-equal 3100)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 4100)
          (expect (subed-subtitle-msecs-start 3) :to-equal 5100)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 6100)
          (expect (point) :to-equal orig-point)
          (subed-move-subtitle-backward 200)
          (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
          (expect (subed-subtitle-msecs-start 2) :to-equal 2900)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 3900)
          (expect (subed-subtitle-msecs-start 3) :to-equal 4900)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 5900)
          (expect (point) :to-equal orig-point))))
    (it "excluding the last subtitle."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:03,000 --> 00:00:04,000\n"
                        "Bar.\n\n"
                        "3\n"
                        "00:00:05,000 --> 00:00:06,000\n"
                        "Baz.\n"))
        (spy-on 'use-region-p :and-return-value t)
        (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-text 1))
        (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-time-stop 2))
        (let ((orig-point (subed-jump-to-subtitle-time-stop 3)))
          (subed-move-subtitle-forward 500)
          (expect (subed-subtitle-msecs-start 1) :to-equal 1500)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2500)
          (expect (subed-subtitle-msecs-start 2) :to-equal 3500)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 4500)
          (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
          (expect (point) :to-equal orig-point)
          (subed-move-subtitle-backward 300)
          (expect (subed-subtitle-msecs-start 1) :to-equal 1200)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2200)
          (expect (subed-subtitle-msecs-start 2) :to-equal 3200)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 4200)
          (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
          (expect (point) :to-equal orig-point))))
    (describe "not changing spacing between subtitles"
      (it "when moving forward."
        (with-temp-buffer
          (insert (concat "1\n"
                          "00:00:01,000 --> 00:00:02,000\n"
                          "Foo.\n\n"
                          "2\n"
                          "00:00:10,000 --> 00:00:11,000\n"
                          "Bar.\n\n"
                          "3\n"
                          "00:00:12,000 --> 00:00:13,000\n"
                          "Baz.\n"))
          (spy-on 'use-region-p :and-return-value t)
          (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 1))
          (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 2))
          (let ((orig-point (subed-jump-to-subtitle-time-start 1)))
            (subed-move-subtitle-forward 2000)
            (expect (subed-subtitle-msecs-start 1) :to-equal 1900)
            (expect (subed-subtitle-msecs-stop 1) :to-equal 2900)
            (expect (subed-subtitle-msecs-start 2) :to-equal 10900)
            (expect (subed-subtitle-msecs-stop 2) :to-equal 11900)
            (expect (subed-subtitle-msecs-start 3) :to-equal 12000)
            (expect (subed-subtitle-msecs-stop 3) :to-equal 13000)
            (expect (point) :to-equal orig-point))))
      (it "when moving backward."
        (with-temp-buffer
          (insert (concat "1\n"
                          "00:00:01,000 --> 00:00:02,000\n"
                          "Foo.\n\n"
                          "2\n"
                          "00:00:3,000 --> 00:00:4,000\n"
                          "Bar.\n\n"
                          "3\n"
                          "00:00:10,000 --> 00:00:11,000\n"
                          "Baz.\n"))
          (spy-on 'use-region-p :and-return-value t)
          (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 2))
          (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 3))
          (let ((orig-point (subed-jump-to-subtitle-time-start 2)))
            (subed-move-subtitle-backward 10000)
            (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
            (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
            (expect (subed-subtitle-msecs-start 2) :to-equal 2100)
            (expect (subed-subtitle-msecs-stop 2) :to-equal 3100)
            (expect (subed-subtitle-msecs-start 3) :to-equal 9100)
            (expect (subed-subtitle-msecs-stop 3) :to-equal 10100)
            (expect (point) :to-equal orig-point))))
      )
    )
  (describe "unless there is no space left"
    (it "when moving forward."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:10,000 --> 00:00:11,000\n"
                        "Bar.\n\n"
                        "3\n"
                        "00:00:11,000 --> 00:00:12,000\n"
                        "Baz.\n"))
        (spy-on 'use-region-p :and-return-value t)
        (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 1))
        (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 2))
        (let ((orig-point (subed-jump-to-subtitle-text 1)))
          (subed-move-subtitle-forward 1)
          (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
          (expect (subed-subtitle-msecs-start 2) :to-equal 10000)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 11000)
          (expect (subed-subtitle-msecs-start 3) :to-equal 11000)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 12000)
          (expect (point) :to-equal orig-point))))
    (it "when moving backward."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:02,000 --> 00:00:03,000\n"
                        "Bar.\n\n"
                        "3\n"
                        "00:00:11,000 --> 00:00:12,000\n"
                        "Baz.\n"))
        (spy-on 'use-region-p :and-return-value t)
        (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 2))
        (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 3))
        (let ((orig-point (subed-jump-to-subtitle-id 3)))
          (subed-move-subtitle-backward 1)
          (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
          (expect (subed-subtitle-msecs-start 2) :to-equal 2000)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
          (expect (subed-subtitle-msecs-start 3) :to-equal 11000)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 12000)
          (expect (point) :to-equal orig-point))))
    )
  (describe "ignoring spacing for non-leading subtitles"
    (it "when moving forward."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:00,000 --> 00:00:01,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:01,050 --> 00:00:02,000\n"
                        "Bar.\n\n"
                        "3\n"
                        "00:00:05,000 --> 00:00:6,000\n"
                        "Baz.\n"))
        (spy-on 'use-region-p :and-return-value t)
        (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 1))
        (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 2))
        (let ((orig-point (subed-jump-to-subtitle-time-start 3)))
          (subed-move-subtitle-forward 1000)
          (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
          (expect (subed-subtitle-msecs-start 2) :to-equal 2050)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
          (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
          (expect (point) :to-equal orig-point))))
    (it "when moving backward."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:04,000 --> 00:00:05,000\n"
                        "Bar.\n\n"
                        "3\n"
                        "00:00:05,000 --> 00:00:05,000\n"
                        "Baz.\n"))
        (spy-on 'use-region-p :and-return-value t)
        (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 2))
        (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 3))
        (let ((orig-point (subed-jump-to-subtitle-time-stop 1)))
          (subed-move-subtitle-backward 1000)
          (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
          (expect (subed-subtitle-msecs-start 2) :to-equal 3000)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 4000)
          (expect (subed-subtitle-msecs-start 3) :to-equal 4000)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 4000)
          (expect (point) :to-equal orig-point))))
    )
  (describe "ignoring overlapping subtitles"
    (it "when moving forward."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:01,500\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:01,300 --> 00:00:02,000\n"
                        "Bar.\n\n"
                        "3\n"
                        "00:00:05,000 --> 00:00:6,000\n"
                        "Baz.\n"))
        (spy-on 'use-region-p :and-return-value t)
        (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 1))
        (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 2))
        (let ((orig-point (subed-jump-to-subtitle-text 2)))
          (subed-move-subtitle-forward 1000)
          (expect (subed-subtitle-msecs-start 1) :to-equal 2000)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2500)
          (expect (subed-subtitle-msecs-start 2) :to-equal 2300)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
          (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
          (expect (point) :to-equal orig-point))))
    (it "when moving backward."
      (with-temp-buffer
        (insert (concat "1\n"
                        "00:00:01,000 --> 00:00:02,000\n"
                        "Foo.\n\n"
                        "2\n"
                        "00:00:04,500 --> 00:00:04,000\n"
                        "Bar.\n\n"
                        "3\n"
                        "00:00:04,500 --> 00:00:04,490\n"
                        "Baz.\n"))
        (spy-on 'use-region-p :and-return-value t)
        (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-id 2))
        (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-text 3))
        (let ((orig-point (subed-jump-to-subtitle-text 1)))
          (subed-move-subtitle-backward 1000)
          (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
          (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
          (expect (subed-subtitle-msecs-start 2) :to-equal 3500)
          (expect (subed-subtitle-msecs-stop 2) :to-equal 3000)
          (expect (subed-subtitle-msecs-start 3) :to-equal 3500)
          (expect (subed-subtitle-msecs-stop 3) :to-equal 3490)
          (expect (point) :to-equal orig-point))))
    )
  (it "ignoring start time being larger than stop time."
    (with-temp-buffer
      (insert (concat "1\n"
                      "00:00:01,500 --> 00:00:01,400\n"
                      "Foo.\n\n"
                      "2\n"
                      "00:00:02,500 --> 00:00:02,499\n"
                      "Bar.\n\n"
                      "3\n"
                      "00:00:05,000 --> 00:00:06,000\n"
                      "Bar.\n"))
      (spy-on 'use-region-p :and-return-value t)
      (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-text 1))
      (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-time-start 2))
      (let ((orig-point (subed-jump-to-subtitle-time-stop 1)))
        (subed-move-subtitle-forward 1000)
        (expect (subed-subtitle-msecs-start 1) :to-equal 2500)
        (expect (subed-subtitle-msecs-stop 1) :to-equal 2400)
        (expect (subed-subtitle-msecs-start 2) :to-equal 3500)
        (expect (subed-subtitle-msecs-stop 2) :to-equal 3499)
        (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
        (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
        (expect (point) :to-equal orig-point)
        (subed-move-subtitle-backward 500)
        (expect (subed-subtitle-msecs-start 1) :to-equal 2000)
        (expect (subed-subtitle-msecs-stop 1) :to-equal 1900)
        (expect (subed-subtitle-msecs-start 2) :to-equal 3000)
        (expect (subed-subtitle-msecs-stop 2) :to-equal 2999)
        (expect (subed-subtitle-msecs-start 3) :to-equal 5000)
        (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
        (expect (point) :to-equal orig-point))))
  (it "ignoring stop time being smaller than start time."
    (with-temp-buffer
      (insert (concat "1\n"
                      "00:00:01,000 --> 00:00:02,000\n"
                      "Foo.\n\n"
                      "2\n"
                      "00:00:04,100 --> 00:00:04,099\n"
                      "Bar.\n\n"
                      "3\n"
                      "00:00:05,500 --> 00:00:05,000\n"
                      "Bar.\n"))
      (spy-on 'use-region-p :and-return-value t)
      (spy-on 'region-beginning :and-return-value (subed-jump-to-subtitle-text 2))
      (spy-on 'region-end :and-return-value (subed-jump-to-subtitle-time-start 3))
      (let ((orig-point (subed-jump-to-subtitle-text 1)))
        (subed-move-subtitle-forward 1000)
        (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
        (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
        (expect (subed-subtitle-msecs-start 2) :to-equal 5100)
        (expect (subed-subtitle-msecs-stop 2) :to-equal 5099)
        (expect (subed-subtitle-msecs-start 3) :to-equal 6500)
        (expect (subed-subtitle-msecs-stop 3) :to-equal 6000)
        (expect (point) :to-equal orig-point)
        (subed-move-subtitle-backward 500)
        (expect (subed-subtitle-msecs-start 1) :to-equal 1000)
        (expect (subed-subtitle-msecs-stop 1) :to-equal 2000)
        (expect (subed-subtitle-msecs-start 2) :to-equal 4600)
        (expect (subed-subtitle-msecs-stop 2) :to-equal 4599)
        (expect (subed-subtitle-msecs-start 3) :to-equal 6000)
        (expect (subed-subtitle-msecs-stop 3) :to-equal 5500)
        (expect (point) :to-equal orig-point))))
  (it "disables subtitle replay while moving subtitles."
    (with-temp-buffer
      (insert mock-srt-data)
      (subed-enable-replay-adjusted-subtitle :quiet)
      (spy-on 'subed-enable-replay-adjusted-subtitle :and-call-through)
      (spy-on 'subed-disable-replay-adjusted-subtitle :and-call-through)
      (spy-on 'subed-adjust-subtitle-time-start :and-call-fake
              (lambda (msecs &optional a b) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
      (spy-on 'subed-adjust-subtitle-stop :and-call-fake
              (lambda (msecs &optional a b) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
      (subed-move-subtitle-forward 100)
      (expect 'subed-disable-replay-adjusted-subtitle :to-have-been-called-times 1)
      (expect 'subed-enable-replay-adjusted-subtitle :to-have-been-called-times 1)
      (subed-move-subtitle-backward 100)
      (expect 'subed-disable-replay-adjusted-subtitle :to-have-been-called-times 2)
      (expect 'subed-enable-replay-adjusted-subtitle :to-have-been-called-times 2)))
  (it "does not enable subtitle replay afterwards if it is disabled."
    (with-temp-buffer
      (insert mock-srt-data)
      (subed-disable-replay-adjusted-subtitle :quiet)
      (spy-on 'subed-enable-replay-adjusted-subtitle :and-call-through)
      (spy-on 'subed-disable-replay-adjusted-subtitle :and-call-through)
      (spy-on 'subed-adjust-subtitle-time-start :and-call-fake
              (lambda (msecs &optional a b) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
      (spy-on 'subed-adjust-subtitle-stop :and-call-fake
              (lambda (msecs &optional a b) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
      (subed-move-subtitle-forward 100)
      (expect 'subed-disable-replay-adjusted-subtitle :to-have-been-called-times 1)
      (expect 'subed-enable-replay-adjusted-subtitle :to-have-been-called-times 0)
      (subed-move-subtitle-backward 100)
      (expect 'subed-disable-replay-adjusted-subtitle :to-have-been-called-times 2)
      (expect 'subed-enable-replay-adjusted-subtitle :to-have-been-called-times 0)))
  (it "seeks player to current subtitle if region is not active."
    (with-temp-buffer
      (insert mock-srt-data)
      (spy-on 'subed-replay-adjusted-subtitle-p :and-return-value t)
      (spy-on 'subed-mpv-jump)
      (subed-move-subtitle-forward 100)
      (expect 'subed-mpv-jump :to-have-been-called-times 1)
      (expect 'subed-mpv-jump :to-have-been-called-with 183550)
      (subed-move-subtitle-backward 200)
      (expect 'subed-mpv-jump :to-have-been-called-times 2)
      (expect 'subed-mpv-jump :to-have-been-called-with 183350)))
  (it "seeks player to first subtitle in active region."
    (with-temp-buffer
      (insert mock-srt-data)
      (let ((beg 15)
            (end (point-max)))
        (spy-on 'use-region-p :and-return-value t)
        (spy-on 'region-beginning :and-return-value beg)
        (spy-on 'region-end :and-return-value end)
        (spy-on 'subed-replay-adjusted-subtitle-p :and-return-value t)
        (spy-on 'subed-mpv-jump)
        (subed-move-subtitle-forward 100)
        (expect 'subed-mpv-jump :to-have-been-called-times 1)
        (expect 'subed-mpv-jump :to-have-been-called-with '61100)
        (subed-move-subtitle-backward 300)
        (expect 'subed-mpv-jump :to-have-been-called-times 2)
        (expect 'subed-mpv-jump :to-have-been-called-with '60800))))
  )

(describe "Inserting evenly spaced"
  (before-each
    (spy-on 'subed-regenerate-ids-soon))
  (describe "in an empty buffer,"
    (describe "appending"
      (it "a single subtile."
        (cl-loop for arg in (list nil 1) do
                 (with-temp-buffer
                   (expect (subed-insert-subtitle arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:00,000 --> 00:00:01,000\n"
                                                             "\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtiles."
        (cl-loop for arg in (list 2) do
                 (with-temp-buffer
                   (expect (subed-insert-subtitle arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:00,000 --> 00:00:01,000\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:00:01,100 --> 00:00:02,100\n"
                                                             "\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    (describe "prepending"
      (it "a single subtile."
        (cl-loop for arg in (list '- -1 (list 4)) do
                 (with-temp-buffer
                   (expect (subed-insert-subtitle arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:00,000 --> 00:00:01,000\n"
                                                             "\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtiles."
        (cl-loop for arg in (list -2 (list -16)) do
                 (with-temp-buffer
                   (expect (subed-insert-subtitle arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:00,000 --> 00:00:01,000\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:00:01,100 --> 00:00:02,100\n"
                                                             "\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    )
  (describe "in a non-empty buffer"
    (describe "prepending between subtitles"
      (it "a single subtitle."
        (cl-loop for arg in (list '- -1 (list 4)) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n\n"
                                   "2\n"
                                   "00:02:00,000 --> 00:02:01,000\n"
                                   "Bar.\n"))
                   (subed-jump-to-subtitle-text 2)
                   (expect (subed-insert-subtitle arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:30,000 --> 00:01:31,000\n"
                                                             "\n\n"
                                                             "2\n"
                                                             "00:02:00,000 --> 00:02:01,000\n"
                                                             "Bar.\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtitles."
        (cl-loop for arg in (list -2 (list 16)) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n\n"
                                   "2\n"
                                   "00:02:00,000 --> 00:02:01,000\n"
                                   "Bar.\n"))
                   (subed-jump-to-subtitle-text 2)
                   (expect (subed-insert-subtitle arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:20,000 --> 00:01:21,000\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:01:40,000 --> 00:01:41,000\n"
                                                             "\n\n"
                                                             "2\n"
                                                             "00:02:00,000 --> 00:02:01,000\n"
                                                             "Bar.\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    (describe "appending between subtitles"
      (it "a single subtitle."
        (cl-loop for arg in (list nil 1) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n\n"
                                   "2\n"
                                   "00:02:00,000 --> 00:02:01,000\n"
                                   "Bar.\n"))
                   (subed-jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:30,000 --> 00:01:31,000\n"
                                                             "\n\n"
                                                             "2\n"
                                                             "00:02:00,000 --> 00:02:01,000\n"
                                                             "Bar.\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtitles."
        (cl-loop for arg in (list 2) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n\n"
                                   "2\n"
                                   "00:02:00,000 --> 00:02:01,000\n"
                                   "Bar.\n"))
                   (subed-jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:20,000 --> 00:01:21,000\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:01:40,000 --> 00:01:41,000\n"
                                                             "\n\n"
                                                             "2\n"
                                                             "00:02:00,000 --> 00:02:01,000\n"
                                                             "Bar.\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    (describe "prepending to the first subtitle"
      (it "a single subtitle."
        (cl-loop for arg in (list '- -1 (list 4)) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:01:00,000 --> 00:01:01,000\n"
                                   "Foo.\n"))
                   (subed-srt--jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:30,000 --> 00:00:31,000\n"
                                                             "\n\n"
                                                             "1\n"
                                                             "00:01:00,000 --> 00:01:01,000\n"
                                                             "Foo.\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtitles."
        (cl-loop for arg in (list -2 (list 16)) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:01:00,000 --> 00:01:01,000\n"
                                   "Foo.\n"))
                   (subed-srt--jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:20,000 --> 00:00:21,000\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:00:40,000 --> 00:00:41,000\n"
                                                             "\n\n"
                                                             "1\n"
                                                             "00:01:00,000 --> 00:01:01,000\n"
                                                             "Foo.\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    (describe "appending to the last subtitle"
      (it "a single subtitle."
        (cl-loop for arg in (list nil 1) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n"))
                   (subed-srt--jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:00,100 --> 00:01:01,100\n"
                                                             "\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtitles."
        (cl-loop for arg in (list 2) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n"))
                   (subed-srt--jump-to-subtitle-text 3)
                   (expect (subed-insert-subtitle arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:00,100 --> 00:01:01,100\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:01:01,200 --> 00:01:02,200\n"
                                                             "\n"))
                   (expect (point) :to-equal 71))))
      )
    (describe "when there is not enough time for the subtitles"
      (describe "to append"
        (it "a single subtitle."
          (cl-loop for arg in (list nil 1) do
                   (with-temp-buffer
                     (insert (concat "1\n"
                                     "00:00:59,000 --> 00:01:00,000\n"
                                     "Foo.\n\n"
                                     "2\n"
                                     "00:01:00,600 --> 00:01:02,000\n"
                                     "Foo.\n"))
                     (subed-srt--jump-to-subtitle-text 1)
                     (expect (subed-insert-subtitle arg) :to-equal 71)
                     (expect (buffer-string) :to-equal (concat "1\n"
                                                               "00:00:59,000 --> 00:01:00,000\n"
                                                               "Foo.\n\n"
                                                               "0\n"
                                                               "00:01:00,100 --> 00:01:00,500\n"
                                                               "\n\n"
                                                               "2\n"
                                                               "00:01:00,600 --> 00:01:02,000\n"
                                                               "Foo.\n"))
                     (expect (point) :to-equal 71)
                     (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                     (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list 2) do
                   (with-temp-buffer
                     (insert (concat "1\n"
                                     "00:00:59,000 --> 00:01:00,000\n"
                                     "Foo.\n\n"
                                     "2\n"
                                     "00:01:00,600 --> 00:01:02,000\n"
                                     "Foo.\n"))
                     (subed-srt--jump-to-subtitle-text 1)
                     (expect (subed-insert-subtitle arg) :to-equal 71)
                     (expect (buffer-string) :to-equal (concat "1\n"
                                                               "00:00:59,000 --> 00:01:00,000\n"
                                                               "Foo.\n\n"
                                                               "0\n"
                                                               "00:01:00,100 --> 00:01:00,250\n"
                                                               "\n\n"
                                                               "0\n"
                                                               "00:01:00,350 --> 00:01:00,500\n"
                                                               "\n\n"
                                                               "2\n"
                                                               "00:01:00,600 --> 00:01:02,000\n"
                                                               "Foo.\n"))
                     (expect (point) :to-equal 71)
                     (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                     (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      (describe "to prepend"
        (describe "between subtitles"
          (it "a single subtitle."
            (cl-loop for arg in (list '- -1 (list 4)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:57,000 --> 00:00:59,100\n"
                                       "Foo.\n\n"
                                       "2\n"
                                       "00:01:00,000 --> 00:01:02,000\n"
                                       "Foo.\n"))
                       (subed-srt--jump-to-subtitle-text 2)
                       (expect (subed-insert-subtitle arg) :to-equal 71)
                       (expect (buffer-string) :to-equal (concat "1\n"
                                                                 "00:00:57,000 --> 00:00:59,100\n"
                                                                 "Foo.\n\n"
                                                                 "0\n"
                                                                 "00:00:59,200 --> 00:00:59,900\n"
                                                                 "\n\n"
                                                                 "2\n"
                                                                 "00:01:00,000 --> 00:01:02,000\n"
                                                                 "Foo.\n"))
                       (expect (point) :to-equal 71)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list -2 (list 16)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:57,000 --> 00:00:59,100\n"
                                       "Foo.\n\n"
                                       "2\n"
                                       "00:01:00,000 --> 00:01:02,000\n"
                                       "Foo.\n"))
                       (subed-srt--jump-to-subtitle-text 2)
                       (expect (subed-insert-subtitle arg) :to-equal 71)
                       (expect (buffer-string) :to-equal (concat "1\n"
                                                                 "00:00:57,000 --> 00:00:59,100\n"
                                                                 "Foo.\n\n"
                                                                 "0\n"
                                                                 "00:00:59,200 --> 00:00:59,500\n"
                                                                 "\n\n"
                                                                 "0\n"
                                                                 "00:00:59,600 --> 00:00:59,900\n"
                                                                 "\n\n"
                                                                 "2\n"
                                                                 "00:01:00,000 --> 00:01:02,000\n"
                                                                 "Foo.\n"))
                       (expect (point) :to-equal 71)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        (describe "before the first subtitle"
          (it "a single subtitle."
            (cl-loop for arg in (list '- -1 (list 4)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:00,500 --> 00:00:02,000\n"
                                       "Foo.\n"))
                       (subed-srt--jump-to-subtitle-text 1)
                       (expect (subed-insert-subtitle arg) :to-equal 33)
                       (expect (buffer-string) :to-equal (concat "0\n"
                                                                 "00:00:00,100 --> 00:00:00,400\n"
                                                                 "\n\n"
                                                                 "1\n"
                                                                 "00:00:00,500 --> 00:00:02,000\n"
                                                                 "Foo.\n"))
                       (expect (point) :to-equal 33)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list -2 (list 16)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:00,600 --> 00:00:01,500\n"
                                       "Foo.\n"))
                       (subed-srt--jump-to-subtitle-text 1)
                       (expect (subed-insert-subtitle arg) :to-equal 33)
                       (expect (buffer-string) :to-equal (concat "0\n"
                                                                 "00:00:00,100 --> 00:00:00,250\n"
                                                                 "\n\n"
                                                                 "0\n"
                                                                 "00:00:00,350 --> 00:00:00,500\n"
                                                                 "\n\n"
                                                                 "1\n"
                                                                 "00:00:00,600 --> 00:00:01,500\n"
                                                                 "Foo.\n"))
                       (expect (point) :to-equal 33)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        )
      )
    (describe "when there is not enough time for spacing"
      (describe "between subtitles"
        (describe "when prepending"
          (it "a single subtitle."
            (cl-loop for arg in (list '- -1 (list 4)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:55,000 --> 00:00:59,950\n"
                                       "Foo.\n\n"
                                       "2\n"
                                       "00:01:00,000 --> 00:01:02,000\n"
                                       "Bar.\n"))
                       (subed-srt--jump-to-subtitle-text 2)
                       (expect (subed-insert-subtitle arg) :to-equal 71)
                       (expect (buffer-string) :to-equal (concat "1\n"
                                                                 "00:00:55,000 --> 00:00:59,950\n"
                                                                 "Foo.\n\n"
                                                                 "0\n"
                                                                 "00:00:59,950 --> 00:00:59,950\n"
                                                                 "\n\n"
                                                                 "2\n"
                                                                 "00:01:00,000 --> 00:01:02,000\n"
                                                                 "Bar.\n"))
                       (expect (point) :to-equal 71)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list -2 (list 16)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:57,000 --> 00:00:59,999\n"
                                       "Foo.\n\n"
                                       "2\n"
                                       "00:01:00,000 --> 00:01:02,000\n"
                                       "Bar.\n"))
                       (subed-srt--jump-to-subtitle-text 2)
                       (expect (subed-insert-subtitle arg) :to-equal 71)
                       (expect (buffer-string) :to-equal (concat "1\n"
                                                                 "00:00:57,000 --> 00:00:59,999\n"
                                                                 "Foo.\n\n"
                                                                 "0\n"
                                                                 "00:00:59,999 --> 00:00:59,999\n"
                                                                 "\n\n"
                                                                 "0\n"
                                                                 "00:00:59,999 --> 00:00:59,999\n"
                                                                 "\n\n"
                                                                 "2\n"
                                                                 "00:01:00,000 --> 00:01:02,000\n"
                                                                 "Bar.\n"))
                       (expect (point) :to-equal 71)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        (describe "when appending"
          (it "a single subtitle."
            (cl-loop for arg in (list nil 1) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:59,000 --> 00:01:00,000\n"
                                       "Foo.\n\n"
                                       "2\n"
                                       "00:01:00,010 --> 00:01:02,000\n"
                                       "Bar.\n"))
                       (subed-srt--jump-to-subtitle-text 1)
                       (expect (subed-insert-subtitle arg) :to-equal 71)
                       (expect (buffer-string) :to-equal (concat "1\n"
                                                                 "00:00:59,000 --> 00:01:00,000\n"
                                                                 "Foo.\n\n"
                                                                 "0\n"
                                                                 "00:01:00,000 --> 00:01:00,000\n"
                                                                 "\n\n"
                                                                 "2\n"
                                                                 "00:01:00,010 --> 00:01:02,000\n"
                                                                 "Bar.\n"))
                       (expect (point) :to-equal 71)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list 2) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:59,000 --> 00:01:00,000\n"
                                       "Foo.\n\n"
                                       "2\n"
                                       "00:01:00,100 --> 00:01:02,000\n"
                                       "Bar.\n"))
                       (subed-srt--jump-to-subtitle-text 1)
                       (expect (subed-insert-subtitle arg) :to-equal 71)
                       (expect (buffer-string) :to-equal (concat "1\n"
                                                                 "00:00:59,000 --> 00:01:00,000\n"
                                                                 "Foo.\n\n"
                                                                 "0\n"
                                                                 "00:01:00,000 --> 00:01:00,000\n"
                                                                 "\n\n"
                                                                 "0\n"
                                                                 "00:01:00,000 --> 00:01:00,000\n"
                                                                 "\n\n"
                                                                 "2\n"
                                                                 "00:01:00,100 --> 00:01:02,000\n"
                                                                 "Bar.\n"))
                       (expect (point) :to-equal 71)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        )
      (describe "before the first subtitle"
        (it "a single subtitle."
          (cl-loop for arg in (list '- -1 (list 4)) do
                   (with-temp-buffer
                     (insert (concat "1\n"
                                     "00:00:00,050 --> 00:00:02,000\n"
                                     "Foo.\n"))
                     (subed-srt--jump-to-subtitle-text 1)
                     (expect (subed-insert-subtitle arg) :to-equal 33)
                     (expect (buffer-string) :to-equal (concat "0\n"
                                                               "00:00:00,000 --> 00:00:00,000\n"
                                                               "\n\n"
                                                               "1\n"
                                                               "00:00:00,050 --> 00:00:02,000\n"
                                                               "Foo.\n"))
                     (expect (point) :to-equal 33)
                     (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                     (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list -2 (list 16)) do
                   (with-temp-buffer
                     (insert (concat "1\n"
                                     "00:00:00,100 --> 00:00:01,500\n"
                                     "Foo.\n"))
                     (subed-srt--jump-to-subtitle-text 1)
                     (expect (subed-insert-subtitle arg) :to-equal 33)
                     (expect (buffer-string) :to-equal (concat "0\n"
                                                               "00:00:00,000 --> 00:00:00,000\n"
                                                               "\n\n"
                                                               "0\n"
                                                               "00:00:00,000 --> 00:00:00,000\n"
                                                               "\n\n"
                                                               "1\n"
                                                               "00:00:00,100 --> 00:00:01,500\n"
                                                               "Foo.\n"))
                     (expect (point) :to-equal 33)
                     (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                     (spy-calls-reset 'subed-regenerate-ids-soon))))
        )
      )
    )
  )

(describe "Inserting adjacent"
  (before-each
    (spy-on 'subed-regenerate-ids-soon))
  (describe "in an empty buffer,"
    (describe "appending"
      (it "a single subtile."
        (cl-loop for arg in (list nil 1) do
                 (with-temp-buffer
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:00,000 --> 00:00:01,000\n"
                                                             "\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtiles."
        (cl-loop for arg in (list 2) do
                 (with-temp-buffer
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:00,000 --> 00:00:01,000\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:00:01,100 --> 00:00:02,100\n"
                                                             "\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    (describe "prepending"
      (it "a single subtile."
        (cl-loop for arg in (list '- -1 (list 4)) do
                 (with-temp-buffer
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:00,000 --> 00:00:01,000\n"
                                                             "\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtiles."
        (cl-loop for arg in (list -2 (list -16)) do
                 (with-temp-buffer
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:00,000 --> 00:00:01,000\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:00:01,100 --> 00:00:02,100\n"
                                                             "\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    )
  (describe "in a non-empty buffer"
    (describe "prepending between subtitles"
      (it "a single subtitle."
        (cl-loop for arg in (list '- -1 (list 4)) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n\n"
                                   "2\n"
                                   "00:02:00,000 --> 00:02:01,000\n"
                                   "Bar.\n"))
                   (subed-jump-to-subtitle-text 2)
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:58,900 --> 00:01:59,900\n"
                                                             "\n\n"
                                                             "2\n"
                                                             "00:02:00,000 --> 00:02:01,000\n"
                                                             "Bar.\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtitles."
        (cl-loop for arg in (list -2 (list 16)) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n\n"
                                   "2\n"
                                   "00:02:00,000 --> 00:02:01,000\n"
                                   "Bar.\n"))
                   (subed-jump-to-subtitle-text 2)
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:57,800 --> 00:01:58,800\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:01:58,900 --> 00:01:59,900\n"
                                                             "\n\n"
                                                             "2\n"
                                                             "00:02:00,000 --> 00:02:01,000\n"
                                                             "Bar.\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    (describe "appending between subtitles"
      (it "a single subtitle."
        (cl-loop for arg in (list nil 1) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n\n"
                                   "2\n"
                                   "00:02:00,000 --> 00:02:01,000\n"
                                   "Bar.\n"))
                   (subed-jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:00,100 --> 00:01:01,100\n"
                                                             "\n\n"
                                                             "2\n"
                                                             "00:02:00,000 --> 00:02:01,000\n"
                                                             "Bar.\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtitles."
        (cl-loop for arg in (list 2) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n\n"
                                   "2\n"
                                   "00:02:00,000 --> 00:02:01,000\n"
                                   "Bar.\n"))
                   (subed-jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:00,100 --> 00:01:01,100\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:01:01,200 --> 00:01:02,200\n"
                                                             "\n\n"
                                                             "2\n"
                                                             "00:02:00,000 --> 00:02:01,000\n"
                                                             "Bar.\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    (describe "prepending to the first subtitle"
      (it "a single subtitle."
        (cl-loop for arg in (list '- -1 (list 4)) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:01:00,000 --> 00:01:01,000\n"
                                   "Foo.\n"))
                   (subed-srt--jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:58,900 --> 00:00:59,900\n"
                                                             "\n\n"
                                                             "1\n"
                                                             "00:01:00,000 --> 00:01:01,000\n"
                                                             "Foo.\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtitles."
        (cl-loop for arg in (list -2 (list 16)) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:01:00,000 --> 00:01:01,000\n"
                                   "Foo.\n"))
                   (subed-srt--jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                   (expect (buffer-string) :to-equal (concat "0\n"
                                                             "00:00:57,800 --> 00:00:58,800\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:00:58,900 --> 00:00:59,900\n"
                                                             "\n\n"
                                                             "1\n"
                                                             "00:01:00,000 --> 00:01:01,000\n"
                                                             "Foo.\n"))
                   (expect (point) :to-equal 33)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      )
    (describe "appending to the last subtitle"
      (it "a single subtitle."
        (cl-loop for arg in (list nil 1) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n"))
                   (subed-srt--jump-to-subtitle-text 1)
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:00,100 --> 00:01:01,100\n"
                                                             "\n"))
                   (expect (point) :to-equal 71)
                   (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                   (spy-calls-reset 'subed-regenerate-ids-soon))))
      (it "multiple subtitles."
        (cl-loop for arg in (list 2) do
                 (with-temp-buffer
                   (insert (concat "1\n"
                                   "00:00:59,000 --> 00:01:00,000\n"
                                   "Foo.\n"))
                   (subed-srt--jump-to-subtitle-text 3)
                   (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                   (expect (buffer-string) :to-equal (concat "1\n"
                                                             "00:00:59,000 --> 00:01:00,000\n"
                                                             "Foo.\n\n"
                                                             "0\n"
                                                             "00:01:00,100 --> 00:01:01,100\n"
                                                             "\n\n"
                                                             "0\n"
                                                             "00:01:01,200 --> 00:01:02,200\n"
                                                             "\n"))
                   (expect (point) :to-equal 71))))
      )
    (describe "when there is not enough time for the subtitles"
      (describe "to append"
        (it "a single subtitle."
          (cl-loop for arg in (list nil 1) do
                   (with-temp-buffer
                     (insert (concat "1\n"
                                     "00:00:59,000 --> 00:01:00,000\n"
                                     "Foo.\n\n"
                                     "2\n"
                                     "00:01:00,500 --> 00:01:05,000\n"
                                     "Bar.\n"))
                     (subed-srt--jump-to-subtitle-text 1)
                     (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                     (expect (buffer-string) :to-equal (concat "1\n"
                                                               "00:00:59,000 --> 00:01:00,000\n"
                                                               "Foo.\n\n"
                                                               "0\n"
                                                               "00:01:00,100 --> 00:01:00,400\n"
                                                               "\n\n"
                                                               "2\n"
                                                               "00:01:00,500 --> 00:01:05,000\n"
                                                               "Bar.\n"))
                     (expect (point) :to-equal 71)
                     (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                     (spy-calls-reset 'subed-regenerate-ids-soon))))
        (it "multiple subtitles."
          (cl-loop for arg in (list 2) do
                   (with-temp-buffer
                     (insert (concat "1\n"
                                     "00:00:59,000 --> 00:01:00,000\n"
                                     "Foo.\n\n"
                                     "2\n"
                                     "00:01:00,500 --> 00:01:05,000\n"
                                     "Bar.\n"))
                     (subed-srt--jump-to-subtitle-text 1)
                     (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                     (expect (buffer-string) :to-equal (concat "1\n"
                                                               "00:00:59,000 --> 00:01:00,000\n"
                                                               "Foo.\n\n"
                                                               "0\n"
                                                               "00:01:00,100 --> 00:01:00,200\n"
                                                               "\n\n"
                                                               "0\n"
                                                               "00:01:00,300 --> 00:01:00,400\n"
                                                               "\n\n"
                                                               "2\n"
                                                               "00:01:00,500 --> 00:01:05,000\n"
                                                               "Bar.\n"))
                     (expect (point) :to-equal 71))))
        )
      (describe "to prepend"
        (describe "between subtitles"
          (it "a single subtitle."
            (cl-loop for arg in (list '- -1 (list 4)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:59,000 --> 00:01:00,000\n"
                                       "Foo.\n\n"
                                       "2\n"
                                       "00:01:00,700 --> 00:01:05,000\n"
                                       "Bar.\n"))
                       (subed-srt--jump-to-subtitle-text 2)
                       (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                       (expect (buffer-string) :to-equal (concat "1\n"
                                                                 "00:00:59,000 --> 00:01:00,000\n"
                                                                 "Foo.\n\n"
                                                                 "0\n"
                                                                 "00:01:00,100 --> 00:01:00,600\n"
                                                                 "\n\n"
                                                                 "2\n"
                                                                 "00:01:00,700 --> 00:01:05,000\n"
                                                                 "Bar.\n"))
                       (expect (point) :to-equal 71)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list -2 (list 16)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:59,000 --> 00:01:00,000\n"
                                       "Foo.\n\n"
                                       "2\n"
                                       "00:01:00,500 --> 00:01:05,000\n"
                                       "Bar.\n"))
                       (subed-srt--jump-to-subtitle-text 2)
                       (expect (subed-insert-subtitle-adjacent arg) :to-equal 71)
                       (expect (buffer-string) :to-equal (concat "1\n"
                                                                 "00:00:59,000 --> 00:01:00,000\n"
                                                                 "Foo.\n\n"
                                                                 "0\n"
                                                                 "00:01:00,100 --> 00:01:00,200\n"
                                                                 "\n\n"
                                                                 "0\n"
                                                                 "00:01:00,300 --> 00:01:00,400\n"
                                                                 "\n\n"
                                                                 "2\n"
                                                                 "00:01:00,500 --> 00:01:05,000\n"
                                                                 "Bar.\n"))
                       (expect (point) :to-equal 71)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        (describe "before the first subtitle"
          (it "a single subtitle."
            (cl-loop for arg in (list '- -1 (list 4)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:01,000 --> 00:00:02,000\n"
                                       "Foo.\n"))
                       (subed-srt--jump-to-subtitle-text 1)
                       (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                       (expect (buffer-string) :to-equal (concat "0\n"
                                                                 "00:00:00,100 --> 00:00:00,900\n"
                                                                 "\n\n"
                                                                 "1\n"
                                                                 "00:00:01,000 --> 00:00:02,000\n"
                                                                 "Foo.\n"))
                       (expect (point) :to-equal 33)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          (it "multiple subtitles."
            (cl-loop for arg in (list -2 (list 16)) do
                     (with-temp-buffer
                       (insert (concat "1\n"
                                       "00:00:00,800 --> 00:00:03,000\n"
                                       "Foo.\n"))
                       (subed-srt--jump-to-subtitle-text 2)
                       (expect (subed-insert-subtitle-adjacent arg) :to-equal 33)
                       (expect (buffer-string) :to-equal (concat "0\n"
                                                                 "00:00:00,100 --> 00:00:00,350\n"
                                                                 "\n\n"
                                                                 "0\n"
                                                                 "00:00:00,450 --> 00:00:00,700\n"
                                                                 "\n\n"
                                                                 "1\n"
                                                                 "00:00:00,800 --> 00:00:03,000\n"
                                                                 "Foo.\n"))
                       (expect (point) :to-equal 33)
                       (expect 'subed-regenerate-ids-soon :to-have-been-called-times 1)
                       (spy-calls-reset 'subed-regenerate-ids-soon))))
          )
        )
      )

    )
  )

(describe "Syncing player to point"
  :var (subed-mpv-playback-position)
  (before-each
    (setq subed-mpv-playback-position 0)
    (spy-on 'subed-subtitle-msecs-start :and-return-value 5000)
    (spy-on 'subed-subtitle-msecs-stop :and-return-value 6500)
    (spy-on 'subed-mpv-jump))
  (it "does not seek player if point is on current subtitle."
    (setq subed-mpv-playback-position 5000)
    (subed--sync-player-to-point)
    (expect 'subed-mpv-jump :not :to-have-been-called)
    (setq subed-mpv-playback-position 6500)
    (subed--sync-player-to-point)
    (expect 'subed-mpv-jump :not :to-have-been-called))
  (it "seeks player if point is on future subtitle."
    (setq subed-mpv-playback-position 6501)
    (subed--sync-player-to-point)
    (expect 'subed-mpv-jump :to-have-been-called-with 5000))
  (it "seeks player if point is on past subtitle."
    (setq subed-mpv-playback-position 4999)
    (subed--sync-player-to-point)
    (expect 'subed-mpv-jump :to-have-been-called-with 5000))
  )

(describe "Temporarily disabling point-to-player syncing"
  (before-each
    (spy-on 'subed-disable-sync-point-to-player))
  (describe "when point-to-player syncing is disabled"
    (before-each
      (spy-on 'subed-sync-point-to-player-p :and-return-value nil)
      (spy-on 'run-at-time))
    (it "does not disable point-to-player syncing."
      (subed-disable-sync-point-to-player-temporarily)
      (expect 'subed-disable-sync-point-to-player :not :to-have-been-called))
    (it "does not schedule re-enabling of point-to-player syncing."
      (subed-disable-sync-point-to-player-temporarily)
      (expect 'run-at-time :not :to-have-been-called)
      (expect subed--point-sync-delay-after-motion-timer :to-be nil))
    )
  (describe "when point-to-player syncing is enabled"
    :var (subed--point-sync-delay-after-motion-timer)
    (before-each
      (spy-on 'subed-sync-point-to-player-p :and-return-value t)
      (spy-on 'run-at-time :and-return-value "mock timer")
      (spy-on 'cancel-timer)
      (setq subed--point-sync-delay-after-motion-timer nil))
    (it "disables point-to-player syncing."
      (subed-disable-sync-point-to-player-temporarily)
      (expect 'subed-disable-sync-point-to-player :to-have-been-called))
    (it "schedules re-enabling of point-to-player syncing."
      (subed-disable-sync-point-to-player-temporarily)
      (expect 'run-at-time :to-have-been-called-with
              subed-point-sync-delay-after-motion nil
              '(closure (t) nil
                        (setq subed--point-sync-delay-after-motion-timer nil)
                        (subed-enable-sync-point-to-player :quiet))))
    (it "cancels previously scheduled re-enabling of point-to-player syncing."
      (subed-disable-sync-point-to-player-temporarily)
      (expect 'cancel-timer :not :to-have-been-called-with "mock timer")
      (subed-disable-sync-point-to-player-temporarily)
      (expect 'cancel-timer :to-have-been-called-with "mock timer")
      (expect 'cancel-timer :to-have-been-called-times 1)
      (subed-disable-sync-point-to-player-temporarily)
      (expect 'cancel-timer :to-have-been-called-with "mock timer")
      (expect 'cancel-timer :to-have-been-called-times 2))
    )
  )
