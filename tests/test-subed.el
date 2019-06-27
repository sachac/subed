(add-to-list 'load-path "./subed")
(require 'subed)
(require 'subed-srt)

(describe "Iterating over subtitles"
          (it "without beginning and end."
              (with-temp-buffer
                (insert mock-srt-data)
                (subed-jump-to-subtitle-time-stop 1)
                (subed--for-each-subtitle nil nil
                  (expect (looking-at "^[0-9]$") :to-be t)
                  (forward-line 2)
                  (kill-line)
                  (insert "Hello."))
                (expect (subed-srt--subtitle-text 1) :to-equal "Hello.")
                (expect (subed-srt--subtitle-text 2) :to-equal "Bar.")
                (expect (subed-srt--subtitle-text 3) :to-equal "Baz.")
                (expect (point) :to-equal 20)
                (subed-jump-to-subtitle-time-stop 2)
                (subed--for-each-subtitle nil nil
                  (expect (looking-at "^[0-9]$") :to-be t)
                  (forward-line 2)
                  (kill-line)
                  (insert "HEllo."))
                (expect (subed-srt--subtitle-text 1) :to-equal "Hello.")
                (expect (subed-srt--subtitle-text 2) :to-equal "HEllo.")
                (expect (subed-srt--subtitle-text 3) :to-equal "Baz.")
                (expect (point) :to-equal 60)
                (subed-jump-to-subtitle-time-stop 3)
                (subed--for-each-subtitle nil nil
                  (expect (looking-at "^[0-9]$") :to-be t)
                  (forward-line 2)
                  (kill-line)
                  (insert "HELlo."))
                (expect (subed-srt--subtitle-text 1) :to-equal "Hello.")
                (expect (subed-srt--subtitle-text 2) :to-equal "HEllo.")
                (expect (subed-srt--subtitle-text 3) :to-equal "HELlo.")
                (expect (point) :to-equal 99)))
          (it "with only the beginning."
              (with-temp-buffer
                (insert mock-srt-data)
                (subed-jump-to-subtitle-time-start 1)
                (expect (point) :to-equal 3)
                (subed--for-each-subtitle 71 nil
                  (expect (looking-at "^[0-9]$") :to-be t)
                  (forward-line 2)
                  (kill-line)
                  (insert "Hello."))
                (expect (subed-srt--subtitle-text 1) :to-equal "Foo.")
                (expect (subed-srt--subtitle-text 2) :to-equal "Hello.")
                (expect (subed-srt--subtitle-text 3) :to-equal "Hello.")
                (expect (point) :to-equal 3)))
          (describe "with beginning and end,"
                    (it "excluding subtitles above."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (subed-jump-to-subtitle-time-stop 1)
                          (subed--for-each-subtitle 71 79
                            (expect (looking-at "^[0-9]$") :to-be t)
                            (forward-line 2)
                            (kill-line)
                            (insert "Hello."))
                          (expect (subed--subtitle-text 1) :to-equal "Foo.")
                          (expect (subed--subtitle-text 2) :to-equal "Hello.")
                          (expect (subed--subtitle-text 3) :to-equal "Hello.")
                          (expect (point) :to-equal 20)))
                    (it "excluding subtitles below."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (subed-jump-to-subtitle-time-stop 3)
                          (subed--for-each-subtitle 5 76
                            (expect (looking-at "^[0-9]$") :to-be t)
                            (forward-line 2)
                            (kill-line)
                            (insert "Hello."))
                          (expect (subed--subtitle-text 1) :to-equal "Hello.")
                          (expect (subed--subtitle-text 2) :to-equal "Hello.")
                          (expect (subed--subtitle-text 3) :to-equal "Baz.")
                          (expect (point) :to-equal 99)))
                    )
          )

(describe "Moving"
          (it "adjusts start and stop time by the same amount."
              (with-temp-buffer
                (insert mock-srt-data)
                (cl-loop for sub-id in '(1 2 3) do
                         (subed-jump-to-subtitle-id sub-id)
                         (let ((orig-start (subed--subtitle-msecs-start))
                               (orig-stop (subed--subtitle-msecs-stop)))
                           (subed-move-subtitle-forward 100)
                           (expect (subed--subtitle-msecs-start) :to-equal (+ orig-start 100))
                           (expect (subed--subtitle-msecs-stop) :to-equal (+ orig-stop 100))
                           (subed-move-subtitle-backward 100)
                           (expect (subed--subtitle-msecs-start) :to-equal orig-start)
                           (expect (subed--subtitle-msecs-stop) :to-equal orig-stop)))))
          (it "adjusts start and stop time by the same amount when adding fails."
              (with-temp-buffer
                (insert (concat "1\n"
                                "00:00:01,000 --> 00:00:01,600\n"
                                "Foo.\n\n"
                                "2\n"
                                "00:00:02,000 --> 00:00:03,000\n"
                                "Bar.\n"))
                (subed-jump-to-subtitle-id 1)
                (subed-move-subtitle-forward 1000)
                (expect (subed--subtitle-msecs-start) :to-equal 1300)
                (expect (subed--subtitle-msecs-stop) :to-equal 1900)))
          (it "adjusts start and stop time by the same amount when subtracting fails."
              (with-temp-buffer
                (insert (concat "1\n"
                                "00:00:01,000 --> 00:00:01,600\n"
                                "Foo.\n\n"
                                "2\n"
                                "00:00:02,000 --> 00:00:03,000\n"
                                "Bar.\n"))
                (subed-jump-to-subtitle-id 2)
                (subed-move-subtitle-backward 1000)
                (expect (subed--subtitle-msecs-start) :to-equal 1700)
                (expect (subed--subtitle-msecs-stop) :to-equal 2700)))
          (it "does not adjust start time if adjusting stop time fails."
              (with-temp-buffer
                (insert (concat "1\n"
                                "00:00:01,000 --> 00:00:02,000\n"
                                "Foo.\n\n"
                                "2\n"
                                "00:00:02,000 --> 00:00:03,000\n"
                                "Bar.\n"))
                (subed-jump-to-subtitle-id 1)
                (expect (subed-move-subtitle-forward 1) :to-be nil)
                (expect (subed--subtitle-msecs-start 1) :to-equal 1000)
                (expect (subed--subtitle-msecs-stop 1) :to-equal 2000)
                (expect (subed--subtitle-msecs-start 2) :to-equal 2000)
                (expect (subed--subtitle-msecs-stop 2) :to-equal 3000)))
          (it "does not adjust stop time if adjusting start time fails."
              (with-temp-buffer
                (insert (concat "1\n"
                                "00:00:01,000 --> 00:00:02,000\n"
                                "Foo.\n\n"
                                "2\n"
                                "00:00:02,000 --> 00:00:03,000\n"
                                "Bar.\n"))
                (subed-jump-to-subtitle-id 2)
                (expect (subed-move-subtitle-backward 1) :to-be nil)
                (expect (subed--subtitle-msecs-start 1) :to-equal 1000)
                (expect (subed--subtitle-msecs-stop 1) :to-equal 2000)
                (expect (subed--subtitle-msecs-start 2) :to-equal 2000)
                (expect (subed--subtitle-msecs-stop 2) :to-equal 3000)))
          (describe "adjusts subtitles in the active region,"
                    (it "excluding the first subtitle."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (let ((beg (subed-jump-to-subtitle-text 2))
                                (end (subed-jump-to-subtitle-time-start 3))
                                (orig-start-1 (subed--subtitle-msecs-start 1))
                                (orig-stop-1 (subed--subtitle-msecs-stop 1))
                                (orig-start-2 (subed--subtitle-msecs-start 2))
                                (orig-stop-2 (subed--subtitle-msecs-stop 2))
                                (orig-start-3 (subed--subtitle-msecs-start 3))
                                (orig-stop-3 (subed--subtitle-msecs-stop 3)))
                            (spy-on 'use-region-p :and-return-value t)
                            (spy-on 'region-beginning :and-return-value beg)
                            (spy-on 'region-end :and-return-value end)
                            (subed-move-subtitle-forward 100)
                            (expect (subed--subtitle-msecs-start 1) :to-equal orig-start-1)
                            (expect (subed--subtitle-msecs-stop 1) :to-equal orig-stop-1)
                            (expect (subed--subtitle-msecs-start 2) :to-equal (+ orig-start-2 100))
                            (expect (subed--subtitle-msecs-stop 2) :to-equal (+ orig-stop-2 100))
                            (expect (subed--subtitle-msecs-start 3) :to-equal (+ orig-start-3 100))
                            (expect (subed--subtitle-msecs-stop 3) :to-equal (+ orig-stop-3 100))
                            (subed-move-subtitle-backward 200)
                            (expect (subed--subtitle-msecs-start 1) :to-equal orig-start-1)
                            (expect (subed--subtitle-msecs-stop 1) :to-equal orig-stop-1)
                            (expect (subed--subtitle-msecs-start 2) :to-equal (- orig-start-2 100))
                            (expect (subed--subtitle-msecs-stop 2) :to-equal (- orig-stop-2 100))
                            (expect (subed--subtitle-msecs-start 3) :to-equal (- orig-start-3 100))
                            (expect (subed--subtitle-msecs-stop 3) :to-equal (- orig-stop-3 100)))))
                    (it "excluding the last subtitle."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (let ((beg (subed-jump-to-subtitle-time-stop 1))
                                (end (1+ (subed-jump-to-subtitle-end 2)))
                                (orig-start-1 (subed--subtitle-msecs-start 1))
                                (orig-stop-1 (subed--subtitle-msecs-stop 1))
                                (orig-start-2 (subed--subtitle-msecs-start 2))
                                (orig-stop-2 (subed--subtitle-msecs-stop 2))
                                (orig-start-3 (subed--subtitle-msecs-start 3))
                                (orig-stop-3 (subed--subtitle-msecs-stop 3)))
                            (spy-on 'use-region-p :and-return-value t)
                            (spy-on 'region-beginning :and-return-value beg)
                            (spy-on 'region-end :and-return-value end)
                            (subed-move-subtitle-forward 100)
                            (expect (subed--subtitle-msecs-start 1) :to-equal (+ orig-start-1 100))
                            (expect (subed--subtitle-msecs-stop 1) :to-equal (+ orig-stop-1 100))
                            (expect (subed--subtitle-msecs-start 2) :to-equal (+ orig-start-2 100))
                            (expect (subed--subtitle-msecs-stop 2) :to-equal (+ orig-stop-2 100))
                            (expect (subed--subtitle-msecs-start 3) :to-equal orig-start-3)
                            (expect (subed--subtitle-msecs-stop 3) :to-equal orig-stop-3)
                            (subed-move-subtitle-backward 300)
                            (expect (subed--subtitle-msecs-start 1) :to-equal (- orig-start-1 200))
                            (expect (subed--subtitle-msecs-stop 1) :to-equal (- orig-stop-1 200))
                            (expect (subed--subtitle-msecs-start 2) :to-equal (- orig-start-2 200))
                            (expect (subed--subtitle-msecs-stop 2) :to-equal (- orig-stop-2 200))
                            (expect (subed--subtitle-msecs-start 3) :to-equal orig-start-3)
                            (expect (subed--subtitle-msecs-stop 3) :to-equal orig-stop-3)
                            )))
                    )
          (it "disables subtitle replay while moving subtitles."
              (with-temp-buffer
                (insert mock-srt-data)
                (subed-enable-replay-adjusted-subtitle :quiet)
                (spy-on 'subed-enable-replay-adjusted-subtitle :and-call-through)
                (spy-on 'subed-disable-replay-adjusted-subtitle :and-call-through)
                (spy-on 'subed--adjust-subtitle-start :and-call-fake
                        (lambda (msecs) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
                (spy-on 'subed--adjust-subtitle-stop :and-call-fake
                        (lambda (msecs) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
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
                (spy-on 'subed--adjust-subtitle-start :and-call-fake
                        (lambda (msecs) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
                (spy-on 'subed--adjust-subtitle-stop :and-call-fake
                        (lambda (msecs) (expect (subed-replay-adjusted-subtitle-p) :to-be nil)))
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

(describe "Syncing player to point"
          :var (subed-mpv-playback-position)
          (before-each
           (setq subed-mpv-playback-position 0)
           (spy-on 'subed--subtitle-msecs-start :and-return-value 5000)
           (spy-on 'subed--subtitle-msecs-stop :and-return-value 6500)
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
                                '(lambda ()
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
