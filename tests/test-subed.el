(add-to-list 'load-path "./subed")
(require 'subed)

(describe "Syncing player to point"
          :var (subed-mpv-playback-position)
          (before-each
           (setq subed-mpv-playback-position 0)
           (spy-on 'subed--subtitle-msecs-start :and-return-value 5000)
           (spy-on 'subed--subtitle-msecs-stop :and-return-value 6500)
           (spy-on 'subed-mpv-jump)
           (spy-on 'subed-disable-sync-point-to-player-temporarily))
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

(describe "Syncing point to player"
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
                                   (subed-enable-sync-point-to-player))))
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
