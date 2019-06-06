(add-to-list 'load-path "./subed")
(require 'subed)

(defvar mock-srt-data
  "1
00:01:01,000 --> 00:01:05,123
Foo.

2
00:02:02,234 --> 00:02:10,345
Bar.

3
00:03:03,456 --> 00:03:15,567
Baz.
")

(describe "Getting"
          (describe "the subtitle ID"
                    (it "returns the subtitle ID if possible."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (subed-srt-move-to-subtitle-text 2)
                          (expect (subed-srt--subtitle-id) :to-equal 2)))
                    (it "returns nil if no subtitle ID can be found."
                        (with-temp-buffer
                          (expect (subed-srt--subtitle-id) :to-equal nil)))
                    )
          (describe "the subtitle ID at playback time"
                    (it "returns subtitle ID if time is equal to start time."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (cl-loop for target-id from 1 to 3 do
                                   (let ((msecs (subed-srt--subtitle-msecs-start target-id)))
                                     (cl-loop for outset-id from 1 to 3 do
                                              (progn
                                                (subed-srt-move-to-subtitle-id outset-id)
                                                (expect (subed-srt--subtitle-id-at-msecs msecs) :to-equal target-id)))))))
                    (it "returns subtitle ID if time is equal to stop time."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (cl-loop for target-id from 1 to 3 do
                                   (let ((msecs (subed-srt--subtitle-msecs-stop target-id)))
                                     (cl-loop for outset-id from 1 to 3 do
                                              (progn
                                                (subed-srt-move-to-subtitle-id outset-id)
                                                (expect (subed-srt--subtitle-id-at-msecs msecs) :to-equal target-id)))))))
                    (it "returns subtitle ID if time is between start and stop time."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (cl-loop for target-id from 1 to 3 do
                                   (let ((msecs (+ 1 (subed-srt--subtitle-msecs-start target-id))))
                                     (cl-loop for outset-id from 1 to 3 do
                                              (progn
                                                (subed-srt-move-to-subtitle-id outset-id)
                                                (expect (subed-srt--subtitle-id-at-msecs msecs) :to-equal target-id)))))))
                    (it "returns first subtitle ID if time is before the first subtitle's start time."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (let ((msecs (- (save-excursion
                                            (goto-char (point-min))
                                            (subed-srt--subtitle-msecs-start)) 1)))
                            (cl-loop for outset-id from 1 to 3 do
                                     (progn
                                       (subed-srt-move-to-subtitle-id outset-id)
                                       (expect (subed-srt--subtitle-id-at-msecs msecs) :to-equal 1))))))
                    (it "returns last subtitle ID if time is after last subtitle's start time."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (let ((msecs (+ (save-excursion
                                            (goto-char (point-max))
                                            (subed-srt--subtitle-msecs-stop)) 1)))
                            (cl-loop for outset-id from 1 to 3 do
                                     (progn
                                       (subed-srt-move-to-subtitle-id outset-id)
                                       (expect (subed-srt--subtitle-id-at-msecs msecs) :to-equal 3))))))
                    (it "returns previous subtitle ID when time is between subtitles"
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (cl-loop for target-id from 1 to 2 do
                                   (let ((msecs (+ (subed-srt--subtitle-msecs-stop target-id) 1)))
                                     (cl-loop for outset-id from 1 to 3 do
                                              (progn
                                                (subed-srt-move-to-subtitle-id outset-id)
                                                (expect (subed-srt--subtitle-id-at-msecs msecs) :to-equal target-id))))

                                   (let ((msecs (- (subed-srt--subtitle-msecs-start (+ target-id 1)) 1)))
                                     (cl-loop for outset-id from 1 to 3 do
                                              (progn
                                                (subed-srt-move-to-subtitle-id outset-id)
                                                (expect (subed-srt--subtitle-id-at-msecs msecs) :to-equal target-id)))))))
                    )
          )


(describe "Adjusting subtitle start/stop time"
          :var (subed-subtitle-time-adjusted-hook)
          (it "runs the appropriate hook."
              (let ((foo (setf (symbol-function 'foo) (lambda (sub-id msecs) ()))))
                (spy-on 'foo)
                (add-hook 'subed-subtitle-time-adjusted-hook 'foo)
                (with-temp-buffer
                  (insert mock-srt-data)
                  (subed-srt-increase-start-time-100ms)
                  (expect 'foo :to-have-been-called-with 3 183556)
                  (expect 'foo :to-have-been-called-times 1)
                  (subed-srt-move-to-subtitle-id 1)
                  (subed-srt-increase-stop-time-100ms)
                  (expect 'foo :to-have-been-called-with 1 65223)
                  (expect 'foo :to-have-been-called-times 2)
                  (subed-srt-move-to-subtitle-end 2)
                  (subed-srt-decrease-start-time-100ms)
                  (expect 'foo :to-have-been-called-with 2 122134)
                  (expect 'foo :to-have-been-called-times 3)
                  (subed-srt-move-to-subtitle-text 3)
                  (subed-srt-decrease-stop-time-100ms)
                  (expect 'foo :to-have-been-called-with 3 195467)
                  (expect 'foo :to-have-been-called-times 4))))
          (it "adjusts the start/stop time."
              (with-temp-buffer
                (insert mock-srt-data)
                (subed-srt-move-to-subtitle-id 1)
                (subed-srt-increase-start-time-100ms)
                (expect (save-excursion (subed-srt-move-to-subtitle-time-start)
                                        (thing-at-point 'line)) :to-equal "00:01:01,100 --> 00:01:05,123\n")
                (subed-srt-decrease-start-time-100ms)
                (subed-srt-decrease-start-time-100ms)
                (expect (save-excursion (subed-srt-move-to-subtitle-time-start)
                                        (thing-at-point 'line)) :to-equal "00:01:00,900 --> 00:01:05,123\n")
                (subed-srt-increase-stop-time-100ms)
                (subed-srt-increase-stop-time-100ms)
                (expect (save-excursion (subed-srt-move-to-subtitle-time-start)
                                        (thing-at-point 'line)) :to-equal "00:01:00,900 --> 00:01:05,323\n")
                (subed-srt-decrease-stop-time-100ms)
                (expect (save-excursion (subed-srt-move-to-subtitle-time-start)
                                        (thing-at-point 'line)) :to-equal "00:01:00,900 --> 00:01:05,223\n")))
          )


(describe "Moving"
          (describe "to current subtitle ID"
                    (it "returns ID's point when point is already on the ID."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-min))
                          (expect (thing-at-point 'word) :to-equal "1")
                          (expect (subed-srt-move-to-subtitle-id) :to-equal 1)
                          (expect (thing-at-point 'word) :to-equal "1")))
                    (it "returns ID's point when point is on the duration."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (search-backward ",234")
                          (expect (thing-at-point 'word) :to-equal "02")
                          (expect (subed-srt-move-to-subtitle-id) :to-equal 39)
                          (expect (thing-at-point 'word) :to-equal "2")))
                    (it "returns ID's point when point is on the text."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (search-backward "Baz.")
                          (expect (thing-at-point 'word) :to-equal "Baz")
                          (expect (subed-srt-move-to-subtitle-id) :to-equal 77)
                          (expect (thing-at-point 'word) :to-equal "3")))
                    (it "returns ID's point when point is after the text."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-min))
                          (search-forward "Bar.\n")
                          (expect (thing-at-point 'line) :to-equal "\n")
                          (expect (subed-srt-move-to-subtitle-id) :to-equal 39)
                          (expect (thing-at-point 'word) :to-equal "2")))
                    (it "returns nil if buffer is empty."
                        (with-temp-buffer
                          (expect (buffer-string) :to-equal "")
                          (expect (subed-srt-move-to-subtitle-id) :to-equal nil)))
                    )
          (describe "to specific subtitle ID"
                    (it "returns ID's point if wanted ID exists."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-max))
                          (expect (subed-srt-move-to-subtitle-id 2) :to-equal 39)
                          (expect (thing-at-point 'word) :to-equal "2")
                          (expect (subed-srt-move-to-subtitle-id 1) :to-equal 1)
                          (expect (thing-at-point 'word) :to-equal "1")
                          (expect (subed-srt-move-to-subtitle-id 3) :to-equal 77)
                          (expect (thing-at-point 'word) :to-equal "3")))
                    (it "returns nil and does not move if wanted ID does not exists."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-min))
                          (search-forward "Foo")
                          (setq stored-point (point))
                          (expect (subed-srt-move-to-subtitle-id 4) :to-equal nil)
                          (expect stored-point :to-equal (point))))
                    )
          (describe "to subtitle ID at specific time"
                    (it "returns ID's point if point changed."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-max))
                          (spy-on 'subed-srt--subtitle-id-at-msecs :and-return-value (point-min))
                          (expect (subed-srt-move-to-subtitle-id-at-msecs 123456) :to-equal (point-min))
                          (expect (point) :to-equal (point-min))
                          (expect 'subed-srt--subtitle-id-at-msecs :to-have-been-called-with 123456)
                          (expect 'subed-srt--subtitle-id-at-msecs :to-have-been-called-times 1)))
                    (it "returns nil if point didn't change."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char 75)
                          (spy-on 'subed-srt--subtitle-id-at-msecs :and-return-value 75)
                          (expect (subed-srt-move-to-subtitle-id-at-msecs 123456) :to-equal nil)
                          (expect (point) :to-equal 75)
                          (expect 'subed-srt--subtitle-id-at-msecs :to-have-been-called-with 123456)
                          (expect 'subed-srt--subtitle-id-at-msecs :to-have-been-called-times 1)))
                    )
          (describe "to subtitle start time"
                    (it "returns start time's point if movement was successful."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-min))
                          (expect (subed-srt-move-to-subtitle-time-start) :to-equal 3)
                          (expect (buffer-substring (point) (+ (point) subed-srt--length-timestamp)) :to-equal "00:01:01,000")
                          (re-search-forward "\n\n")
                          (expect (subed-srt-move-to-subtitle-time-start) :to-equal 41)
                          (expect (buffer-substring (point) (+ (point) subed-srt--length-timestamp)) :to-equal "00:02:02,234")
                          (re-search-forward "\n\n")
                          (expect (subed-srt-move-to-subtitle-time-start) :to-equal 79)
                          (expect (buffer-substring (point) (+ (point) subed-srt--length-timestamp)) :to-equal "00:03:03,456")))
                    (it "returns nil if movement failed."
                        (with-temp-buffer
                          (expect (subed-srt-move-to-subtitle-time-start) :to-equal nil)))
                    )
          (describe "to subtitle stop time"
                    (it "returns stop time's point if movement was successful."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-min))
                          (expect (subed-srt-move-to-subtitle-time-stop) :to-equal 20)
                          (expect (buffer-substring (point) (+ (point) subed-srt--length-timestamp)) :to-equal "00:01:05,123")
                          (re-search-forward "\n\n")
                          (expect (subed-srt-move-to-subtitle-time-stop) :to-equal 58)
                          (expect (buffer-substring (point) (+ (point) subed-srt--length-timestamp)) :to-equal "00:02:10,345")
                          (re-search-forward "\n\n")
                          (expect (subed-srt-move-to-subtitle-time-stop) :to-equal 96)
                          (expect (buffer-substring (point) (+ (point) subed-srt--length-timestamp)) :to-equal "00:03:15,567")))
                    (it "returns nil if movement failed."
                        (with-temp-buffer
                          (expect (subed-srt-move-to-subtitle-time-stop) :to-equal nil)))
                    )
          (describe "to subtitle text"
                    (it "returns subtitle text's point if movement was successful."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-min))
                          (expect (subed-srt-move-to-subtitle-text) :to-equal 33)
                          (expect (point) :to-equal (save-excursion (goto-char (point-max)) (search-backward "Foo.")))
                          (re-search-forward "\n\n")
                          (expect (subed-srt-move-to-subtitle-text) :to-equal 71)
                          (expect (point) :to-equal (save-excursion (goto-char (point-max)) (search-backward "Bar.")))
                          (re-search-forward "\n\n")
                          (expect (subed-srt-move-to-subtitle-text) :to-equal 109)
                          (expect (point) :to-equal (save-excursion (goto-char (point-max)) (search-backward "Baz.")))

                          ))
                    (it "returns nil if movement failed."
                        (with-temp-buffer
                          (expect (subed-srt-move-to-subtitle-time-stop) :to-equal nil)))
                    )
          (describe "to end of subtitle text"
                    (it "returns end of subtitle text's point if movement was successful."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-min))
                          (expect (subed-srt-move-to-subtitle-end) :to-be 37)
                          (expect (looking-back "^Foo.$") :to-be t)
                          (forward-char 2)
                          (expect (subed-srt-move-to-subtitle-end) :to-be 75)
                          (expect (looking-back "^Bar.$") :to-be t)
                          (forward-char 2)
                          (expect (subed-srt-move-to-subtitle-end) :to-be 113)
                          (expect (looking-back "^Baz.$") :to-be t)
                          (goto-char (point-max))
                          (backward-char 2)
                          (expect (subed-srt-move-to-subtitle-end) :to-be 113)
                          (expect (looking-back "^Baz.$") :to-be t)
                          ))
                    (it "returns nil if movement failed."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (goto-char (point-max))
                          (expect (subed-srt-move-to-subtitle-end) :to-be nil)
                          (expect (looking-back "^Baz.$") :to-be nil)
                          (backward-char 1)
                          (expect (subed-srt-move-to-subtitle-end) :to-be nil)
                          (expect (looking-back "^Baz.$") :to-be t)))
                    )
          (describe "to next subtitle ID"
                    (it "returns subtitle ID's point when it moved."
                        (with-temp-buffer
                          (insert mock-srt-data)
                          (subed-srt-move-to-subtitle-id 2)
                          (expect (thing-at-point 'word) :to-equal "2")
                          (expect (subed-srt-forward-subtitle-id) :to-be 77)
                          (expect (thing-at-point 'word) :to-equal "3")))
                    (it "returns nil and doesn't move when point is on the last subtitle and there are trailing lines."
                        (with-temp-buffer
                          (insert (concat mock-srt-data "\n\n"))
                          (subed-srt-move-to-subtitle-text 3)
                          (expect (thing-at-point 'word) :to-equal "Baz")
                          (expect (subed-srt-forward-subtitle-id) :to-be nil)
                          (expect (thing-at-point 'word) :to-equal "Baz")))
                    )
          )


(describe "Killing a subtitle"
          (it "removes it when it is the first one."
              (with-temp-buffer
                (insert mock-srt-data)
                (subed-srt-move-to-subtitle-text 1)
                (subed-srt-subtitle-kill)
                (expect (buffer-string) :to-equal (concat "2\n"
                                                          "00:02:02,234 --> 00:02:10,345\n"
                                                          "Bar.\n"
                                                          "\n"
                                                          "3\n"
                                                          "00:03:03,456 --> 00:03:15,567\n"
                                                          "Baz.\n"))))
          (it "removes it when it is in the middle."
              (with-temp-buffer
                (insert mock-srt-data)
                (subed-srt-move-to-subtitle-text 2)
                (subed-srt-subtitle-kill)
                (expect (buffer-string) :to-equal (concat "1\n"
                                                          "00:01:01,000 --> 00:01:05,123\n"
                                                          "Foo.\n"
                                                          "\n"
                                                          "3\n"
                                                          "00:03:03,456 --> 00:03:15,567\n"
                                                          "Baz.\n"))))
          (it "removes it when it is the last one."
              (with-temp-buffer
                (insert mock-srt-data)
                (subed-srt-move-to-subtitle-text 3)
                (subed-srt-subtitle-kill)
                (expect (buffer-string) :to-equal (concat "1\n"
                                                          "00:01:01,000 --> 00:01:05,123\n"
                                                          "Foo.\n"
                                                          "\n"
                                                          "2\n"
                                                          "00:02:02,234 --> 00:02:10,345\n"
                                                          "Bar.\n"))))
          (it "removes the previous subtitle when point is right above an ID."
              (with-temp-buffer
                (insert mock-srt-data)
                (subed-srt-move-to-subtitle-id 3)
                (backward-char)
                (expect (looking-at "^\n3\n") :to-be t)
                (subed-srt-subtitle-kill)
                (expect (buffer-string) :to-equal (concat "1\n"
                                                          "00:01:01,000 --> 00:01:05,123\n"
                                                          "Foo.\n"
                                                          "\n"
                                                          "3\n"
                                                          "00:03:03,456 --> 00:03:15,567\n"
                                                          "Baz.\n"))))
          )


(describe "Sanitizing"
          (it "removes trailing tabs and spaces from all lines."
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-min))
                (while (re-search-forward "\n" nil t)
                  (replace-match " \n"))
                (expect (buffer-string) :not :to-equal mock-srt-data)
                (subed-srt-sanitize)
                (expect (buffer-string) :to-equal mock-srt-data))
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-min))
                (while (re-search-forward "\n" nil t)
                  (replace-match "\t\n"))
                (expect (buffer-string) :not :to-equal mock-srt-data)
                (subed-srt-sanitize)
                (expect (buffer-string) :to-equal mock-srt-data)))
          (it "removes leading tabs and spaces from all lines."
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-min))
                (while (re-search-forward "\n" nil t)
                  (replace-match "\n "))
                (expect (buffer-string) :not :to-equal mock-srt-data)
                (subed-srt-sanitize)
                (expect (buffer-string) :to-equal mock-srt-data))
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-min))
                (while (re-search-forward "\n" nil t)
                  (replace-match "\n\t"))
                (expect (buffer-string) :not :to-equal mock-srt-data)
                (subed-srt-sanitize)
                (expect (buffer-string) :to-equal mock-srt-data))
              )
          (it "removes excessive newlines between subtitles."
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-min))
                (while (re-search-forward "\n\n" nil t)
                  (replace-match "\n \n  \t  \t\t  \n\n   \n \n \t\n"))
                (expect (buffer-string) :not :to-equal mock-srt-data)
                (subed-srt-sanitize)
                (expect (buffer-string) :to-equal mock-srt-data)))
          (it "removes empty lines from beginning of buffer."
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-min))
                (insert " \n\t\n")
                (expect (buffer-string) :not :to-equal mock-srt-data)
                (subed-srt-sanitize)
                (expect (buffer-string) :to-equal mock-srt-data)))
          (it "removes empty lines from end of buffer."
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-max))
                (insert " \n\t\n\n")
                (expect (buffer-string) :not :to-equal mock-srt-data)
                (subed-srt-sanitize)
                (expect (buffer-string) :to-equal mock-srt-data)))
          (it "ensures a single newline after the last subtitle."
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-max))
                (delete-backward-char 1)
                (expect (buffer-string) :not :to-equal mock-srt-data)
                (subed-srt-sanitize)
                (expect (buffer-string) :to-equal mock-srt-data)))
          )

(describe "Renumbering"
          (it "ensures consecutive subtitle IDs."
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-min))
                (while (looking-at "^[0-9]$")
                  (replace-match "123"))
                (expect (buffer-string) :not :to-equal mock-srt-data)
                (subed-srt--regenerate-ids)
                (expect (buffer-string) :to-equal mock-srt-data))))

(describe "Sorting"
          (it "ensures subtitles are ordered by start time."
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-min))
                (re-search-forward "01:01")
                (replace-match "12:01")
                (goto-char (point-min))
                (re-search-forward "02:02")
                (replace-match "10:02")
                (goto-char (point-min))
                (re-search-forward "03:03")
                (replace-match "11:03")
                (subed-srt-sort)
                (expect (buffer-string) :to-equal
                        (concat
                         "1\n"
                         "00:10:02,234 --> 00:02:10,345\n"
                         "Bar.\n"
                         "\n"
                         "2\n"
                         "00:11:03,456 --> 00:03:15,567\n"
                         "Baz.\n"
                         "\n"
                         "3\n"
                         "00:12:01,000 --> 00:01:05,123\n"
                         "Foo.\n"))))
          (it "preserves point in the current subtitle."
              (with-temp-buffer
                (insert mock-srt-data)
                (goto-char (point-min))
                (re-search-forward "01:01")
                (replace-match "12:01")
                (search-forward "\n")
                (expect (current-word) :to-equal "Foo")
                (subed-srt-sort)
                (expect (current-word) :to-equal "Foo")))
          )
