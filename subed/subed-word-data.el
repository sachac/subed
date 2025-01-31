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
;; .srv2 or WhisperX JSON and tries to match the timing data with the remaining text in
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

(defcustom subed-word-data-score-faces '((0.8 . compilation-info)
                               (0.4 . compilation-warning)
                               (0 . compilation-error))
  "Alist of score thresholds and faces to use."
  :type '(alist :key-type float :value-type face))

(defface subed-word-data-face '((((class color) (background light))
                  :foreground "darkgreen")
                 (((class color) (background dark))
                  :foreground "lightgreen"))
  "Face used for words with word data available.")

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
                                  (cons 'end (min (+ (string-to-number (alist-get 't (xml-node-attributes element)))
                                                     (string-to-number (alist-get 'd (xml-node-attributes element))))
                                                  last-start))
                                  (cons 'text
                                         (replace-regexp-in-string "&#39;" "'"
                                                                   (car (xml-node-children element)))
                                         ))))
                   (setq last-start (alist-get 'start rec))
                   rec))
               text-elements))))

(defun subed-word-data--extract-words-from-youtube-vtt (file &optional from-string)
  "Extract the timing from FILE which is a VTT from YouTube.
Return a list of ((start . ?), (end . ?) (text . ?)).
If FROM-STRING is non-nil, treat FILE as the data itself."
  (with-temp-buffer
    (subed-vtt-mode)
    (if from-string
        (insert file)
      (insert-file-contents file))
    (let ((list (subed-subtitle-list))
          results
          s
          start
          stop
          i)
      (dolist (sub list)
        (when (string-match "<c>" (elt sub 3))
          (setq s (elt sub 3))
          (setq i 0)
          (setq start (elt sub 1))
          (while (and (< i (length s))
                      (string-match "\\(.+?\\)<\\([0-9]+:[0-9]+:[0-9]+\\.[0-9]+\\)>" s i))
            (setq stop (1- (save-match-data (subed-timestamp-to-msecs (match-string 2 s)))))
            (push `((text . ,(save-match-data
                               (string-trim (replace-regexp-in-string "</?c>" "" (match-string 1 s)))))
                    (start . ,start)
                    (end . ,stop))
                  results)
            (setq i (match-end 0)
                  start (1+ stop)))
          (if (and (< i (length s))
                   (not (string= "" (string-trim (substring s i)))))
              (push `((text . ,(string-trim
                              (save-match-data (replace-regexp-in-string "</?c>" "" (substring s i)))))
                      (start . ,start)
                      (end . ,(elt sub 2)))
                    results))))
      (nreverse results))))

(defun subed-word-data--extract-words-from-whisperx-json (file &optional from-string)
  "Extract the timing from FILE in WhisperX's JSON format.
Return a list of ((start . ?), (end . ?) (text . ?) (score . ?)).
If FROM-STRING is non-nil, treat FILE as the data itself."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (data (if from-string
                   (json-read-from-string file)
                 (json-read-file file)))
         (base (seq-mapcat
								(lambda (segment)
									(seq-map (lambda (info)
														 (let-alist info
															 `((start . ,(and .start (* 1000 .start)))
                                 (end . ,(and .end (* 1000 .end)))
																 (text . ,(identity .word))
                                 (score . ,(identity .score)))))
													 (alist-get 'words segment)))
								(alist-get 'segments data)))
         last-end
         current)
		;; numbers at the end of a sentence sometimes don't end up with times
		;; so we need to fix them
    (while current
			(unless (alist-get 'start (car current)) ; start
				(set-cdr (assoc 1 'start (car current)) (1+ last-end)))
			(unless (alist-get 'end (car current)) ; start
				(set-cdr (assoc 1 'end (car current)) (1- (alist-get 'start (cadr current)))))
			(setq
			 last-end (alist-get 'end (car current))
			 current (cdr current)))
    base))

(defun subed-word-data--load (data)
  "Load word-level timing from DATA.
Supports WhisperX JSON, YouTube VTT, and Youtube SRV2 files."
  (when data
    (setq-local subed-word-data--cache data)
    (add-hook 'subed-split-subtitle-timestamp-functions #'subed-word-data-split-at-word-timestamp -5 t)
    (subed-word-data-refresh-text-properties)
    data))

;;;###autoload
(defun subed-word-data-load-from-file (file)
  "Load word-level timing from FILE.
Supports WhisperX JSON, YouTube VTT, and Youtube SRV2 files."
  (interactive (list (read-file-name "JSON, VTT, or srv2: "
                                     nil
                                     nil
                                     nil
                                     nil
                                     (lambda (f)
                                       (or (file-directory-p f)
                                           (string-match
                                            "\\.\\(json\\|srv2\\|vtt\\)\\'"
                                            f))))))
  (subed-word-data--load
   (pcase (file-name-extension file)
     ("json" (subed-word-data--extract-words-from-whisperx-json file))
     ("srv2" (subed-word-data--extract-words-from-srv2 (xml-parse-file file)))
     ("vtt" (subed-word-data--extract-words-from-youtube-vtt file)))))

(defun subed-word-data-load-from-string (string)
  "Load word-level timing from STRING.
For now, only JSON or SRV2 files are supported."
  (subed-word-data--load (cond
           ((string-match "^{" string)
            (subed-word-data--extract-words-from-whisperx-json string t))
           ((string-match "^WEBVTT" string)
            (subed-word-data--extract-words-from-youtube-vtt string t))
           (t
            (subed-word-data--extract-words-from-srv2 string)))))

(defvar subed-word-data-extensions '(".en.srv2" ".srv2" ".json" ".vtt") "Extensions to search for word data.")

;;;###autoload
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
      (when (and file (subed-word-data-load-from-file file))
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

(defvar subed-word-data-compare-function 'subed-word-data-compare-normalized-string=
  "Function to use to compare.")

(defun subed-word-data-compare (word1 word2)
  "Use the `subed-word-data-compare' function to compare WORD1 and WORD2.
Return non-nil if they are the same after normalization."
  (funcall subed-word-data-compare-function word1 word2))

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
                                  (and (or (not (alist-get 'end o)) (<= (alist-get 'end o) end))
                                       (or (not (alist-get 'start o)) (>= (alist-get 'start o) start))
                                       (not (string-match "^\n*$" (alist-get 'text o)))))
                                subed-word-data--cache))))
           (offset 0)
           (done (null remaining-words))
           candidate)
      (while (not done)
        (setq candidate (elt words (+ (1- (length remaining-words)) offset)))
        (cond
         ((and candidate (subed-word-data-compare
                          (car remaining-words)
                          (alist-get 'text candidate)))
          (setq done t))
         ((> offset (length words)) (setq done t))
         ((> offset 0) (setq offset (- offset)))
         (t (setq offset (1+ (- offset))))))
      candidate)))

(defun subed-word-data-split-at-word-timestamp ()
  "Return the starting timestamp if the word is found."
  (cond
   ((get-text-property (point) 'subed-word-start)
    (- time subed-subtitle-spacing))
   (subed-word-data--cache
    (let ((time (assoc-default 'start (subed-word-data--look-up-word))))
      (when time (- time subed-subtitle-spacing))))))

(defun subed-word-data-subtitle-entries ()
  "Return the entries that start and end within the current subtitle."
  (let ((start (subed-subtitle-msecs-start))
        (stop (+ (subed-subtitle-msecs-stop) subed-subtitle-spacing)))
    (seq-filter
     (lambda (o)
       (and (<= (or (alist-get 'end o) most-positive-fixnum) stop)
            (>= (or (alist-get 'start o) 0) start)
            (not (string-match "^\n*$" (alist-get 'text o)))))
     subed-word-data--cache)))

(defvar subed-word-data-threshold 5
  "Number of words to consider for matching.")
(defun subed-word-data-refresh-text-properties-for-subtitle ()
  "Refresh the text properties for the current subtitle."
  (interactive)
  (remove-text-properties (subed-jump-to-subtitle-text) (subed-jump-to-subtitle-end)
                          '(subed-word-data-start subed-word-data-end font-lock-face))
  (let* ((text-start (progn (subed-jump-to-subtitle-text) (point)))
         pos
         (word-data (reverse (subed-word-data-subtitle-entries)))
         candidate
         cand-count)
    (subed-jump-to-subtitle-end)
    (while (> (point) text-start)
      ;; Work our way backwards, matching against remaining words
      (setq pos (point))
      (backward-word)
      (let ((try-list word-data)
            candidate)
        (setq candidate (car try-list) cand-count 0)
        (setq try-list (cdr try-list))
        (while (and candidate
                    (< cand-count subed-word-data-threshold)
                    (not (subed-word-data-compare (buffer-substring (point) pos)
                                                  (alist-get 'text candidate))))
          (setq candidate (car try-list) cand-count (1+ cand-count))
          (when (> cand-count subed-word-data-threshold)
            (setq candidate nil))
          (setq try-list (cdr try-list)))
        (when (and candidate (subed-word-data-compare (buffer-substring (point) pos)
                                       (alist-get 'text candidate)))
          (subed-word-data--add-word-properties (point) pos candidate)
          (setq word-data try-list))))))

(defun subed-word-data-refresh-region (beg end)
  "Refresh text properties in region."
  (when subed-word-data--cache
    (subed-for-each-subtitle beg end nil
      (subed-word-data-refresh-text-properties-for-subtitle))))

(defsubst subed-word-data--candidate-face (candidate)
  "Return the face to use for CANDIDATE."
  (if (and (alist-get 'score candidate)
           subed-word-data-score-faces)
      (cdr (seq-find (lambda (threshold) (>= (alist-get 'score candidate) (car threshold)))
                     subed-word-data-score-faces))
    'subed-word-data-face))

(defsubst subed-word-data--add-word-properties (start end candidate)
  "Add properties from START to END for CANDIDATE."
  (let ((face (subed-word-data--candidate-face candidate)))
    (add-text-properties start end
                         (list 'subed-word-data-start
                               (assoc-default 'start candidate)
                               'subed-word-data-end
                               (assoc-default 'end candidate)
                               'subed-word-data-score
                               (assoc-default 'score candidate)
                               'font-lock-face face))
    (add-face-text-property start end face)))

(defun subed-word-data-refresh-text-properties ()
  "Add word data properties and face when available."
  (interactive)
  (save-excursion
    (remove-text-properties (point-min) (point-max) '(subed-word-data-start subed-word-data-end font-lock-face))
    (when subed-word-data--cache
      (goto-char (point-min))
      (unless (subed-jump-to-subtitle-id) (subed-forward-subtitle-id))
      (while (not (eobp))
        (let* ((text-start (progn (subed-jump-to-subtitle-text) (point)))
               pos
               (word-data (reverse (subed-word-data-subtitle-entries)))
               candidate)
          (subed-jump-to-subtitle-end)
          (while (> (point) text-start)
            ;; Work our way backwards, matching against remaining words
            (setq pos (point))
            (backward-word)
            (let ((try-list word-data)
                  candidate)
              (setq candidate (car try-list))
              (setq try-list (cdr try-list))
              (while (and candidate
                          (not (subed-word-data-compare (buffer-substring (point) pos)
                                         (alist-get 'text candidate))))
                (setq candidate (car try-list))
                (setq try-list (cdr try-list)))
              (when (and candidate (subed-word-data-compare (buffer-substring (point) pos)
                                             (alist-get 'text candidate)))
                ( subed-word-data--add-word-properties (point) pos candidate)
                (setq word-data try-list)))))
        (or (subed-forward-subtitle-id)
            (goto-char (point-max)))))))

(defun subed-word-data-pause-msecs ()
  "Return the number of milliseconds between this word and the previous word.
Requires the text properties to be set."
  (let ((current (get-text-property (point) 'subed-word-data-start)))
    (save-excursion
     (skip-syntax-backward "w")
     (backward-word)
     (when (get-text-property (point) 'subed-word-data-end)
       (- current (get-text-property (point) 'subed-word-data-end))))))

(defun subed-word-data-jump-to-longest-pause-in-current-subtitle ()
  "Jump to the word after the longest pause in the current subtitle.
Requires the text properties to be set."
  (interactive)
  (let ((start (or (subed-jump-to-subtitle-text) (point)))
        (end (or (subed-jump-to-subtitle-end) (point)))
        pos last-start-time pause (max-pause 0) max-pos)
    (backward-word)
    (setq max-pos (point))
    (while (> (point) start)
      (setq pos (point) last-start-time (get-text-property (point) 'subed-word-data-start))
      (backward-word)
      (if (get-text-property (point) 'subed-word-data-end)
          (progn
            (setq pause (and last-start-time (- last-start-time
                                                (get-text-property (point) 'subed-word-data-end))))
            (when (and pause (> pause max-pause))
              (setq max-pos pos
                    max-pause pause)))
        (setq last-start-time nil)))
    (goto-char max-pos)))

(defun subed-word-data-find-minimum-distance (from-n to-n distance-fn &optional short-circuit low-threshold)
  "Return (number distance) that minimizes DISTANCE-FN.
Check the range FROM (inclusive) to TO (exclusive).
If SHORT-CIRCUIT is specified, call that function with i as the argument and stop when it returns true.
If LOW-THRESHOLD is specified, stop when the distance is less than or equal to that number."
  (setq low-threshold (or low-threshold 0))
  (catch 'found
    (let* (min-distance
				   current-distance
           i)
      (setq i from-n)
      (while (< i to-n)
        (setq current-distance (funcall distance-fn i))
        (when (or (null min-distance) (< current-distance (cdr min-distance)))
          (setq min-distance (cons i current-distance))
          (when (<= current-distance low-threshold)
            (throw 'found min-distance)))
        (when (and short-circuit (funcall short-circuit i))
          (throw 'found min-distance))
        (setq i (1+ i)))
      min-distance)))

(defun subed-word-data-find-approximate-match (phrase list-of-words &optional short-circuit)
  "Match PHRASE against the beginning of LIST-OF-WORDS.
LIST-OF-WORDS is a list of strings or a list of alists that have 'text.
If SHORT-CIRCUIT is non-nil, use it as a regexp that short-circuits recognition and stops there.
Return (distance . list of words) that minimizes the string distance from PHRASE.
distance is expressed as a ratio of number of edits / maximum length of phrase or words.
"
  (let ((min-distance
         (subed-word-data-find-minimum-distance
          1
          (+ (length (split-string phrase " ")) 8)
          (lambda (num-words)
            (let ((cand (mapconcat
                         (lambda (o)
                           (if (stringp o) o (alist-get 'text o)))
                         (seq-take list-of-words num-words)
                         " ")))
              (/
               (* 1.0
                  (string-distance phrase cand))
               (max (length phrase)
                    (length cand)))))
          (if (and short-circuit
                   (not (string-match short-circuit phrase)))
              (lambda (num-words)
                (string-match
                 short-circuit
                 (mapconcat
                  (lambda (o)
                    (if (stringp o) o (alist-get 'text o)))
                  (seq-take list-of-words num-words)
                  " ")))))))
    (cons (cdr min-distance) (seq-take list-of-words (car min-distance)))))

;; (subed-word-data-find-approximate-match "Go into the room." (split-string "Go in to the room. There you will" " "))
;; (subed-word-data-find-approximate-match "The quick brown fox jumps over the lazy dog" (split-string "The quick, oops, the quick brown fox jumps over the lazy dog and goes all sorts of places" " ") "\\<oops\\>")
;; (subed-word-data-find-approximate-match "I already talk pretty quickly," (split-string "I already talk pretty quickly. Oops. I already talk pretty quickly, so I'm not going" " ") "\\<oops\\>")
(defun subed-word-data-fix-subtitle-timing (beg end)
  "Sets subtitle starts and stops based on the word data.
Assumes words haven't been edited."
  (interactive (list (if (region-active-p) (min (point) (mark)))
                     (if (region-active-p) (max (point) (mark)))))
  (unless subed-word-data--cache
    (call-interactively #'subed-word-data-load-from-file))
  (setq beg (or beg (point-min)))
  (setq end (if end (save-excursion
                      (goto-char end)
                      (subed-jump-to-subtitle-end)
                      (point))
              (point-max)))
  (goto-char beg)
  (if (subed-subtitle-msecs-start)
      (subed-jump-to-subtitle-text)
    (subed-forward-subtitle-text))
  (let* ((start-ms (save-excursion
                     (goto-char beg)
                     (or (subed-subtitle-msecs-start)
                         (progn
                           (subed-forward-subtitle-time-start)
                           (subed-subtitle-msecs-start)))))
         (data (seq-drop-while
                (lambda (o)
                  (< (or (alist-get 'start o) 0)
                     start-ms))
                subed-word-data--cache))
				 candidate)
    (while (and (not (> (point) end)) data)
      (setq current-sub (replace-regexp-in-string "\n" " " (subed-subtitle-text)))
      (let ((candidate (subed-word-data-find-approximate-match current-sub data)))
        (subed-set-subtitle-time-start
         (alist-get
          'start
          (seq-find (lambda (o) (alist-get 'start o))
                    (cdr candidate))))
        (subed-set-subtitle-time-stop
         (alist-get
          'end
          (seq-find (lambda (o) (alist-get 'end o))
                    (reverse (cdr candidate)))))
        (subed-word-data-refresh-text-properties-for-subtitle)
        (setq data (seq-drop data (length (cdr candidate)))))
      (unless (subed-forward-subtitle-text)
        (goto-char (point-max))))))

(defun subed-word-data-move-untimed-words-from-previous ()
	"Move untimed words from previous subtitle to current one."
	(interactive)
	(save-excursion
		(subed-backward-subtitle-end)
		(text-property-search-backward 'subed-word-data-end)
		(goto-char (next-single-property-change (point) 'subed-word-data-end))
		(let* ((start (point))
					 (text (buffer-substring start (subed-jump-to-subtitle-end))))
			(delete-region start (point))
			(subed-forward-subtitle-text)
			(insert text " "))))

(defvar subed-word-data-script-difference-threshold 0.2
	"*If string difference is above this threshold, include original text as a comment.")

(defvar subed-word-data-oops-regexp "\\<oops\\>"
	"*Regular expression matching the signal used after a false start.")

(defun subed-word-data-combine-script-and-transcript (phrases bag-of-words &optional oops-regexp keep-transcript-words)
	"Use PHRASES to split the words in BAG-OF-WORDS.
If OOPS-REGEXP is non-nil, use that as the regular expression that signals a false start.
If KEEP-TRANSCRIPT-WORDS is non-nil, don't correct transcript words.
Return a list of subtitles and comments."
	(let* ((phrase-length (length phrases))
				 (phrase-cursor 0)
				 (case-fold-search t)
				 lookback
				 min-candidate
				 result)
		(while (and (< phrase-cursor phrase-length)
								bag-of-words)
			(when (and oops-regexp (string-match oops-regexp (car bag-of-words)))
				;; discard that word and figure out where we're restarting
				(setf (elt (cdr (car result)) 3)
              (concat (elt (cdr (car result)) 3) " " (car bag-of-words)))
				(setf (elt (cdr (car result)) 4)
							(string-trim
							 (concat (or (elt (cdr (car result)) 4) "") "\n#+SKIP")))
				(setq bag-of-words (cdr bag-of-words)))
      (setq phrase-cursor
						(- phrase-cursor
							 (car (subed-word-data-find-minimum-distance
										 0 (1+ (min phrase-cursor 4))
										 (lambda (i)
											 (if (< (- phrase-cursor i) 0)
													 most-positive-fixnum
												 (car (subed-word-data-find-approximate-match
															 (elt phrases (- phrase-cursor i))
															 bag-of-words
															 oops-regexp))))))))
      ;; mark the previous ones as oopses also
      (dolist (o result)
        (when (>= (car o) phrase-cursor)
          (unless (string-match "#\\+SKIP" (or (elt (cdr o) 4) ""))
            (setf (elt (cdr o) 4)
                  (string-trim
                   (concat
                    (or (elt (cdr o) 4) "")
                    "\n"
                    "#+SKIP"))))))
			(setq candidate
						(subed-word-data-find-approximate-match
						 (elt phrases phrase-cursor)
						 bag-of-words
						 oops-regexp))
			(setq result
						(cons
             (cons
              phrase-cursor
						  (if (and oops-regexp
											 (string-match oops-regexp (string-join (cdr candidate) " "))
											 (not (string-match oops-regexp (elt phrases phrase-cursor))))
								  (list nil 0 0 (string-join (cdr candidate) " ") "#+SKIP")
							  (setq phrase-cursor (1+ phrase-cursor))
							  (list nil 0 0
 										  (if (or keep-transcript-words
                              (> (car candidate) subed-word-data-script-difference-threshold))
												  (string-join (cdr candidate) " ")
											  (elt phrases (1- phrase-cursor)))
                      (cond
											 ((and keep-transcript-words (> (car candidate) 0))
                        (concat "#+SCRIPT: " (elt phrases (1- phrase-cursor)) "\n"
                                "#+DISTANCE: " (format "%.2f" (car candidate))))
											 ((= (car candidate) 0) nil)
                       ((> (car candidate) subed-word-data-script-difference-threshold)
											  (concat "#+SCRIPT: " (elt phrases (1- phrase-cursor)) "\n"
                                "#+DISTANCE: " (format "%.2f" (car candidate))))
                       ((< (car candidate) subed-word-data-script-difference-threshold)
											  (concat "#+TRANSCRIPT: " (string-join (cdr candidate) " ") "\n"
                                "#+DISTANCE: " (format "%.2f" (car candidate))))
                       ))))
						 result))
			;; found a good match, move to the next phrase
			(setq bag-of-words (seq-drop bag-of-words (length (cdr candidate)))))
		(mapcar 'cdr (reverse result))))

(defun subed-word-data-use-script-file (script-file output-file &optional oops-regexp keep-transcript-words)
	"Use the info from SCRIPT-FILE to correct the current transcript.
Write OUTPUT-FILE so that it uses the words and phrasing from SCRIPT-FILE,
but includes any extra phrases from TRANSCRIPT-FILE (such as oopses).
If OOPS-REGEXP is non-nil, use that as the regular expression that signals a false start.
If KEEP-TRANSCRIPT-WORDS is non-nil, don't correct current transcript words.

When called with `\\[universal-argument]', don't correct current transcript words."
	(interactive (list
								(read-file-name "Script: ")
								(read-file-name "Output file: ")
								subed-word-data-oops-regexp
								current-prefix-arg))
	(let* ((phrases
					(if (string= "vtt" (file-name-extension script-file))
							(mapcar (lambda (o) (elt o 3)) (subed-parse-file script-file))
						(with-temp-buffer
							(insert-file script-file)
							(split-string (string-trim (buffer-string)) "\n"))))
				 (bag-of-words
					(split-string
					 (if (derived-mode-p 'subed-mode)
               (mapconcat (lambda (o) (elt o 3)) (subed-subtitle-list) " ")
						 (string-trim (buffer-string)))
					 "[ \n]+"))
				 (result (subed-word-data-combine-script-and-transcript phrases bag-of-words oops-regexp)))
		(subed-create-file
		 output-file
		 result
		 t)
		(find-file output-file)))

(with-eval-after-load 'subed
  (add-hook 'subed-region-adjusted-hook #'subed-word-data-refresh-region))


(provide 'subed-word-data)
;;; subed-word-data.el ends here
