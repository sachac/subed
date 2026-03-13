;;; subed-align.el --- use forced alignment tools like aeneas  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Sacha Chua

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

;; This has some extra support for using Aeneas for forced alignment
;; in order to get VTT or SRT timestamps from a plain text file and an
;; audio file.
;;
;; You will also need aeneas and its dependencies: https://github.com/readbeyond/aeneas
;;
;;; Code:

(require 'subed)

(defvar subed-align-command '("python3" "-m" "aeneas.tools.execute_task")
  "Command to run aeneas.")

(defvar subed-align-language "eng"
  "Language code.")

(defvar subed-align-options nil
  "Other options to include in the aeneas invocation.
Ex: task_adjust_boundary_nonspeech_min=0.500|task_adjust_boundary_nonspeech_string=REMOVE
will remove silence and other non-speech spans.")

(defvar subed-align-mfa-conda-env "~/miniconda/env/aligner"
  "Set this to the path to your Conda environment.")
(defvar subed-align-mfa-command '("mfa" "align")
  "Command to run the Montreal Forced Aligner.")
(defvar subed-align-mfa-dictionary "french_mfa")
(defvar subed-align-mfa-acoustic-model "french_mfa")


;;;###autoload
(defun subed-align-region (audio-file beg end)
  "Align just the given section."
  (interactive
   (list
    (or
     (subed-media-file)
     (subed-guess-media-file subed-audio-extensions)
     (read-file-name "Audio file: "))
    (if (region-active-p) (min (point) (mark)) (point-min))
    (if (region-active-p) (max (point) (mark)) (point-max))))
  (let* ((format (cond
									((derived-mode-p 'subed-vtt-mode) "VTT")
									((derived-mode-p 'subed-srt-mode) "SRT")))
         (input-mode major-mode)
         (input-subtitles (subed-subtitle-list beg end))
         (temp-input-file
          (make-temp-file "subed-align" nil ".txt"
                          (mapconcat (lambda (o) (elt o 3)) input-subtitles "\n\n")))
				 (temp-file
          (concat (make-temp-name "subed-align")
                  "."
                  (if (buffer-file-name)
											(file-name-extension (buffer-file-name))
										(downcase format))))
				 (ignore-before (save-excursion
													(goto-char beg)
													(unless (subed-subtitle-msecs-start)
														(subed-forward-subtitle-text))
													(/ (subed-subtitle-msecs-start) 1000.0)))
				 (process-length (save-excursion
													 (goto-char end)
													 (- (/ (subed-subtitle-msecs-stop) 1000.0)
															ignore-before)))
         results)
    (unwind-protect
        (progn
          (apply
           #'call-process
           (car subed-align-command)
           nil
           (get-buffer-create "*subed-aeneas*")
           t
           (append (cdr subed-align-command)
                   (list (expand-file-name audio-file)
                         temp-input-file
                         (format "is_audio_file_head_length=%.3f|is_audio_file_process_length=%.3f|task_language=%s|os_task_file_format=%s|is_text_type=%s%s"
                                 ignore-before
                                 process-length
                                 subed-align-language
                                 (downcase format)
                                 "subtitles"
                                 (if subed-align-options (concat "|" subed-align-options) ""))
                         temp-file)))
          ;; parse the subtitles from the resulting output
          (setq results (subed-parse-file temp-file))
          (save-excursion
            (subed-for-each-subtitle beg end nil
              (when-let* ((current (pop results)))
                (subed-set-subtitle-time-start (elt current 1))
                (subed-set-subtitle-time-stop (elt current 2)))))
          (run-hook-with-args 'subed-region-adjusted-hook beg end))
      (delete-file temp-input-file)
      (delete-file temp-file))))

;;;###autoload
(defun subed-align (audio-file text-file format)
  "Align AUDIO-FILE with TEXT-FILE to get timestamps in FORMAT.
Return the new filename."
  (interactive
   (list
    (or
     (subed-media-file)
     (subed-guess-media-file subed-audio-extensions)
     (read-file-name "Audio file: "))
    (buffer-file-name)
    (completing-read "Format: "
                     '("AUD" "CSV" "EAF" "JSON" "SMIL" "SRT"
                       "SSV" "SUB" "TEXTGRID" "TSV" "TTML" "TXT" "VTT" "XML"))))
  (let ((new-file
         (and (buffer-file-name)
              (expand-file-name
               (concat (file-name-sans-extension (buffer-file-name)) "." (downcase format)))))
        temp-file subtitles)
    (when (or (null (file-exists-p new-file))
              (yes-or-no-p (format "%s exists. Overwrite? " (file-name-nondirectory new-file))))
      (when (derived-mode-p 'subed-mode)
        (setq subtitles (subed-subtitle-list))
        (setq temp-file (make-temp-file "subed-align" nil ".txt"))
        (with-temp-file temp-file
          (insert (mapconcat (lambda (o) (elt o 3)) subtitles "\n\n"))))
      (apply
       #'call-process
       (car subed-align-command)
       nil
       (get-buffer-create "*subed-aeneas*")
       t
       (append (cdr subed-align-command)
               (list (expand-file-name audio-file)
                     (or temp-file (expand-file-name text-file))
                     (format "task_language=%s|os_task_file_format=%s|is_text_type=%s%s"
                             subed-align-language
                             (downcase format)
                             (if temp-file
                                 "subtitles"
                               "plain")
                             (if subed-align-options (concat "|" subed-align-options) ""))
                     new-file)))
      (when temp-file (delete-file temp-file))
      (with-temp-file new-file
        (insert-file-contents new-file)
        (subed-guess-format new-file)
        (when (derived-mode-p 'subed-mode)
          (subed-trim-overlaps))
        (when (derived-mode-p 'subed-vtt-mode)
          (goto-char (point-min))
          (flush-lines "^[0-9]+$")
          ;; reinsert comments
          (subed-align-reinsert-comments subtitles)))
      (when (called-interactively-p 'any)
        (find-file new-file))
      new-file)))

;; TODO Scope it to the region
(defun subed-align-aeneas-set-word-data (audio-file)
  "Align AUDIO-FILE with TEXT-FILE to get timestamps in FORMAT.
Store the word data in `subed-word-data--cache' for use by subed-word-data.
This uses the Aeneas forced aligner."
  (interactive
   (list
    (or
     (subed-media-file)
     (subed-guess-media-file subed-audio-extensions)
     (read-file-name "Audio file: "))))
  (unless (derived-mode-p 'subed-mode)
    (error "Must be in `subed-mode' buffer."))
  (let ((temp-input (make-temp-file "subed-align-input" nil ".txt"))
        (temp-output (make-temp-file "subed-align-output" nil ".txt"))
        (json-object-type 'alist)
        (json-array-type 'list)
        data)
    (write-region
     (if (derived-mode-p 'subed-mode)
         (mapconcat
          (lambda (o) (elt o 3)) (subed-subtitle-list)
          "\n\n")
       (buffer-string))
     nil temp-input)
    (apply
     #'call-process
     (car subed-align-command)
     nil
     (get-buffer-create "*subed-aeneas*")
     t
     (append (cdr subed-align-command)
             (list (expand-file-name audio-file)
                   temp-input
                   (format "task_language=%s|is_text_type=mplain|os_task_file_levels=3|os_task_file_format=json%s"
                           subed-align-language
                           (if subed-align-options (concat "|" subed-align-options) ""))
                   temp-output)))
    (setq data (mapcar
                (lambda (o)
                  (let-alist o
                    `((start . ,(* 1000 (string-to-number .begin)))
                      (end . ,(* 1000 (string-to-number .end)))
                      (text . ,(string-join .lines " ")))))
                (alist-get 'fragments
                           (with-temp-buffer
                             (insert-file-contents temp-output)
                             (goto-char (point-min))
                             (json-read)))))
    (subed-word-data--load data)
    (delete-file temp-input)
    (delete-file temp-output)
    data))

(defun subed-align-mfa-process-environment ()
  "Return process-environment with conda env activated."
  (let* ((env-bin (expand-file-name "bin" subed-align-mfa-conda-env))
         (env-lib (expand-file-name "lib" subed-align-mfa-conda-env))
         (path-var (concat env-bin ":" (getenv "PATH")))
         (ld-library-path (concat env-lib ":" (or (getenv "LD_LIBRARY_PATH") ""))))
    (list (concat "PATH=" path-var)
          (concat "LD_LIBRARY_PATH=" ld-library-path)
          (concat "CONDA_PREFIX=" (expand-file-name subed-align-mfa-conda-env))
          (concat "CONDA_DEFAULT_ENV=aligner"))))

(defvar subed-align-mfa-beam-size nil)
(defvar subed-align-mfa-retry-beam nil)

;;;###autoload
(defun subed-align-mfa-set-word-data (audio-file &optional beg end callback)
  "Set the word data using Montreal Forced Aligner."
  (interactive
   (list
    (or
     (subed-media-file)
     (subed-guess-media-file subed-audio-extensions)
     (read-file-name "Audio file: "))
    (when (region-active-p) (region-beginning))
    (when (region-active-p) (region-end))))
  ;; MFA expects audio and text
  (let* ((temp-input (make-temp-file "subed-align-mfa-input" t))
         (temp-output (make-temp-file "subed-align-mfa-output" t))
         (input-wav (expand-file-name "input.wav" temp-input))
         (start-ms (and beg (save-excursion (goto-char beg) (subed-subtitle-msecs-start))))
         (stop-ms (and end (save-excursion (goto-char end) (subed-subtitle-msecs-stop)))))
    ;; Set up input.wav
    (if (string= (downcase (file-name-extension audio-file)) "wav")
        (copy-file audio-file input-wav)
      (apply #'call-process
             subed-ffmpeg-executable
             nil (get-buffer-create "*mfa*") nil
             (append
              (when start-ms
                (list "-ss"
                      (number-to-string (/ start-ms 1000.0))))
              (list "-i" audio-file)
              (when stop-ms
                (list "-t"
                      (number-to-string (/ (- stop-ms start-ms) 1000.0))))
              (list
               "-ar"
               "16000"
               input-wav))))
    ;; Set up input.txt
    (write-region
     (subed-align-mfa-prepare-text (if (derived-mode-p 'subed-mode)
                            (mapconcat
                             (lambda (o) (elt o 3)) (subed-subtitle-list beg end)
                             "\n\n")
                          (buffer-string)))
     nil (expand-file-name "input.txt" temp-input))
    (let* ((process-environment (append
                                 (and subed-align-mfa-conda-env (subed-align-mfa-process-environment))
                                 process-environment))
           (default-directory temporary-file-directory)
           (subtitle-buffer (current-buffer))
           (command (append
                     (list
                      (if subed-align-mfa-conda-env
                          (expand-file-name
                           (car subed-align-mfa-command)
                           (expand-file-name
                            "bin"
                            subed-align-mfa-conda-env))
                        (car subed-align-mfa-command)))
                     (cdr subed-align-mfa-command)
                     (list
                      (file-name-base temp-input)
                      subed-align-mfa-dictionary
                      subed-align-mfa-acoustic-model
                      (file-name-base temp-output))
                     (when subed-align-mfa-beam-size
                       (list
                        "--beam"
                        (number-to-string subed-align-mfa-beam-size)))
                     (when subed-align-mfa-retry-beam
                       (list
                        "--retry_beam"
                        (number-to-string subed-align-mfa-retry-beam))))))
      (message "Starting alignment...")
      (make-process
       :name "mfa"
       :buffer (get-buffer-create "*mfa*")
       :command command
       :sentinel
       (lambda (process event)
         (when (string-match "finished" event)
           (when (file-exists-p (expand-file-name "input.TextGrid" temp-output))
             (when (and (buffer-file-name) (not beg))
               (copy-file (expand-file-name "input.TextGrid" temp-output)
                          (concat (file-name-sans-extension (buffer-file-name)) ".TextGrid")
                          t))
             (with-current-buffer subtitle-buffer
               (if beg
                   (let ((data (subed-word-data--extract-words-from-textgrid
                                (expand-file-name "input.TextGrid"
                                                  temp-output))))
                     (setq subed-word-data--cache
                           (append
                            (seq-filter
                             (lambda (o) (< (alist-get 'start o) start-ms))
                             subed-word-data--cache)
                            (mapcar (lambda (o)
                                      (setf (alist-get 'start o)
                                            (+ (alist-get 'start o) start-ms))
                                      (setf (alist-get 'end o)
                                            (+ (alist-get 'end o) start-ms))
                                      o)
                                    data)
                            (seq-filter
                             (lambda (o) (>= (alist-get 'start o) stop-ms))
                             subed-word-data--cache)))
                     (subed-word-data-refresh-region beg end))
                 (subed-word-data-load-from-file (expand-file-name "input.TextGrid"
                                                                   temp-output)))
               (when callback
                 (funcall callback))))
           (delete-directory temp-input t)
           (delete-directory temp-output t)))))))

(defun subed-align-mfa-prepare-text (text)
  (replace-regexp-in-string "[()]" "" text))

(defun subed-align-reinsert-comments (subtitles)
  "Reinsert the comments from SUBTITLES.
Assume that the subtitles are still in the same sequence."
  (goto-char (point-min))
  (mapc
   (lambda (sub)
     (subed-forward-subtitle-time-start)
     (when (elt sub 4)
       (subed-set-subtitle-comment (elt sub 4))))
   subtitles))

(provide 'subed-align)
;;; subed-align.el ends here
