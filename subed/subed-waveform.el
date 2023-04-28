;;; subed-waveform.el --- display waveforms in subed buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sacha Chua, Marcin Borkowski

;; Author: Sacha Chua <sacha@sachachua.com>, Marcin Borkowski <mbork@mbork.pl>
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

;; This file contains variables, options, functions and commands for
;; displaying a waveform along with the current subtitle.  To turn the
;; displaying on, say `M-x subed-toggle-show-waveform'.  Press `C-x
;; C-=' and `C-x C--' to make the amplitude of the displayed waveform
;; larger and smaller.  If the waveform becomes corrupted or is out of
;; sync (this may happen for example when you modify the start/stop
;; timestamp(s) using Subed mode commands but then undo your changes),
;; press `C-c |' to redisplay it.  Say `M-x customize-group
;; subed-waveform' to configure.  Click the waveform with
;; `mouse-1'/`mouse-2' to set the start/stop timestamp.  Consider
;; setting `subed-loop-seconds-before' and `subed-loop-seconds-after'
;; to positive values for better experience.
;;
;;; Code:

(defcustom subed-waveform-ffmpeg-executable "ffmpeg"
  "Path to the FFMPEG executable used for generating waveforms."
  :type 'file
  :group 'subed-waveform)

(defcustom subed-waveform-ffmpeg-filter-args ":colors=white,negate"
  "Additional arguments for the showwavespic filter.
The background is black by default and the foreground reddish.
To change the foreground color, use something like
\":colors=white\".  To invert the colors (for example to obtain
black on white), use \":colors=white,negate\"."
  :type '(choice
          (string :tag "Extra arguments to include")
          (function :tag "Function to call with the width and height"))
  :group 'subed-waveform)

(defcustom subed-waveform-bar-params
  '((:start . (:id "start" :stroke-color "darkgreen" :stroke-width "3"))
    (:stop . (:id "stop" :stroke-color "darkred" :stroke-width "3"))
    (:current . (:id "current" :stroke-color "orange" :stroke-width "2")))
  "An alist of bar types and parameters.
The keys in it are `:start', `:stop' and `:current'.  The values are
SVG parameters of the displayed bars.  Every bar must have a unique
`:id' parameter."
  :type '(alist :key-type (choice (const :tag "Start" :start)
				  (const :tag "Stop" :stop)
				  (const :tag "Current" :current))
		:value-type (plist :key-type symbol :value-type string))
  :group 'subed-waveform)

(defcustom subed-waveform-volume
  2.0
  "A multiplier of the volume.
Set it to more than 1.0 if the voice is too quiet and the moments
when people speak are indistinguishable from silence."
  :type 'number
  :group 'subed-waveform)

(defcustom subed-waveform-timestamp-resolution
  20
  "Resolution of the timestamps.
When the user clicks on the waveform, the timestamp set will be
rounded to the nearest multiple of this number."
  :type 'integer
  :group 'subed-waveform)

(defun subed-show-waveform-p ()
  "Whether waveform is displayed for the current subtitle."
  (member #'subed-waveform-put-svg subed-subtitle-motion-hook))

(defun subed-enable-show-waveform ()
  "Enable showing the waveform for the current subtitle."
  (interactive)
  (add-hook 'subed-subtitle-motion-hook #'subed-waveform-put-svg)
  (add-hook 'subed-subtitle-time-adjusted-hook #'subed--waveform-update-bars)
  (add-hook 'subed-mpv-playback-position-hook #'subed--waveform-update-current-bar)
  (subed-waveform-put-svg))

(defun subed-disable-show-waveform ()
  "Enable showing the waveform for the current subtitle."
  (interactive)
  (remove-hook 'subed-subtitle-motion-hook #'subed-waveform-put-svg)
  (remove-hook 'subed-subtitle-time-adjusted-hook #'subed--waveform-update-bars)
  (remove-hook 'subed-mpv-playback-position-hook #'subed--waveform-update-current-bar)
  (delete-overlay subed--waveform-image-overlay))

(defun subed-toggle-show-waveform ()
  "Toggle showing the waveform for the current subtitle."
  (interactive)
  (if (subed-show-waveform-p)
      (subed-enable-show-waveform)
    (subed-disable-show-waveform)))

(defconst subed-waveform-volume-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-=") #'subed-waveform-volume-increase)
    (define-key map (kbd "C--") #'subed-waveform-volume-decrease)
    map)
  "A keymap for manipulating waveform \"volume\".")

(defun subed-waveform-volume-increase (amount)
  "Increase `subed-waveform-value' by AMOUNT/2."
  (interactive "p")
  (setq subed-waveform-volume (+ subed-waveform-volume (/ amount 2.0)))
  (message "Waveform volume multiplier is now set to %s" subed-waveform-volume)
  (subed-waveform-put-svg)
  (set-transient-map subed-waveform-volume-map))

(defun subed-waveform-volume-decrease (amount)
  "Increase `subed-waveform-value' by AMOUNT/2."
  (interactive "p")
  (setq subed-waveform-volume (max 1.0
				   (- subed-waveform-volume (/ amount 2.0))))
  (message "Waveform volume multiplier is now set to %s" subed-waveform-volume)
  (subed-waveform-put-svg)
  (set-transient-map subed-waveform-volume-map))

(defun subed--waveform-from-file (filename from to width height)
  "Returns a string representing the image data in PNG format.
FILENAME is the input file, FROM and TO are time positions, WIDTH
and HEIGHT are dimensions in pixels."
  (let* ((args
          (append
	   (list "-accurate_seek"
		 "-ss" (format "%s" from)
		 "-to" (format "%s" to))
           (list "-i" filename)
           (list
            "-loglevel"
            "0"
            "-filter_complex"
            (format "volume=%s,showwavespic=s=%dx%d%s"
		    subed-waveform-volume
                    width height
		    subed-waveform-ffmpeg-filter-args)
            "-frames:v" "1"
	    "-c:v" "png"
            "-f" "image2" "-"))))
    (with-temp-buffer
      (apply 'call-process subed-waveform-ffmpeg-executable nil t nil args)
      (encode-coding-string (buffer-string) 'binary))))

(defvar subed--waveform-start nil
  "Timestamp (in milliseconds) of the start of the waveform.")
(defvar subed--waveform-stop nil
  "Timestamp (in milliseconds) of the stop of the waveform.")

(defun subed--waveform-msecs-to-ffmpeg (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ (floor msecs) 1000))
          "." (format "%03d" (mod (floor msecs) 1000))))

(defun subed--waveform-position-to-percent (pos start stop)
  "Return a percentage of POS relative to START/STOP."
  (format "%s%%" (/ (* 100 (- pos start)) (- stop start))))

(defvar-local subed--waveform-pos nil
  "Buffer position of the image with the current waveform.")

(defvar-local subed--waveform-svg nil
  "SVG image with the current waveform.")

(defvar-local subed--waveform-image-overlay nil
  "The overlay with the waveform.")

;; TODO: determine height/width only once
(defun subed--waveform-set-svg ()
  "Create the svg for the waveform of the current subtitle.
Set `subed--waveform-svg' and `subed--waveform-pos'."
  (save-excursion
    (let ((start (subed-subtitle-msecs-start))
	  (stop (subed-subtitle-msecs-stop)))
      (setq subed--waveform-start
	    (max 0 (- start (* 1000 subed-loop-seconds-before))))
      (setq subed--waveform-stop
	    (+ stop (* 1000 subed-loop-seconds-after)))
      (let* ((width (string-pixel-width (make-string fill-column ?*)))
	     (height (save-excursion
		       ;; don't count the current waveform towards the
		       ;; line height
		       (forward-line -1)
		       (* 2 (line-pixel-height))))
	     (image (subed--waveform-from-file
		     (or subed-mpv-media-file (error "No media file found"))
		     (subed--waveform-msecs-to-ffmpeg subed--waveform-start)
		     (subed--waveform-msecs-to-ffmpeg subed--waveform-stop)
		     width
		     height)))
	(setq subed--waveform-svg (svg-create width height))
	(svg-embed subed--waveform-svg image "image/png" t
		   :x 0 :y 0
 		   :width "100%" :height "100%"
		   :preserveAspectRatio "none")
	(subed--waveform-update-bars (subed-subtitle-msecs-start))))))

(defun subed--waveform-move-bar (bar-type position)
  "Update `subed--waveform-svg', moving bar BAR-TYPE to POSITION.
BAR-TYPE should be a symbol, one of :start, :stop, :current.
POSITION should be a percentage as a string."
  (svg-remove subed--waveform-svg
	      (plist-get (alist-get bar-type subed-waveform-bar-params)
			 ":id"
			 #'string=))
  (apply #'svg-line
	 subed--waveform-svg position 0 position "100%"
	 (alist-get bar-type subed-waveform-bar-params)))

(defun subed--waveform-update-bars (subed-subtitle-msecs-start)
  "Update the bars in `subed--waveform-svg'.
Recompute the waveform if the start bar is too far to the left or
the stop bar is too far to the right."
  (let* ((start subed-subtitle-msecs-start)
	 (stop (subed-subtitle-msecs-stop))
	 (start-pos (subed--waveform-position-to-percent
		     start
		     subed--waveform-start
		     subed--waveform-stop))
	 (stop-pos (subed--waveform-position-to-percent
		    stop
		    subed--waveform-start
		    subed--waveform-stop)))
    (when (or (not subed--waveform-svg)
	      (< start subed--waveform-start)
	      (> stop subed--waveform-stop))
      (subed--waveform-set-svg))
    (subed--waveform-move-bar :start start-pos)
    (subed--waveform-move-bar :stop stop-pos))
  (subed--waveform-update-current-bar subed-mpv-playback-position))

(defun subed--waveform-update-current-bar (subed-mpv-playback-position)
  "Update the \"current\" bar in `subed--waveform-svg'.
Assume all necessary variables are already set.  This function is
meant to be as fast as possible so that it can be called many
times per second."
  (subed--waveform-move-bar
   :current
   (subed--waveform-position-to-percent
    subed-mpv-playback-position
    subed--waveform-start
    subed--waveform-stop))
  (subed--waveform-update-overlay-svg))

(defconst subed-waveform-map
  (let ((subed-waveform-map (make-keymap)))
    (define-key subed-waveform-map [mouse-1] #'subed-waveform-set-start)
    (define-key subed-waveform-map [mouse-3] #'subed-waveform-set-stop)
    subed-waveform-map)
  "A keymap for clicking on the waveform.")

(defun subed--waveform-update-overlay-svg ()
  "Update the SVG in the overlay.
Assume `subed--waveform-svg' is already set."
  (overlay-put subed--waveform-image-overlay
	       'before-string
	       (propertize
		" "
		'display (svg-image subed--waveform-svg)
		'pointer 'arrow
		'keymap subed-waveform-map)))

(defun subed-waveform-put-svg ()
  "Put an overlay with the SVG in the current subtitle.
Set the relevant variables if necessary."
  (interactive)
  (setq subed--waveform-pos (subed-jump-to-subtitle-text))
  (if (overlayp subed--waveform-image-overlay)
      (move-overlay subed--waveform-image-overlay
		    subed--waveform-pos subed--waveform-pos)
    (setq subed--waveform-image-overlay
	  (make-overlay subed--waveform-pos subed--waveform-pos))
    (overlay-put subed--waveform-image-overlay
		 'after-string
		 "\n"))
  (subed--waveform-set-svg))

(defun waveform-mouse-event-to-ms (event)
  "Return the millisecond position of EVENT."
  (let* ((x (car (elt (cadr event) 8)))
         (width (car (elt (cadr event) 9))))
    (* subed-waveform-timestamp-resolution
       (round (+ (* (/ (* 1.0 x) width)
		    (- subed--waveform-stop subed--waveform-start))
		 subed--waveform-start)
	      subed-waveform-timestamp-resolution))))

(defun subed-waveform-set-start (event)
  "Set the start timestamp in the place clicked."
  (interactive "e")
  (subed-set-subtitle-time-start (waveform-mouse-event-to-ms event))
  (subed--waveform-update-bars (subed-subtitle-msecs-start)))

(defun subed-waveform-set-stop (event)
  "Set the start timestamp in the place clicked."
  (interactive "e")
  (subed-set-subtitle-time-stop (waveform-mouse-event-to-ms event))
  (subed--waveform-update-bars (subed-subtitle-msecs-start)))

(provide 'subed-waveform)
;;; subed-waveform.el ends here
