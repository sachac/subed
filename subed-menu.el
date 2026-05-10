;;;;;; subed-menu.el --- Menus for subed  -*- mode: Emacs-Lisp; lexical-binding: t ; -*-
;;;
;;;

(lambda () "
* Authors:
** Mohsen Banan --  http://mohsen.1.banan.byname.net/contact
** Email: emacs at mohsen.1.banan.byname.net
* Copying And Usage: This is a perpetual Libre-Halaal polyexistential.
")

(require 'easymenu)

;;
;; (subed:menu|define)
;;
(defun subed:menu|define ()
  "Returns subed:menu."

    (easy-menu-define
      subed:menu
      subed-mode-map
      "subed menu"
      `("subed"
	:help "Sub-Titles Editing Facilities"
	:active t
	:visible t
	"---"
	 [
	  "MPV::Play From Specified File"
	  (call-interactively 'subed-mpv-play-from-file)
	  :help "open a media file manually with C-c C-v (subed-mpv-play-from-file)"
	  :active t
	  :visible t
	  ]
         ["MPV:: Jump to the current subtitle (M-j)"
	  (subed-mpv-jump-to-current-subtitle)
	  :help "Jump to the current subtitle in the MPV player"
	  :active t
	  :visible t
	  ]
	 ["MPV:: Pause and resume playback  (M-SPC)"
	  (subed-mpv-toggle-pause)
	  :help "Start or stop playback"
	  :active t
	  :visible t
	  ]
        ["MPV:: Toggle looping over the current subtitle"
	  (subed-toggle-loop-over-current-subtitle)
	  :help "Loop over the current subtitle in mpv (C-c C-l)."
	  :active t
	  :visible t
	  ]
	"----"
	 [
	  "Show the waveform"
	  (call-interactively 'subed-waveform-minor-mode)
	  :help "extracted from the media file using ffmpeg with the start/stop positions of the current subtitle."
	  :active t
	  :visible t
	  ]
	 [
	  "Align text with audio file (using  aeneas)"
	  (call-interactively 'subed-align)
	  :help "Use aeneas to align your text or subtitles with an audio file in order to get timestamps."
	  :active t
	  :visible t
	  ]
	"-----"
	))

    (easy-menu-add-item subed:menu nil
			(subed:menu:mpv|define) "----")

    (easy-menu-add-item subed:menu nil
			(subed:menu:help|define) "-----")

    'subed:menu
    )

;; (subed:menu:mpv|define)
(defun subed:menu:mpv|define ()
  "Generic Blee Help Menu"
  (easy-menu-define
    subed:menu:mpv
    nil
    "MPV::  Sub-Menu --- MPV Integration"
    `("MPV::  Sub-Menu --- MPV Integration"
      :help "More MPV Integration Features"
      ["Copy the current playback position as start"
       (subed-copy-player-pos-to-start-time)
       :help "Replace current subtitle’s start time with current playback time."
       ]
      ["Copy the current playback position as stop"
       (subed-copy-player-pos-to-stop-time)
       :help "Replace current subtitle’s stop time with current playback time."
       ]
      ))
    'subed:menu:mpv
    )

;; (subed:menu:help|define)
(defun subed:menu:help|define ()
  "Generic Blee Help Menu"
  (let (
	;;($thisFuncName (compile-time-function-name))
        ($thisFuncName "subed:menu:help|define")
	)
    (easy-menu-define
      subed:menu:help
      nil
      "Help Menu For subed"
      `("Subed Help"
	:help "Help For This Menu"
	["Visit subed Help Panel"
	 (browse-url "https://github.com/sachac/subed")
	 t
	 ]
	"---"
	[(format "Visit %s" ,$thisFuncName)
	 (find-function (intern ,$thisFuncName))
	 t
	 ]
	)
      )
    'subed:menu:help
    ))

(subed:menu|define)

(provide 'subed-menu)
