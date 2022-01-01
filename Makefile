# SPDX-FileCopyrightText: 2019-2020 The subed Authors
#
# SPDX-License-Identifier: GPL-3.0-or-later

clean:
	find . -name "*.elc" -delete

test:
	emacs -batch -f package-initialize -L . -f buttercup-run-discover
	emacs --no-init-file -f package-initialize --batch \
		  --eval "(require 'package-lint)" \
	      --file package-lint-batch-and-exit \
	      ./subed/subed.el
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-config.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-mpv.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-srt.el\")"

autoloads:
	emacs --quick --batch --eval "(progn (setq generated-autoload-file (expand-file-name \"subed-autoloads.el\" \"subed\") backup-inhibited t) \
	(update-directory-autoloads \"./subed\"))"

test-compile:
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-config.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-common.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-mpv.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-srt.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-vtt.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-ass.el\"))"
	emacs --quick --batch --eval "(progn (add-to-list 'load-path (expand-file-name \"subed\" default-directory)) \
	                                     (byte-compile-file \"subed/subed-debug.el\"))"
	make clean

test-emacs:
	emacs -Q -L ./subed --eval "(require 'subed)"
