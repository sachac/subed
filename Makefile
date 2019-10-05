clean:
	find . -name "*.elc" -delete

test:
	buttercup
	emacs --no-init-file --batch \
		  --eval "(require 'package-lint)" \
	      --file package-lint-batch-and-exit \
	      ./subed/subed.el
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-config.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-mpv.el\")"
	emacs --quick --batch --eval "(checkdoc-file \"subed/subed-srt.el\")"

test-compile:
	emacs -Q -batch --eval "(byte-compile-file \"subed/subed.el\")"
	emacs -Q -batch --eval "(byte-compile-file \"subed/subed-config.el\")"
	emacs -Q -batch --eval "(byte-compile-file \"subed/subed-mpv.el\")"
	emacs -Q -batch --eval "(byte-compile-file \"subed/subed-srt.el\")"
	make clean
