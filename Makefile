clean:
	find . -name "*.elc" -delete

test:
	buttercup
	emacs -q -batch \
		  --eval "(require 'package-lint)" \
	      -f package-lint-batch-and-exit \
	      ./subed/subed.el
	emacs -Q -batch --eval "(checkdoc-file \"subed/subed.el\")"
	emacs -Q -batch --eval "(checkdoc-file \"subed/subed-config.el\")"
	emacs -Q -batch --eval "(checkdoc-file \"subed/subed-mpv.el\")"
	emacs -Q -batch --eval "(checkdoc-file \"subed/subed-srt.el\")"
