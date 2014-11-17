# GNU Make rules for fetching and byte-compiling various elisp files.

EMACS=emacs -q --no-site-file

.PHONY: compile package elpa

package: *.el
	@ver=`grep -o "Version: .*" hl-anything.el | cut -c 10-`; \
	tar cvf hl-anything-$$ver.tar `git ls-files '*.el' | xargs`

elpa: *.el
	@ver=`grep -o "Version: .*" hl-anything.el | cut -c 10-`; \
	dir=hl-anything-$$ver; \
	mkdir -p "$$dir"; \
	cp `git ls-files '*.el' | xargs` hl-anything-$$ver; \
	echo "(define-package \"hl-anything\" \"$$version\" \
	\"Modular in-buffer completion framework\")" \
	> "$$dir"/hl-anything-pkg.el; \
	tar cvf hl-anything-$$ver.tar "$$dir"

clean:
	@rm -rf hl-anything-*/ hl-anything-*.tar hl-anything-*.tar.bz2 *.elc

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile hl-anything.el

