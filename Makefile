all: tursas

tursas:
	/usr/local/bin/sbcl --eval "(progn (asdf:operate 'asdf:load-op 'tursas) (save-lisp-and-die :executable t :toplevel #'tursas:main))"

clean:
	/bin/rm /home/zmyrgel/.cache/common-lisp/sbcl-1.0.54.openbsd-bsd-x64/home/zmyrgel/src/lisp/tursas/
