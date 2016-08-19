all:	tootstest.cgi src/lib/jscl/jscl.js
tootstest.cgi:	tootstest.asd $(shell find . -name \*.lisp -and -not -path '**/lib/**')
	PATH=~/bin:${PATH} ~/bin/buildapp --output tootstest.cgi.new \
		--load ~/quicklisp/setup.lisp \
		--asdf-path . \
		--eval '(ql:quickload :tootstest)' \
		--load-system tootstest \
		--entry tootstest:fastcgi-entry
	mv --backup=t tootstest.cgi.new tootstest.cgi

src/lib/jscl/jscl.js:	$(shell find src/lib/jscl -name \*.lisp -or -name \*.lisp)
	cd src/lib/jscl; ./make.sh

test:	all
	cd src/lib/jscl; ./run-tests.sh
	./tootstest.cgi --test
