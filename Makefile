all:	bin doc test

bin:	tootstest.cgi static/js/mesh.js

tootstest.cgi:	tootstest.asd $(shell find . -name \*.lisp -and -not -path \**/.\*)
	buildapp --output tootstest.cgi.new \
		--load ~/quicklisp/setup.lisp \
		--asdf-path . \
		--load "src/setup.lisp" \
		--eval '(ql:quickload :tootstest)' \
		--load-system tootstest \
		--entry tootstest:entry
	mv --backup=t tootstest.cgi.new tootstest.cgi

src/lib/jscl/jscl.js:	$(shell find src/lib/jscl -name \*.lisp -and -not -name .\*)
	cd src/lib/jscl; ./make.sh

static/js/mesh.js:	js/mesh.js
	closure-compiler --compilation_level SIMPLE \
		--create_source_map $<.map \
		--formatting PRETTY_PRINT \
		--third_party true \
		--warning_level VERBOSE \
		--js_output_file $< \
		--js $@

static/css/main.css:	src/css/main.less
	lessc src/css/main.less | cleancss -o static/css/main.css

js/mesh.js:	src/lib/jscl/jscl.js src/bootstrap-tootstest.lisp \
		$(find src/mesh -name \*.lisp -and -not -path \**/.\*)
	cd src/lib/jscl; sbcl --load jscl.lisp \
		--load ../../../src/bootstrap-tootstest.lisp \
		--eval '(jscl::bootstrap-mesh)' --eval '(quit)'

test:	bin
	cd src/lib/jscl; ./run-tests.sh
	./tootstest.cgi test

doc/violet-volts.pdf:	doc/violet-volts.texi

doc/violet-volts.html.tar.gz:	doc/violet-volts.html.tar
	gzip -9 -c < $< > $@

doc/violet-volts.html.tar.z:	doc/violet-volts.html.tar
	compress -9 -c < $< > $@

doc/violet-volts.html.tar.bz2:	doc/violet-volts.html.tar
	bzip2 -9 -c < $< > $@

doc/violet-volts.html.tar.xz:	doc/violet-volts.html.tar
	xz -9 -c < $< > $@

doc/violet-volts.html.tar:	doc/violet-volts.html.d/violet-volts.html\
		doc/violet-volts.texi
	cd doc; tar cf violet-volts.html.tar violet-volts.html.d

doc/violet-volts.html.zip:	doc/violet-volts.html.d/violet-volts.html\
		doc/violet-volts.texi
	cd doc; zip -9 violet-volts.html.zip violet-volts.html.d

doc/violet-volts.html.d/violet-volts.html:	doc/violet-volts.texi
	cd doc; makeinfo -o violet-volts.html.d/ \
		--html --css-include=src/static/css/doc.css \
		--split=node ../violet-volts.texi

doc/violet-volts.ps:	doc/violet-volts.texi
	cd doc; makeinfo --ps -o violet-volts.ps violet-volts.texi

doc/violet-volts.pdf:	doc/violet-volts.texi
	cd doc; makeinfo --pdf -o violet-volts.pdf violet-volts.texi

doc/violet-volts.txt:	doc/violet-volts.texi
	cd doc; makeinfo --plaintext -o violet-volts.txt violet-volts.texi

doc/violet-volts.info:	doc/violet-volts.texi
	cd doc; makeinfo -o violet-volts.info violet-volts.texi

doc:	doc/violet-volts.pdf doc/violet-volts.info doc/violet-volts.txt \
	doc/violet-volts.html.tar.gz

doc/violet-volts.texi:	tootstest.cgi
	./tootstest.cgi write-docs

