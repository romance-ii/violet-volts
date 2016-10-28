all:	bin doc test

deploy:	bin doc test
	./server-push

bin:	tootstest.cgi \
	static/js/mesh.js \
	static/js/lisp.js \
	static/js/social.js \
	static/css/main.css static/css/doc.css

bin/buildapp:
	if which buildapp; \
	then \
		ln -s $$(which buildapp) bin/buildapp; \
	else \
		cl -e '(ql:quickload :buildapp) (eval (read-from-string "(buildapp:build-buildapp \"bin/buildapp\")"))'; \
	fi

tootstest.cgi:	tootstest.cgi.new
	./tootstest.cgi.new check
	mv --backup=t tootstest.cgi.new tootstest.cgi

tootstest.cgi.new:	tootstest.asd bin/buildapp \
		$(shell find . -name \*.lisp \
			-and -not -path \**/.\* \
			-and -not -path src/mesh/\**)
	bin/buildapp --output tootstest.cgi.new \
		--load ~/quicklisp/setup.lisp \
		--asdf-path . \
		--load "src/setup.lisp" \
		--eval '(ql:quickload :tootstest)' \
		--load-system tootstest \
		--entry tootstest:entry

src/lib/jscl/jscl.js:	$(shell find src/lib/jscl -name \*.lisp \
			-and -not -name .\*) \
		src/lib/jscl/src/prelude.js
	cd src/lib/jscl; ./make.sh

# required to make Closure happy
js/undef-require.js:	
	echo 'var require=undefined;' >> $@

js/mesh.cc.js:	js/mesh.js js/undef-require.js src/lib/jscl/jscl.js
	closure-compiler --compilation_level ADVANCED \
		--create_source_map static/js/mesh.cc.js.map \
		--third_party true \
		--js_output_file $@ \
		--js js/undef-require.js \
		--js src/lib/jscl/jscl.js \
		--js js/mesh.js

js/mesh.yug.js: js/mesh.js src/lib/jscl/jscl.js
	uglifyjs src/lib/jscl/jscl.js js/mesh.js \
		--source-map static/$@.map \
		--screw-ie8 \
		-o js/mesh.yug.js \
		-m -c		

js/%.yug.js: src/js/%.js
	uglifyjs $< \
		--source-map static/$@.map \
		--screw-ie8 \
		-o $@ \
		-m -c		

js/%.cc.js: src/js/%.js
	closure-compiler --compilation_level ADVANCED \
		--create_source_map static/$@.map \
		--third_party true \
		--js_output_file $@ \
		--js $<

js/jscl.yug.js: src/lib/jscl/jscl.js
	uglifyjs $< \
		--source-map static/$@.map \
		--screw-ie8 \
		-o $@ \
		-m -c		

js/jscl.cc.js: src/lib/jscl/jscl.js
	closure-compiler --compilation_level ADVANCED \
		--create_source_map static/$@.map \
		--third_party true \
		--js_output_file $@ \
		--js $<

static/js/%.js: js/%.cc.js js/%.yug.js
	echo "Comparing smaller JS isn't working, just using Closure version for now"
	cp $< $@

static/js/jscl.js:	js/jscl.cc.js js/jscl.yug.js
	echo "Comparing smaller JS isn't working, just using Closure version for now"
	cp $< $@

static/js/mesh.js: js/mesh.cc.js js/mesh.yug.js
	echo "Comparing smaller JS isn't working, just using Closure version for now"
	cp $< $@

static/css/%.css:	src/css/%.less $(shell echo src/css/*.less)
	lessc $< | cleancss --skip-import -o $@

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
		--split=node violet-volts.texi

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

