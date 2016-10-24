all:	bin

deploy:	bin
	./server-push

bin:	tootstest.cgi \
	static/js/mesh.js \
	static/js/lisp.js \
	static/js/social.js \
	static/css/main.css static/css/doc.css

tootstest.cgi:	tootstest.asd \
		$(shell find . -name \*.lisp \
			-and -not -path \**/.\* \
			-and -not -path src/mesh/\**)
	buildapp --output tootstest.cgi.new \
		--load ~/quicklisp/setup.lisp \
		--asdf-path . \
		--load "src/setup.lisp" \
		--eval '(ql:quickload :tootstest)' \
		--load-system tootstest \
		--entry tootstest:entry
	mv --backup=t tootstest.cgi.new tootstest.cgi

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
		--source-map static/js/mesh.yug.js.map \
		--screw-ie8 \
		-o js/mesh.yug.js \
		-m -c		

static/js/%.js: src/js/%.js
	uglifyjs $< \
		--source-map $@.map \
		--screw-ie8 \
		-o $@ \
		-m -c		


static/js/mesh.js: js/mesh.cc.js js/mesh.yug.js
	if [ \
	    $$(stat js/mesh.cc.js | grep Size: | \
		cut -d: -f2 | cut -d B -f 1) \
	    -lt \
	    $$(stat js/mesh.yug.js | grep Size: | \
		cut -d: -f2 | cut -d B -f 1) \
	    ]; then \
	  echo "Closure produced tighter code than Uglify"; \
	  cp js/mesh.cc.js static/js/mesh.js; \
	else \
	  echo "Uglify produced tighter code than Closure"; \
	  cp js/mesh.yug.js static/js/mesh.js; \
	fi
	echo "Ignoring both and using raw";
	cat src/lib/jscl/jscl.js js/mesh.js > static/js/mesh.js

static/css/%.css:	src/css/%.less $(shell echo src/css/*.less)
	lessc $< | cleancss -o $@

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

