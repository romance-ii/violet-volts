#!gmake

# MAIN TARGETS: all (dist); clean; push (to dev server)
all: dist

clean:
	-rm -rf build/*
	-rm -rf $(DISTVDIR)
	-find . -name \*~ -exec rm {} \;
	git submodule foreach git clean -f

include ../program.mak
include ../versions.mak
include ../clients.mak
include ../license.mak
include ../push-to.mak

########################################################################

push:	../dist/$(PROJECT)-$(WEBVERSION).tar.xz
	scp $< $(HOST):$(DIR)
	ssh $(HOST) tar -C $(DIR) -jxvf $(DIR)/$(PROJECT)-$(WEBVERSION).tar.xz

ALL_JS=	build/01-preamble.js	\
	build/store.js \
	build/Modernizr.js \
	build/js-lib/glge.js \
	build/gl-utils.js	\
	build/network.js	\
	build/asset-loader.js	\
	build/string-utils.js	\
	build/ui-help.js	\

ALL_CSS= src/css/base.css	\
	 src/css/text.css	\
	 src/css/game-ui.css

DISTVDIR=../dist/$(PROJECT)-$(WEBVERSION)/
DISTASSETVDIR=../dist/$(PROJECT)-assets-$(ASSETSVERSION)

dist: ../dist/$(PROJECT)-$(WEBVERSION).tar.xz ../dist/$(PROJECT)-assets-$(ASSETSVERSION).tar.xz

../dist/$(PROJECT)-$(WEBVERSION).tar.xz: \
	$(DISTVDIR)/$(PROJECT).js \
	$(DISTVDIR)/$(PROJECT).css \
	$(DISTVDIR)/$(PROJECT).html \
	$(DISTVDIR)
	cp -a src/static/v $(DISTVDIR)
	cp -a src/static/help $(DISTVDIR)
	tar Jcvf $@ --strip-components=1 $(DISTVDIR)

$(DISTVDIR):
	mkdir -p $@

$(DISTVDIR)/$(PROJECT).css: \
	$(DISTVDIR) \
	build/$(PROJECT)-$(WEBVERSION)-min.yahoo.css
	cp $(shell tools/bin/smaller $^) $@

$(DISTVDIR)/$(PROJECT).js: \
	$(DISTVDIR) \
	build/$(PROJECT)-$(WEBVERSION)-min.yahoo.js \
	build/$(PROJECT)-$(WEBVERSION)-min.ugly.js \
	build/$(PROJECT)-$(WEBVERSION)-min.google.js
	cp $(shell tools/bin/smaller $^) $@
	cp build/$(PROJECT)-$(WEBVERSION)-all.js $(DISTVDIR)/debug.js

build/01-preamble.js: src/ps/01-preamble.js
	cp $^ $@

build/%.js:	src/static/%.js
	cp $^ $@

build/js-lib/glge.js:	src/js-lib/glge/glge-compiled.js build/js-lib/glge-prefix.txt 
	mkdir -p build/js-lib/
	cat build/js-lib/glge-prefix.txt $^ > $@

build/js-lib/glge-prefix.txt:	Makefile
	mkdir -p build/js-lib
	echo -n "/** @COPYRIGHT: " > $@

src/js-lib/glge/glge-compiled.js:	$(shell find src/js-lib/glge/src -type f)
	$(MAKE) -C src/js-lib/glge

build/%.js: src/ps/%.lisp \
	src/ps/00-macros.lisp \
	src/ps/01-preamble.js
# Brute-force: updates all JavaScript files every time
	sbcl --disable-debugger --load tools/parenscript-compile.lisp

build/$(PROJECT)-$(WEBVERSION)-all.js: \
	$(ALL_JS)
	cat $^ > $@

build/$(PROJECT)-$(WEBVERSION)-all.css: $(ALL_CSS)
	cat $^ > $@

build/$(PROJECT)-$(WEBVERSION)-min.yahoo.js: \
		build/$(PROJECT)-$(WEBVERSION)-all.js \
		tools/yuicompressor.jar
	java -jar tools/yuicompressor.jar \
		--type js --charset utf-8 -o $@ $<

tools/yuicompressor.jar: $(shell find src/tools/yuicompressor/src -name \*java)
	cd src/tools/yuicompressor/; ant
	cp src/tools/yuicompressor/build/yuicompressor-*.jar $@

build/$(PROJECT)-$(WEBVERSION)-min.ugly.js: build/$(PROJECT)-$(WEBVERSION)-all.js
	uglifyjs $< -o $@ --source-map $@.source-map

build/$(PROJECT)-$(WEBVERSION)-min.google.js: build/$(PROJECT)-$(WEBVERSION)-all.js
	java -jar tools/closure-compiler/compiler.jar \
		--compilation_level ADVANCED_OPTIMIZATIONS \
		--jscomp_off=internetExplorerChecks \
		--create_source_map $(DISTVDIR)/debug.js.map \
		--js=$< --js_output_file=$@

build/$(PROJECT)-$(WEBVERSION)-min.yahoo.css: \
	build/$(PROJECT)-$(WEBVERSION)-all.css tools/yuicompressor.jar
	java -jar tools/yuicompressor.jar \
		--type css --charset utf-8 --verbose -o $@ $<

build/$(PROJECT)-$(WEBVERSION).html: src/$(PROJECT).html
	sed -s 's,$$VERSION,$(WEBVERSION),' < $< > $@

build/$(PROJECT)-$(WEBVERSION)-min.google.html: \
	build/$(PROJECT)-$(WEBVERSION).html
	java -jar tools/htmlcompressor-1.5.3.jar -t html $< -o $@


$(DISTVDIR)/$(PROJECT).html: \
	$(DISTVDIR) \
	build/$(PROJECT)-$(WEBVERSION)-min.google.html
	cp $(shell tools/bin/smaller $^) $@

../assets.mak:	tools/bin/make-assets $(SWFSOURCESDIR)
	tools/bin/make-assets $(SWFSOURCESDIR) > ../assets.mak

include ../assets.mak

../dist/$(PROJECT)-assets-$(ASSETSVERSION).tar.xz: \
	../assets.mak \
	$(VERSIONEDASSETS)
	tar jcvf $@ -C build/$(PROJECT)-assets *

build/$(PROJECT)-assets/%: build/art/%
	mkdir -p `dirname $@`
	ln $< $@

%.svg:	%.swf
	tools/bin/swf2svg $< > $@ 2| tee $@.err

%.raw.dae:	%.svg
	tools/bin/svg2collada $< > $@ 2| tee $@.err

build/art/%.svg: $(SWFSOURCESDIR)/%.swf
	tools/bin/swf2svg $< > $@ 2| tee $@.err

build/art/%.svg: $(SWFSOURCESDIR)/%.fla
	tools/bin/fla2svg $< > $@ 2| tee $@.err

%.dae:	%.raw.dae
	tools/bin/minify-xml $< $@

%.ttf:	%.ttf
	cp $< $@

%.mp3:	%.mp3
	cp $< $@

%.ogg:	%.mp3
	FIXME

%.txt:	%.org
	emacs --batch -q -nw \
		--load doc/batch-export.el \
		--visit $< \
		--funcall org-ascii-export-to-ascii

%.html:	%.org
	emacs --batch -q -nw \
		--load doc/batch-export.el \
		--visit $< \
		--funcall org-html-export-to-html

%.tex:	%.org
	emacs --batch -q -nw \
		--load doc/batch-export.el \
		--visit $< \
		--funcall org-latex-export-to-latex

%.pdf:	%.tex
	( cd $(shell dirname $< ) ; pdflatex $(shell basename $< ) ) < /dev/null

%.info:  %.texi
	texi2any --info --force $< -o $@
                
%.html:  %.texi 
	texi2any --html --split=section --css-ref=romance2.css --force $< -o $@
                        
%.pdf:  %.texi
	texi2any --pdf --force $< -o $@
                                
%.ps:   %.texi
	texi2any --ps --force $< -o $@
                                        
%.txt:  %.texi
	texi2any --plaintext --force $< -o $@

doc:	\
	doc/devel/Developers-Guide.html \
	doc/devel/Developers-Guide.txt \
	doc/devel/Developers-Guide.pdf

