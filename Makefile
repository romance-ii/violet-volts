all:	Romance-II/src/romans/lib/cl-bullet2l/LICENSE
	$(MAKE) -j -k -C Romance-II || :
	$(MAKE) -C Romance-II dist
	$(MAKE) -j -k -C Parenthetical || :
	$(MAKE) -C Parenthetical dist

# Ensure submodules were initialized, to be safe
Romance-II/src/romans/lib/cl-bullet2l/LICENSE:	
	git submodule init
	git submodule sync
	git submodule update
	git submodule foreach git submodule init
	git submodule foreach git submodule sync
	git submodule foreach git submodule update
	cd Romance-II; ./Build

