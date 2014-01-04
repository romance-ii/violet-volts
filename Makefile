all:	Romance-II/src/romans/lib/cl-bullet2l/LICENSE
	$(MAKE) -C Romance-II

Romance-II/src/romans/lib/cl-bullet2l/LICENSE:	
	git submodule init
	git submodule sync
	git submodule update
	cd Romance-II; ./Build

