.PHONY: minimal full clean

minimal:
	# NOTE: you don't need to build the library first. Just "with" the project file in your project and choose the appropriate linking type.
	# However, this stand-alone build process is useful (for me, the developer) to check that all files compile correctly.
	gprbuild -Pagpl

clean:
	gprclean -r -Pagpl
#	rm -f obj/* libstatic/* libdynamic/*
#	make -C hungarian clean
