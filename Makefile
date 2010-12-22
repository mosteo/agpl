.PHONY: all clean

PARAMS=-Pagpl -XGtkAda_Include_Core=Yes -XHungarian_Include_Test=Yes -XHungarian_Include_Base=Yes -XHungarian_Link=Static_Library -XAgpl_Include_Test=Yes -XAgpl_Include_Psql=Yes -XAgpl_Include_Http=No -XAgpl_Include_Gtk=Yes -XAgpl_Include_Concorde=Yes -XAgpl_Include_Boost=Yes -XAgpl_Link=Static_Library

all:
	# NOTE: you don't need to build the library first. Just "with" the project file in your project and choose the appropriate linking type.
	# However, this stand-alone build process is useful (for me, the developer) to check that all files compile correctly.
	#
	# gprmake builds everything.
	#
	gprmake ${PARAMS}
	gnatmake ${PARAMS}

fix:
	# sometimes depending of previous builds, some C object files are not properly included. Use this to force recompilation of only C sources.
	# This may be a bug in GPL 2007 and previous.
	rm -f obj/*.d

clean:
	gprclean ${PARAMS}
	rm -f obj/* libstatic/* libdynamic/*
	make -C hungarian clean
