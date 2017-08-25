GCC		?= $(CC)
GCC_PLUGIN_DIR=$(shell $(GCC) -print-file-name=plugin)
GCC_VERSION_STRING=$(shell $(GCC) -dumpversion).0
GCC_MAJOR=$(word 1, $(subst ., ,$(GCC_VERSION_STRING)))
GCC_MINOR=$(word 2, $(subst ., ,$(GCC_VERSION_STRING)))
GCC_MICRO=$(word 3, $(subst ., ,$(GCC_VERSION_STRING)))
TARGET_TRIPLE=$(shell $(GCC) -dumpmachine)

CFLAGS	= -Wall -Wextra -fno-rtti -fPIC \
          -DTARGET_TRIPLE=\"$(TARGET_TRIPLE)\" \
	      -DGCC_MAJOR=$(GCC_MAJOR) -DGCC_MINOR=$(GCC_MINOR) \
	      -DGCC_MICRO=$(GCC_MICRO) \
	      -isystem$(GCC_PLUGIN_DIR)/include
ifeq ($(CC),clang)
	CFLAGS += -Xclang -ast-view
else
	CFLAGS += -fdump-tree-original-raw #-fdump-tree-all
endif
CPATH	=
LIBPATH	=
LDLIBS  =
ifeq ($(CC),clang)
	LDLIBS += libfoo.a
else
	LIBPATH += -L.
	LDLIBS += -lfoo
endif
PRE	    = hello

all: libfoo hello plugin

libfoo:
	$(CC) -o foo.o -c $(CFLAGS) $(CPATH) foo.c
	ar rc libfoo.a foo.o
	ranlib libfoo.a
	
plugin:
	$(CC) -o GCCPlugin.o -c $(CFLAGS) $(CPATH) GCCPlugin.cpp
	$(CC) -o GCCPlugin.so GCCPlugin.o -shared

debug-plugin:
	$(CC) -fplugin=./GCCPlugin.so bare.c -wrapper gdb,--args

hello:
	$(CC) -o hello.o -c $(CFLAGS) $(CPATH) hello.c
	$(CC) -o hello hello.o $(LIBPATH) $(LDLIBS)

# http://digitocero.com/en/blog/exporting-and-visualizing-gccs-abstract-syntax-tree-ast
treeviz:
	./pre.awk $(PRE).c.003t.original | ./treeviz.awk > $(PRE).dot
	dot -Tpng $(PRE).dot -o $(PRE).png

clean: 
	rm -rf *.o *.a *.so *.tu *.class *.original *.gimple *.omplower *.lower *.eh \
		*.cfg *.ompexp *.fixup_cfg1 *.ssa *.fixup_cfg3 *.inline_param1 \
		*.einline *.profile_estimate *.release_ssa *.inline_param2 \
		*.fixup_cfg4 *.oaccdevlow *.veclower *.cplxlower0 *.optimized \
		*.statistics *.dot *.png *-hello
