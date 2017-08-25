GCC		?= $(CC)
GCC_PLUGIN_DIR=$(shell $(GCC) -print-file-name=plugin)
GCC_VERSION_STRING=$(shell $(GCC) -dumpversion).0
GCC_MAJOR=$(word 1, $(subst ., ,$(GCC_VERSION_STRING)))
GCC_MINOR=$(word 2, $(subst ., ,$(GCC_VERSION_STRING)))
GCC_MICRO=$(word 3, $(subst ., ,$(GCC_VERSION_STRING)))
TARGET_TRIPLE=$(shell $(GCC) -dumpmachine)

CFLAGS	= -g -Wall -Wextra -fno-rtti -fPIC \
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

all: libfoo hello gcc-rtl gcc-ast-c gcc-ast-cxx

libfoo:
	$(CC) -o foo.o -c $(CFLAGS) $(CPATH) foo.c
	ar rc libfoo.a foo.o
	ranlib libfoo.a
	
gcc-rtl:
	$(CC) -o gcc-rtl.o -c $(CFLAGS) $(CPATH) gcc-rtl.cpp
	$(CC) -o gcc-rtl.so gcc-rtl.o -shared

gcc-ast-c:
	$(CC) -o gcc-ast-c.o -c $(CFLAGS) $(CPATH) gcc-ast-c.cpp
	$(CC) -o gcc-ast-c.so gcc-ast-c.o -shared

gcc-ast-cxx:
	$(CC) -o gcc-ast-cxx.o -c $(CFLAGS) $(CPATH) gcc-ast-cxx.cpp
	$(CC) -o gcc-ast-cxx.so gcc-ast-cxx.o -shared

debug-rtl:
	$(CC) -fplugin=./gcc-rtl.so bare.c -wrapper gdb,--args

debug-ast-c:
	$(CC) -fplugin=./gcc-ast-c.so bare.c -wrapper gdb,--args

debug-ast-cxx:
	$(CC) -fplugin=./gcc-ast-cxx.so -x c++ bare.cxx -wrapper gdb,--args

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
		*.statistics *.dot *.png *-hello hello a.out
