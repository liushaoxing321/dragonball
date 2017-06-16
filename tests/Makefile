CC		= gcc
CFLAGS	= -Wall -fPIC
ifeq ($(CC),clang)
	CFLAGS += -Xclang -ast-view
else
	CFLAGS += -fdump-tree-original-raw #-fdump-tree-all
endif
CPATH	=
LIBPATH	=
LDLIBS  =
ifeq ($(CC),clang)
	LDLIBS += libfoo-$(CC).a
else
	LIBPATH += -L.
	LDLIBS += -lfoo-$(CC)
endif
PRE	    = hello

all: libfoo hello

libfoo:
	$(CC) -o $(CC)-foo.o -c $(CFLAGS) $(CPATH) foo.c
	ar rc libfoo-$(CC).a $(CC)-foo.o
	ranlib libfoo-$(CC).a

hello:
	$(CC) -o $(CC)-hello.o -c $(CFLAGS) $(CPATH) hello.c
	$(CC) -o $(CC)-hello $(CC)-hello.o $(LIBPATH) $(LDLIBS)

# http://digitocero.com/en/blog/exporting-and-visualizing-gccs-abstract-syntax-tree-ast
treeviz:
	./pre.awk $(PRE).c.003t.original | ./treeviz.awk > $(PRE).dot
	dot -Tpng $(PRE).dot -o $(PRE).png

clean: 
	rm -rf *.o *.a *.tu *.class *.original *.gimple *.omplower *.lower *.eh \
		*.cfg *.ompexp *.fixup_cfg1 *.ssa *.fixup_cfg3 *.inline_param1 \
		*.einline *.profile_estimate *.release_ssa *.inline_param2 \
		*.fixup_cfg4 *.oaccdevlow *.veclower *.cplxlower0 *.optimized \
		*.statistics *.dot *.png *-hello
