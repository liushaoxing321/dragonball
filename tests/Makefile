CC		= gcc
CFLAGS	= -Wall -fPIC -fdump-tree-original-raw #-fdump-tree-all
CPATH	=
LIBPATH	=
LIBPATH += -L.
LDLIBS	=
LDLIBS  += -lfoo-$(CC)
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
