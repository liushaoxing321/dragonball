CC		= gcc
CFLAGS	= -Wall -fPIC -fdump-tree-all
CPATH	=
LIBPATH	=
LIBPATH += -L.
LDLIBS	=
LDLIBS  += -lfoo-$(CC)

all: libfoo hello

libfoo:
	$(CC) -o $(CC)-foo.o -c $(CFLAGS) $(CPATH) foo.c
	ar rc libfoo-$(CC).a $(CC)-foo.o
	ranlib libfoo-$(CC).a

hello:
	$(CC) -o $(CC)-hello.o -c $(CFLAGS) $(CPATH) hello.c
	$(CC) -o $(CC)-hello $(CC)-hello.o $(LIBPATH) $(LDLIBS)

clean: 
	rm -rf *.o *.a *.tu *.class *.original *.gimple *.omplower *.lower *.eh \
		*.cfg *.ompexp *.fixup_cfg1 *.ssa *.fixup_cfg3 *.inline_param1 \
		*.einline *.profile_estimate *.release_ssa *.inline_param2 \
		*.fixup_cfg4 *.oaccdevlow *.veclower *.cplxlower0 *.optimized \
		*.statistics *-hello
