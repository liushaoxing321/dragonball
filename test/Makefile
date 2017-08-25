CC          = arm-linux-gnu-gcc
CXX         = arm-linux-gnu-g++
CFLAGS      = -g -Wall
CXXFLAGS    = -g -Wall -fPIC
CPATH       = -I/usr/arm-linux-gnu/include
CXXPATH     = -I/usr/arm-linux-gnu/include
LIBPATH     = -Wl,-rpath-link=/usr/arm-linux-gnu/lib -L/usr/arm-linux-gnu/lib
LIB         = -nostdlib /usr/arm-linux-gnu/lib/crt1.o /usr/arm-linux-gnu/lib/crti.o /usr/arm-linux-gnu/lib/crtn.o -lc -lgcc -ldl
LIBSO       = -nostdlib -lc -shared

all: hello

hello:
	$(CC) -o hello.o -c $(CFLAGS) $(CPATH) hello.c
	$(CC) -o hello hello.o $(LIBPATH) $(LIB)

clean: 
	rm -rf *.o hello
