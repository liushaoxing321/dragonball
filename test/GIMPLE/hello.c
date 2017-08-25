#include <stdio.h>

#include "foo.h"

int main(int argc, char *argv[]) 
{
    char *s = "Leslie";
    int n;
    printf("%s: Hello World\n", s);
    if (n = foo())
        printf("%d\n", n);
    return 0;
}
