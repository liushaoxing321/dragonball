#include <stdio.h>

#include "foo.h"

int main(int argc, char *argv[]) 
{
    printf("Hello World\n");
    if (foo())
        printf("bar\n");
    return 0;
}
