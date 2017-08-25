#include "foo.h"

#include <stdio.h>

int foo() 
{
    printf("DEBUG: %s, %s, line %d\n", __FILE__, __func__, __LINE__);
    return 0;
}
