#include <stdio.h>

void foo() {
  printf("DEBUG: %s, line %d: %s\n", __FILE__, __LINE__, __func__);
}

int main(int argc, char *argv[]) {
  int n = 1;
  char *s = "Leslie Zhai";
  printf("%s: Hello World %d\n", s, n);
  return 0;
}
