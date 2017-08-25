#include <stdio.h>

#define PI 3.1415926

static const int num = 83;

static int foo();
static int bar(int fd);

static int foo() {
  return 0;
}

static int bar(int fd) {
  if (foo() != 0)
    return -1;
  return 0;
}

int main(int argc, char *argv[]) {
  int n;
  n = bar(0);
  printf("%d\n", num);
  return 0;
}
