#include <stdio.h>

int login(const char *Name, const char *Pwd) {
  printf("DEBUG: %s, line %d\n", __FILE__, __LINE__);
  if (!Name || !Pwd)
    return -1;

  return 0;
}

int logout(int Id) {
  printf("DEBUG: %s, line %d: ID %d\n", __FILE__, __LINE__, Id);
  return 0;
}

int main(int argc, char *argv[]) {
  printf("Hello world\n");
  login(NULL, NULL);
  logout(-1);
  return 0;
}
