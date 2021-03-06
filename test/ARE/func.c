#include <stdio.h>
#include "jni.h"

#define SKEY "6xxzcQMhb4WgKX0EUkwG747K"

JNIEXPORT jstring JNICALL
Java_com_tencent_mm_network_Java2C_getNetworkServerIp(JNIEnv *env,
                                                      jclass thiz) {
  return (*env)->NewStringUTF(env, "140.207.135.104");
}

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
  const char *Key = "Vml5Z0pFZGk9UHg2a2dPY0loZW49S3cxN3dVQUFBPT0";
  unsigned const char buf[] = { 0x12, 0x34, 0x56, 0x78 };
  printf("Hello world: %s\n", SKEY);
  login(NULL, NULL);
  logout(-1);
  return 0;
}
