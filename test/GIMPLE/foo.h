#ifndef FOO_H
#define FOO_H

#define LIBFOO_EXPORTED __attribute__((__visibility__("default")))

#ifdef __cplusplus
extern "C" {
#endif

LIBFOO_EXPORTED int foo();

#ifdef __cplusplus
}
#endif

#endif /* FOO_H */
