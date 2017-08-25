DragonBall
----------

[dragonegg](http://dragonegg.llvm.org.cn/) was really just a proof of concept:
 [Extracting the abstract syntax tree from GCC](https://lwn.net/Articles/629259/) 
would need to be complete enough to be usable as input to a proprietary
 compiler backend.

[Clang Tidy](http://clang.llvm.org/extra/clang-tidy/) is a proof that 
libclangStaticAnalyzer
 ([Clang Static Analyzer](http://clang-analyzer.llvm.org.cn/) library) is
 reusable through AST gate.

So DragonBall is GCC Frontend -> GCC GIMPLE -> Clang AST -> Clang Analyzer
 Checker to static analysis Linux kernel and other
 [only for GCC](https://bugs.llvm.org/show_bug.cgi?id=22830) open source projects.
and it also provides LLVM PASS to modify IR for ARE (Anti-Reverse Engeering) purpose.

## Build

### Fedora 25

```
# dnf upgrade --refresh
# dnf install gcc-plugin-devel llvm-devel ncurses-devel zlib-devel gcc-c++ redhat-rpm-config
$ make -j4
```

## Debug

### Fedora 25

```
$ gcc -fplugin=./dragonball.so \
    -fplugin-arg-dragonegg-debug-pass-arguments \
    -ftime-report \
    -fverbose-asm \
    -fplugin-arg-dragonegg-enable-gcc-optzns \
    -fplugin-arg-dragonegg-emit-ir \
    -S \
    test/GIMPLE/bare.c \
    -wrapper gdb,--args
```

