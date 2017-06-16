DragonBall
----------

[dragonegg](http://dragonegg.llvm.org/) might really piss RMS off but it was 
really just a proof of concept: [Extracting the abstract syntax tree from GCC](https://lwn.net/Articles/629259/) 
would need to be complete enough to be usable as input to a proprietary 
compiler backend.

[Clang Tidy](http://clang.llvm.org/extra/clang-tidy/) is a proof that 
libclangStaticAnalyzer is reusable through AST gate.

So DragonBall is GCC Frontend -> GCC AST -> Clang AST -> Clang Analyzer Checker 
to static analysis qtbase, glibc, linux kernel and other [only for GCC](https://bugs.llvm.org/show_bug.cgi?id=22830).


