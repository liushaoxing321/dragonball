DragonBall
----------

[dragonegg](http://dragonegg.llvm.org/) was really just a proof of concept: [Extracting the abstract syntax tree from GCC](https://lwn.net/Articles/629259/) 
would need to be complete enough to be usable as input to a proprietary 
compiler backend.

[Clang Tidy](http://clang.llvm.org/extra/clang-tidy/) is a proof that 
libclangStaticAnalyzer is reusable through AST gate.

So DragonBall is GCC Frontend -> GCC GIMPLE -> Clang AST -> Clang Analyzer Checker 
to static analysis Linux kernel and other [only for GCC](https://bugs.llvm.org/show_bug.cgi?id=22830) open source projects.

DragonBall is based on dragonegg, so it needs to open source the related part,
but I will close source GCC GIMPLE -> Clang AST and proprietary Checker.

