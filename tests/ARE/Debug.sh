#!/bin/bash 

rm -f a.out *.bc *.ll

LC=$1
if [[ -z "$LC" ]]; then
  LC=llvm-config
fi
$LC --version

LLVM_BINDIR=$($LC --bindir)
LLVM_LIBDIR=$($LC --libdir)
CFLAGS=-fdeclspec

# TESTCASE1
# change function name
$LLVM_BINDIR/clang $CFLAGS -emit-llvm -S func.c -o func.ll
$LLVM_BINDIR/llc func.ll -o func.s
$LLVM_BINDIR/clang $CFLAGS -fdeclspec -emit-llvm func.c -c -o func.bc
$LLVM_BINDIR/lli func.bc
$LLVM_BINDIR/opt -load $LLVM_LIBDIR/DragonBallARE.so -dragonball-are-func < func.bc > func-mod.bc
$LLVM_BINDIR/lli func-mod.bc
$LLVM_BINDIR/llvm-dis func-mod.bc -o func-mod.ll
$LLVM_BINDIR/llc func-mod.ll -o func-mod.s

