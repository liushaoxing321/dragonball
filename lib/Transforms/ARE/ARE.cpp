//===- ARE.cpp - an Anti Reverse Engeering Pass -----------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// TODO This file implements:
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Statistic.h"
#include "llvm/IR/Function.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

#define DEBUG_TYPE "dragonball-are"

namespace {
  // ARE
  struct ARE : public FunctionPass {
    static char ID; // Pass identification
    ARE() : FunctionPass(ID) {}

    bool runOnFunction(Function &F) override {
      errs() << "Function body:\n";
      F.dump();

      for (auto& B : F) {
        errs() << "Basic block:\n";
        B.dump();

        for (auto& I : B) {
          errs() << "Instruction: ";
          I.dump();
        }
      }
      return false;
    }
  };
}

char ARE::ID = 0;
static RegisterPass<ARE> ARE("dragonball-are",
                             "an Anti Reverse Engeering Pass");
