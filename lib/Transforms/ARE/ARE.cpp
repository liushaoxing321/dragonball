//===------- ARE.cpp - Implements the Anti Reverse Engeering PASS. --------===//
//
// Copyright (C) 2017 Leslie Zhai <lesliezhai@llvm.org.cn>
//
// This file is part of DragonBall.
//
// DragonBall is free software; you can redistribute it and/or modify it under
// the terms of the GNU General Public License as published by the Free Software
// Foundation; either version 2, or (at your option) any later version.
//
// DragonBall is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with
// DragonBall; see the file COPYING.  If not, write to the Free Software
// Foundation, 51 Franklin Street, Suite 500, Boston, MA 02110-1335, USA.
//
//===----------------------------------------------------------------------===//
// This file implements Anti Reverse Engeering PASS.
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
    using FuncNameMapTy = StringMap<StringRef>;
    ARE() : FunctionPass(ID) {}

    bool runOnFunction(Function &F) override {
      StringRef FuncName = F.getName();
#ifdef DRAGONBALL_DEBUG
      errs() << "Function: " << FuncName << "\n";
#endif
      if (FuncName != StringRef("main")) {
        // Generate new function name.
        Twine T(FuncNames.size() + 1);
        Twine NewFuncName("dragonball" + T.str());
        // It is able to export FuncNames.
        FuncNames[FuncName] = StringRef(NewFuncName.str());
#ifdef DRAGONBALL_DEBUG
        errs() << FuncName << " -> " << NewFuncName.str() << "\n";
#endif
        F.setName(NewFuncName);
      }

      for (auto& B : F) {
        for (auto& I : B) {
#ifdef DRAGONBALL_DEBUG
          errs() << "Instruction: " << I.getType() << "\n";
#endif
        }
      }

      return true; // We modified the code.
    }

  private:
    FuncNameMapTy FuncNames;
  };
}

char ARE::ID = 0;
static RegisterPass<ARE> ARE("dragonball-are",
                             "an Anti Reverse Engeering Pass");
