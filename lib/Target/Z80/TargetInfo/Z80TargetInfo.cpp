//===-- Z80TargetInfo.cpp - Z80 Target Implementation ---------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "MCTargetDesc/Z80MCTargetDesc.h"
#include "llvm/Support/TargetRegistry.h"
using namespace llvm;

Target llvm::TheZ80Target, llvm::TheEZ80Target;

extern "C" void LLVMInitializeZ80TargetInfo() {
  RegisterTarget<Triple::z80, /*HasJIT=*/false> X(TheZ80Target, "z80", "Z80");
  RegisterTarget<Triple::ez80, /*HasJIT=*/false> Y(TheEZ80Target, "ez80", "EZ80");
}

