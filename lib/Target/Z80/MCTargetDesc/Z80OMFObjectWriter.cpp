//===-- Z80OMFObjectWriter.cpp - Z80 OMF Writer ---------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Z80MCTargetDesc.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCOMFObjectWriter.h"
#include "llvm/ADT/StringRef.h"

using namespace llvm;

std::unique_ptr<MCObjectWriter>
llvm::createZ80OMFObjectWriter(raw_pwrite_stream &OS) {
  return createOMFObjectWriter(OS);
}
