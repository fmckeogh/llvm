//===- lib/MC/OMFObjectWriter.cpp - OMF File Writer -----------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements OMF object file writer information.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCOMFObjectWriter.h"

using namespace llvm;

std::unique_ptr<MCObjectWriter>
llvm::createOMFObjectWriter(raw_pwrite_stream &OS) {
  llvm_unreachable("Unimplemented!");
}
