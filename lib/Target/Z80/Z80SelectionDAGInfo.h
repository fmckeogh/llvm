//===-- Z80SelectionDAGInfo.h - Z80 SelectionDAG Info -----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file defines the Z80 subclass for SelectionDAGTargetInfo.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Z80_Z80SELECTIONDAGINFO_H
#define LLVM_LIB_TARGET_Z80_Z80SELECTIONDAGINFO_H

#include "llvm/CodeGen/SelectionDAGTargetInfo.h"

namespace llvm {

class Z80SelectionDAGInfo : public SelectionDAGTargetInfo {
public:
  explicit Z80SelectionDAGInfo() = default;
};

}

#endif // LLVM_LIB_TARGET_Z80_Z80SELECTIONDAGINFO_H
