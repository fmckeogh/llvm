//===-- Z80FixupKinds.h - Z80 Specific Fixup Entries ------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_LIB_TARGET_Z80_MCTARGETDESC_Z80FIXUPKINDS_H
#define LLVM_LIB_TARGET_Z80_MCTARGETDESC_Z80FIXUPKINDS_H

#include "llvm/MC/MCFixup.h"

namespace llvm {
namespace Z80 {
enum Fixups {
  // Marker
  LastTargetFixupKind = FirstTargetFixupKind,
  NumTargetFixupKinds = LastTargetFixupKind - FirstTargetFixupKind
};
}
}

#endif
