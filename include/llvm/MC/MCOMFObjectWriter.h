//===- llvm/MC/MCOMFObjectWriter.h - OMF Object Writer ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_MC_MCOMFOBJECTWRITER_H
#define LLVM_MC_MCOMFOBJECTWRITER_H

#include "llvm/Support/raw_ostream.h"

namespace llvm {

class MCObjectWriter;

/// \brief Construct a new OMF writer instance.
///
/// \param OS - The stream to write to.
/// \returns The constructed object writer.
std::unique_ptr<MCObjectWriter> createOMFObjectWriter(raw_pwrite_stream &OS);

} // end namespace llvm

#endif // LLVM_MC_MCOMFOBJECTWRITER_H
