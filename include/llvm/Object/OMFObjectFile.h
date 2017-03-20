//===- OMFObjectFile.h - OMF object file implementation ---------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file declares the OMFObjectFile class, which implements the ObjectFile
// interface for OMF files.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_OBJECT_OMF_H
#define LLVM_OBJECT_OMF_H

namespace llvm {
namespace object {

class OMFObjectFile : public ObjectFile {
};

} // end namespace object
} // end namespace llvm

#endif // LLVM_OBJECT_OMF_H
