//===--- ZDS.h - ZDS ToolChain Implementations ------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ZDS_H
#define LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ZDS_H

#include "clang/Driver/Tool.h"
#include "clang/Driver/ToolChain.h"

namespace clang {
namespace driver {
namespace tools {

// Zilog Developer Studio tools
namespace ZDS {
class LLVM_LIBRARY_VISIBILITY Assembler : public Tool {
public:
  explicit Assembler(const ToolChain &TC)
    : Tool("ZDS::Assembler", "assembler", TC) {}

  bool hasIntegratedCPP() const override { return false; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};

class LLVM_LIBRARY_VISIBILITY Linker : public Tool {
public:
  explicit Linker(const ToolChain &TC)
    : Tool("ZDS::Linker", "linker", TC) {}

  bool hasIntegratedCPP() const override { return false; }
  bool isLinkJob() const override { return true; }

  void ConstructJob(Compilation &C, const JobAction &JA,
                    const InputInfo &Output, const InputInfoList &Inputs,
                    const llvm::opt::ArgList &TCArgs,
                    const char *LinkingOutput) const override;
};
} // end namespace zds

} // end namespace tools

namespace toolchains {

class LLVM_LIBRARY_VISIBILITY ZDSToolChain : public ToolChain {
public:
  ZDSToolChain(const Driver &D, const llvm::Triple &T,
               const llvm::opt::ArgList &Args);

  bool IsIntegratedAssemblerDefault() const override { return false; }
  bool isPICDefault() const override { return false; }
  bool isPIEDefault() const override { return false; }
  bool isPICDefaultForced() const override { return false; }

protected:
  Tool *buildAssembler() const override;
  Tool *buildLinker() const override;
};

} // end namespace toolchains
} // end namespace driver
} // end namespace clang

#endif // LLVM_CLANG_LIB_DRIVER_TOOLCHAINS_ZDS_H
