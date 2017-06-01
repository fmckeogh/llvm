//===--- ZDSToolChain.cpp - ZDS Tool Chain --------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "ZDS.h"
#include "CommonArgs.h"
#include "clang/Driver/Compilation.h"
#include "clang/Driver/Options.h"
#include "llvm/Option/ArgList.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"

using namespace clang::driver;
using namespace clang::driver::toolchains;
using namespace llvm::opt;

void tools::ZDS::Assembler::ConstructJob(
    Compilation &C, const JobAction &JA, const InputInfo &Output,
    const InputInfoList &Inputs, const ArgList &Args,
    const char *LinkingOutput) const {
  assert(Inputs.size() == 1);
  claimNoWarnArgs(Args);

  InputInfo TempInput = Inputs[0], TempOutput = Output;
  while (true) {
    llvm::StringRef InputStem = llvm::sys::path::stem(TempInput.getFilename());
    TempOutput.setFilename(Args.MakeArgString(InputStem + ".obj"));
    if (llvm::sys::path::has_extension(TempInput.getFilename()) &&
        !llvm::sys::fs::exists(TempOutput.getFilename()))
      break;
    TempInput.setFilename(C.addTempFile(
        Args.MakeArgString(C.getDriver().GetTemporaryPath(InputStem, "asm"))));
  }

  if (Inputs[0].getFilename() != TempInput.getFilename())
    C.addCommand(
        llvm::make_unique<CopyFileCommand>(JA, *this, Inputs[0], TempInput));

  const char *Exec =
      Args.MakeArgString(getToolChain().GetProgramPath("ez80asm.exe"));
  ArgStringList CmdArgs;
  CmdArgs.push_back("-quiet");       // Suppress title
  CmdArgs.push_back("-cpu:EZ80F91"); // Suppress a warning
  CmdArgs.push_back("-NOlist");      // Suppress list file
  Args.AddAllArgValues(CmdArgs, options::OPT_Wa_COMMA, options::OPT_Xassembler);
  SmallString<128> WindowsInput;
  llvm::sys::path::native(TempInput.getFilename(), WindowsInput,
                          llvm::sys::path::Style::windows);
  CmdArgs.push_back(Args.MakeArgString(WindowsInput));
  C.addCommand(llvm::make_unique<Command>(JA, *this, Exec, CmdArgs, TempInput));

  if (std::strcmp(TempOutput.getFilename(), Output.getFilename())) {
    C.addResultFile(TempOutput.getFilename(), &JA);
    C.addCommand(
        llvm::make_unique<MoveFileCommand>(JA, *this, TempOutput, Output));
  }
}

void tools::ZDS::Linker::ConstructJob(
    Compilation &C, const JobAction &JA, const InputInfo &Output,
    const InputInfoList &Inputs, const ArgList &Args,
    const char *LinkingOutput) const {
  llvm_unreachable("Unimplemented!");
}

ZDSToolChain::ZDSToolChain(const Driver &D, const llvm::Triple &Triple,
                           const ArgList &Args)
    : ToolChain(D, Triple, Args) {}

Tool *ZDSToolChain::buildAssembler() const {
  return new tools::ZDS::Assembler(*this);
}

Tool *ZDSToolChain::buildLinker() const {
  return new tools::ZDS::Linker(*this);
}
