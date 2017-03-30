//===- lib/MC/MCSectionOMF.cpp - OMF Code Section Representation ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/MC/MCSectionOMF.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

MCSectionOMF::MCSectionOMF(const Twine &Section, SectionKind K, MCSymbol *Begin)
    : MCSection(SV_OMF, K, Begin) {
  Section.toVector(SectionName);
}

MCSectionOMF::~MCSectionOMF() {} // anchor.

// Decides whether a '.section' directive
// should be printed before the section name.
bool MCSectionOMF::ShouldOmitSectionDirective(StringRef Name,
                                              const MCAsmInfo &MAI) const {
  return MAI.shouldOmitSectionDirective(Name);
}

void MCSectionOMF::PrintSwitchToSection(const MCAsmInfo &MAI, const Triple &T,
                                         raw_ostream &OS,
                                         const MCExpr *Subsection) const {
  assert(!Subsection && "Unimplemented!");
  if (ShouldOmitSectionDirective(SectionName, MAI)) {
    OS << '\t' << getSectionName() << '\n';
    return;
  }

  OS << "\tSEGMENT\t" << getSectionName() << '\n';
}

bool MCSectionOMF::UseCodeAlign() const { return false; }

bool MCSectionOMF::isVirtualSection() const { return false; }
