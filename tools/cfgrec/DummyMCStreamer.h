//===-- llvm/CodeGen/MachineBasicBlock.h ------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// A dummy MCStreamer class just for catching the MCInsts and labels.
//
//===----------------------------------------------------------------------===//
#ifndef TARGET_CLP_CLPWCET_DUMMYMCSTREAMER_H
#define TARGET_CLP_CLPWCET_DUMMYMCSTREAMER_H

#include "llvm/ALF/LabelledInst.h"

#include "llvm/MC/MCStreamer.h"
#include "llvm/Support/Debug.h"

#include <tuple>

using namespace std;
using namespace llvm;

class DummyMCStreamer : public MCStreamer {
private:
	/* Stores the instructions */ 
	vector<LabelledInst> Insts;
	/* Stores the last seen labels, is cleared if an Inst is found */
	vector<string> lastLabels;

public:
  DummyMCStreamer(MCContext &Context) : MCStreamer(Context) { };
  virtual ~DummyMCStreamer() { };

  /* Important interface functions */
  void EmitInstruction(const MCInst &Inst, const MCSubtargetInfo &STI) override;
  virtual void EmitLabel(MCSymbol *Symbol) override;
  void EmitBytes(StringRef Data);

  /* Others not really implemented */
  virtual bool EmitSymbolAttribute (MCSymbol *Symbol, MCSymbolAttr Attribute);
  virtual void EmitCommonSymbol(MCSymbol *Symbol, uint64_t Size, unsigned ByteAlignment);
  virtual void EmitZerofill(MCSection *Section, MCSymbol *Symbol=nullptr, uint64_t Size=0, unsigned ByteAlignment=0);

  /* functions for iterator */
  using iterator = vector<LabelledInst>::iterator;
  using const_iterator = vector<LabelledInst>::const_iterator;

  void clear() { Insts.clear(); }
  void erase(iterator I) { Insts.erase(I); }
  size_t size() const { return Insts.size(); }
  iterator begin() { return Insts.begin(); }
  const_iterator begin() const { return Insts.begin(); }
  iterator end() { return Insts.end(); }
  const_iterator end() const { return Insts.end(); }

  iterator insert(iterator I, const LabelledInst &inst) {
	  return Insts.insert(I, inst);
  }
};

#endif
