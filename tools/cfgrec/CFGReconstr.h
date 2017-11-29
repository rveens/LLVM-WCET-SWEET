//===-- llvm/CodeGen/MachineBasicBlock.h ------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Implements CFG reconstruction for the CLP _specifically_. 
//
// Please see the paper by Cooper et al. :
// "Building a Control-flow Graph from Scheduled Assembly Code"
//
//===----------------------------------------------------------------------===//
#ifndef LIB_TARGET_CLP_CLPWCET_CFGRECONSTR_H
#define LIB_TARGET_CLP_CLPWCET_CFGRECONSTR_H

#include "DummyMCStreamer.h"
#include "LabelledInst.h"
#include "MCInstBB.h"
#include "OutputFileManager.h"
#include "OutputFileDOT.h"
#include "llvm/MC/MCInstPrinter.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCInstrAnalysis.h"

#include "llvm/Support/Debug.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/Support/FileSystem.h"

#include <vector>
#include <list>
#include <memory>
#include <set>
#include <functional>

using namespace llvm;
using namespace std;


class CFGReconstr {
private:
	vector<LabelledInst> &ds;
	vector<unsigned> &btargets; // all the branch targets 
	const MCRegisterInfo &MRI;
	MCInstPrinter &instPrinter;
	MCSubtargetInfo const &STI;
	const MCInstrAnalysis &MIA;

	shared_ptr<MCInstBB> startBB = make_shared<MCInstBB>();
public:
	CFGReconstr(vector<LabelledInst> &_ds, vector<unsigned> &_btargets, const MCRegisterInfo &_MRI, MCInstPrinter &_instPrinter, MCSubtargetInfo const &_STI, const MCInstrAnalysis &_MIA)
		: ds(_ds), btargets(_btargets), MRI(_MRI), instPrinter(_instPrinter), STI(_STI), MIA(_MIA)
	{
	}
	virtual ~CFGReconstr() { }
private:
  // helper functions:
  bool isBranch(LabelledInst &linst);
  bool isCondAnd_CondOr(MCInst &inst);
  bool isConditionalBranch(LabelledInst &curr);
  bool splitMCInstBB(shared_ptr<MCInstBB> given, unsigned int index, shared_ptr<MCInstBB> bComma);
  shared_ptr<MCInstBB> findBranchTarget(list<shared_ptr<MCInstBB>> bblist, LabelledInst &branch);
  shared_ptr<MCInstBB> findFallthrough(list<shared_ptr<MCInstBB>> bblist, shared_ptr<MCInstBB> bb);

  /* utility printing functions */
  void printLabelledInst(LabelledInst &lInst, bool printlabels = true, bool printOrigIndex = false);
  void prettyPrintMCInst(MCInst &inst);
  void printMCInstBBlist(list<shared_ptr<MCInstBB>> &list);
  void printCFG(shared_ptr<MCInstBB> bb, set<shared_ptr<MCInstBB>> &seen);

  // step one fom Cooper article (split up BB based on labels)
  list<shared_ptr<MCInstBB>> CFGReconstrStepOne();

  // step two fom Cooper article (add branches. do not consider branches in delay slots)
  list<shared_ptr<MCInstBB>> CFGReconstrStepTwo(list<shared_ptr<MCInstBB>> bblist, list<shared_ptr<MCInstBB>> &bblist_unreachable);

  // step three fom Cooper article (consider branches in delay slots)
  void CFGReconstrStepThree(shared_ptr<MCInstBB> firstBB, list<shared_ptr<MCInstBB>> bblist);
  void processBlock(shared_ptr<MCInstBB> block,
								vector<std::pair<LabelledInst &, unsigned int>> &counterList,
								list<pair<shared_ptr<MCInstBB>, vector<std::pair<LabelledInst &, unsigned int>>>> &worklist,
								list<shared_ptr<MCInstBB>> bblist);

  list<shared_ptr<MCInstBB>> CFGMergeBBs(list<shared_ptr<MCInstBB>> bblist);
 
public:
  void reconstructCFG();
};

#endif
