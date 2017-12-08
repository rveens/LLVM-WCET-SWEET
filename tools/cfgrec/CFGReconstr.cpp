#include "CFGReconstr.h"

#define DEBUG_TYPE "clp-wcet"

/* #include "CLPGenFindCLPBranch.inc" */

#include <typeinfo>
#include <algorithm>

#include "llvm/CodeGen/MachineInstrBuilder.h"

// bleh
#include "../../lib/Target/ARM/ALF/ARMALFWriter.h"

bool CFGReconstr::isBranch(LabelledInst &linst)
{
	dbgs() << "branch-target is:" << linst.branchTarget << "\n";
	return MIA.isBranch(linst.inst) && linst.branchTarget != 0 && !MIA.isCall(linst.inst);
	/* const uint64_t &code = GetCLPOpCode(inst); */

	/* // extract the CLP opcode field (bits 31-24). */
	/* const uint8_t &opcode = (code & 0xFF000000) >> 24; */

	/* if (inst.size() != 2) { */
	/* 	/1* DEBUG(dbgs() << "  Need 2 operands!\n"); *1/ */
	/* 	return false; */
	/* } */

	/* // check if the instruction is a MOV */
	/* /1* if (opcode == 0b10000100) { *1/ */
	/* if (opcode == 0x84) { */
	/* 	/1* DEBUG(dbgs() << "  Found a MOV!\n"); *1/ */
	/* 	// check if the target-register is the RPC register */
	/* 	const MCOperand &targetReg = inst.getOperand(1); */
	/* 	if (StringRef("RPC") == StringRef(MRI.getName(targetReg.getReg()))) { */
	/* 		/1* DEBUG(dbgs() << "  Found a branch!\n"); *1/ */
	/* 		return true; */
	/* 	} */
	/* } else // check if the instruction is MOVATO, MOVBTO, MOVSUTO */
	/* /1* if (opcode == 0b10000001 || opcode == 0b10000010 || opcode == 0b10000011) { *1/ */
	/* if (opcode == 0x81 || opcode == 0x82 || opcode == 0x83) { */
	/* 	/1* DEBUG(dbgs() << "Found a MOVATO, MOVBTO, MOVSUTO!\n"); *1/ */
	/* 	const MCOperand &targetReg = inst.getOperand(1); */
	/* 	if (StringRef("RPC") == StringRef(MRI.getName(targetReg.getReg()))) { */
	/* 		/1* DEBUG(dbgs() << "  Found a branch!\n"); *1/ */
	/* 		return true; */
	/* 	} */
	/* } else { // remaining option is MOVIND. Do not consider for now. */
	/* 	return false; */
	/* } */
}

bool CFGReconstr::isCondAnd_CondOr(MCInst &inst)
{
#define DEBUG_TYPE "clp-wcet"
	/* const uint64_t &code = GetCLPOpCode(inst); */

	/* // extract the CLP opcode field (bits 31-24). */
	/* const uint8_t &opcode = (code & 0xFF000000) >> 24; */

	/* // check if the instruction is a CONDAND, CONDOR */
	/* /1* if (opcode == 0b11000101 || opcode == 0b11000100) { *1/ */
	/* if (opcode == 0xC5 || opcode == 0xC4) { */
	/* 	DEBUG(dbgs() << "Found a CONDAND, CONDOR!\n"); */
	/* 	return true; */
	/* } else { */ 
		return false;
	/* } */
#undef DEBUG_TYPE
}

bool CFGReconstr::isConditionalBranch(LabelledInst &curr)
{
#define DEBUG_TYPE "clp-wcet"
	DEBUG(dbgs() << "DEBUG isConditonalBranch - checking on curr: ");
	prettyPrintMCInst(curr.inst);
	if (isBranch(curr) && MIA.isConditionalBranch(curr.inst))
		DEBUG(dbgs() << "Branch is conditional\n\n");
	else
		DEBUG(dbgs() << "Branch is not conditional\n\n");
	
	/* DEBUG(dbgs() << "DEBUG isConditonalBranch - checking on curr: "); */
	/* prettyPrintMCInst(curr); */
	/* DEBUG(dbgs() << "  and checking on prev: "); */
	/* prettyPrintMCInst(prev); */
	/* DEBUG(dbgs() << "\n"); */
	/* if (isCLPBranch(curr)) { */
	/* 	if (isCondAnd_CondOr(prev)) { */
	/* 		DEBUG(dbgs() << "Branch is conditional\n\n"); */
	/* 		return true; */
	/* 	} */
	/* } */
	/* DEBUG(dbgs() << "Branch is not conditional\n\n"); */
	/* return false; */
#undef DEBUG_TYPE
}

list<shared_ptr<MCInstBB>> CFGReconstr::CFGReconstrStepOne()
{
#define DEBUG_TYPE "clp-wcet"

	DEBUG(dbgs() << "--------------------------------------------------------------------------------\n");
	DEBUG(dbgs() << "| DEBUG Starting Step One                                                      |\n");
	DEBUG(dbgs() << "--------------------------------------------------------------------------------\n");

	// make basic blocks based on labels only.
	list<shared_ptr<MCInstBB>> BBlist;

	shared_ptr<MCInstBB> currBB = make_shared<MCInstBB>();
	bool handlefirstBB = false;

	// loop through the list of instructions
	for (LabelledInst &inst : ds) {
		// if the inst has incoming branch targets, store and make a new BB
		if (handlefirstBB) {
			for (unsigned i : btargets)
				if (i == inst.origIndex) {
					BBlist.push_back(currBB);
					currBB = make_shared<MCInstBB>();
					break; 
				}
		}
		// if the instr is a return, set a flag in the MCInstBB
		if (MIA.isReturn(inst.inst)) {
			currBB->isReturn = true;
		}
		if (MIA.isCall(inst.inst)) {
			currBB->isCall = true;
		}
		/* if (!inst.label.empty() &&) { */
		/* } */
		currBB->Insts.push_back(inst);
		handlefirstBB = true;
	}
	if (!currBB->Insts.empty()) { // add last basic block if not empty
		BBlist.push_back(currBB);
	}

	DEBUG(dbgs() << "DEBUG Step one results:\n\n");
	DEBUG(printMCInstBBlist(BBlist));

	return BBlist;
#undef DEBUG_TYPE
}


shared_ptr<MCInstBB> CFGReconstr::findBranchTarget(list<shared_ptr<MCInstBB>> bblist, LabelledInst &branch)
{
#define DEBUG_TYPE "clp-wcet"
	DEBUG(dbgs() << "DEBUG findBranchTarget - looking at instr:\n");
	DEBUG(printLabelledInst(branch, true, true));
	dbgs() << "i, ti: " << branch.origIndex << " " << branch.branchTarget << "\n";

	// 1) first check if the given instruction is really a branch.  if not: retrurn.
	if (!isBranch(branch))
		return nullptr;

	if (branch.branchTarget)  { // check if not 0.
		unsigned btargetIdx = branch.branchTarget;

		for (auto linst : bblist) {
			if (linst->Insts[0].origIndex == btargetIdx)
				return linst;
		}
	}
	
	/* // 2) obtain the target-label from the given branch */
	/* string targetlabel; */
	/* for (auto &op : branch.inst) { */
	/* 	if (op.isExpr()) { */
	/* 		if (auto *symref = dyn_cast<MCSymbolRefExpr>(op.getExpr())) { */
	/* 			targetlabel = symref->getSymbol().getName(); */
	/* 		} */
	/* 	} */
	/* } */
	/* DEBUG(dbgs() << "Extracted label '" << targetlabel << "'\n"); */

	/* // 3) for each MCInstBB in bblist, find check if the target-label matches */
	/* for (auto &bb : bblist) { */
	/* 	auto &firstInst = bb->Insts[0]; */
	/* 	string &l = firstInst.label; */
	/* 	if (l == targetlabel) { */
	/* 		// done! return this bb */
	/* 		DEBUG(dbgs() << "Found branch target bb: \n"); */
	/* 		for (auto &linst : bb->Insts) { */
	/* 			DEBUG(printLabelledInst(linst, true, true)); */
	/* 		} */
	/* 		DEBUG(dbgs() << "\n"); */
	/* 		return bb; */
	/* 	} */
	/* } */

	// we didn't find any :(
	DEBUG(dbgs() << "ERROR: could not find branch target of branch");
	DEBUG(printLabelledInst(branch, false));
	return nullptr;
#undef DEBUG_TYPE
}

list<shared_ptr<MCInstBB>> CFGReconstr::CFGReconstrStepTwo(list<shared_ptr<MCInstBB>> bblist, list<shared_ptr<MCInstBB>> &bblist_unreachable)
{
#define DEBUG_TYPE "clp-wcet"
	// make basic blocks by looking at branches
	// note: ignores branches in branch-delay slots

	DEBUG(dbgs() << "\n");
	DEBUG(dbgs() << "--------------------------------------------------------------------------------\n");
	DEBUG(dbgs() << "| DEBUG Starting Step Two                                                      |\n");
	DEBUG(dbgs() << "--------------------------------------------------------------------------------\n");
	
	unsigned int countdown = 0;
	unsigned int BBIndex = 0; // additional variable to find index for splitting BB
	bool branch_found = false;
	bool branch_at_end_of_BB = false;

	// take a reference to the entry-BB
	shared_ptr<MCInstBB> returnValue = bblist.front();
	// save the original list, needed for some functions
	list<shared_ptr<MCInstBB>> origbblist = bblist;
	// another list, to keep track of all the basic blocks (not the working list)
	list<shared_ptr<MCInstBB>> bblist_total = bblist;

	while (!bblist.empty()) {
		DEBUG(dbgs() << "*** DEBUG new StepTwo iteration. Printing bblist (block_list) ***\n\n");
		DEBUG(dbgs() << "------- START -------\n\n");
		DEBUG(printMCInstBBlist(bblist));
		DEBUG(dbgs() << "------- END -------\n\n");

		// remove b from block_list
		shared_ptr<MCInstBB> b = bblist.front();
		bblist.pop_front();
		branch_found = false;
		branch_at_end_of_BB = false;

		auto I = b->Insts.begin(); 
		BBIndex = 0;
		for (; I != b->Insts.end(); I++, BBIndex++) {

			if (isBranch(*I)) {
				branch_found = true;
				countdown = 0; // delayslot for CLP is always one
				break;
			}
		}
		if (branch_found) {
			for (auto P = I; P != b->Insts.end(); P++) {
				/* countdown--; */
				if (countdown == 0) {
					if (std::next(std::next(P)) == b->Insts.end()) {
						DEBUG(dbgs() << "DEBUG counter = 0 on basic block end.\n");
					}
					break;
				}
				BBIndex++;
			}
			if (countdown == 0) {
				// split b at p (or: BBIndex)
				shared_ptr<MCInstBB> bComma = make_shared<MCInstBB>(); // b'
				branch_at_end_of_BB = !splitMCInstBB(b, BBIndex, bComma);

				if (bComma && !bComma->Insts.empty()) {
					bblist.push_back(bComma); // add b' to block_list
					// find b in bblist_total. we need to insert bComma after b
					// to keep the order.
					auto it = std::find(bblist_total.begin(), bblist_total.end(), b); 
					// note: ++it works because list.insert(list.end(), val) is valid
					if (it != bblist_total.end())
						bblist_total.insert(++it, bComma);
				}

				// add edges from b to targets of I
				shared_ptr<MCInstBB> bTarget = findBranchTarget(origbblist, *I);
				if (bTarget && !bTarget->Insts.empty()) {
					// jump goes to bTarget
					b->jump = bTarget;
					bTarget->incoming.insert(b);
				}

				// if i is conditional add edge from b to b'
				if (bComma && !bComma->Insts.empty()) {
					if (isConditionalBranch(*I)) {
						b->fall_through = bComma;
						bComma->incoming.insert(b);
					}
				}
			}
		}
		if (!branch_found ) {
			// add edge from b to fallthrough of b
			shared_ptr<MCInstBB> ft = findFallthrough(bblist_total, b);
			if (ft) {
				b->fall_through = ft;
				ft->incoming.insert(b);
			}
		}
	}

	// afterwards, see if we can find unreachable basic blocks
	// unreachable = they have no incoming branches
	for (auto bb : bblist_total) {
		if (bb->isUnreachable() && !bb->isInitialBasicBlock())
			bblist_unreachable.push_back(bb);
	}

	DEBUG(dbgs() << "\nDEBUG Step two results:\n");
	DEBUG(printMCInstBBlist(bblist_total));

	DEBUG(dbgs() << "\nUnreachable BBs:\n");
	DEBUG(printMCInstBBlist(bblist_unreachable));

	DEBUG(dbgs() << "\n");

	return bblist_total;
#undef DEBUG_TYPE
}

list<shared_ptr<MCInstBB>> CFGReconstr::CFGMergeBBs(list<shared_ptr<MCInstBB>> bblist)
{
	// Merge basic blocks, IF:
	//  - BB 'x' has only a fall-through BB 'y'
	//	- 'y' does not have incoming branches other than 'x'
	// What to do when merging:
	// 	- extend BB 'x' with the instructions of 'y'.
	// 	- set the jump and fall-through of 'x' to be those of 'y'
	for (auto bbX : bblist) {
		if (bbX->fall_through != nullptr && bbX->jump == nullptr) {
			auto bbY = bbX->fall_through;
			if (bbY->incoming.size() == 1) {
				// conditions met. we can merge bbY into bbX
				for (auto linst : *bbY) {
					bbX->Insts.push_back(linst);
				}
				// clear bbY
				bbY->Insts.clear();
				bbX->jump = bbY->jump;
				bbX->fall_through = bbY->fall_through;
			}
		}
	}
	// remove empty basic block from bblist
	bblist.erase(std::remove_if(bblist.begin(), bblist.end(),
		[](shared_ptr<MCInstBB> bb) {
			return bb->Insts.empty();
		}), bblist.end());

	return bblist;
}

void CFGReconstr::CFGReconstrStepThree( shared_ptr<MCInstBB> firstBB,
										list<shared_ptr<MCInstBB>> bblist)
{
#define DEBUG_TYPE "clp-wcet"
	DEBUG(dbgs() << "\n");
	DEBUG(dbgs() << "\n");
	DEBUG(dbgs() << "--------------------------------------------------------------------------------\n");
	DEBUG(dbgs() << "| DEBUG Starting Step Three                                                    |\n");
	DEBUG(dbgs() << "--------------------------------------------------------------------------------\n");

	// consider branches in branch delay slots
	/* worklist = {start-block:Ø} */
	list<pair<shared_ptr<MCInstBB>, vector<std::pair<LabelledInst &, unsigned int>>>> workList;
	workList.push_back(make_pair(firstBB, vector<std::pair<LabelledInst &, unsigned int>>()));

	/* while (worklist) */
	while (!workList.empty()) {
		DEBUG(dbgs() << "\n");
		DEBUG(dbgs() << "DEBUG new step three iteration. Basic block print: \n\n");
		DEBUG(dbgs() << "------- START -------\n\n");
		set<shared_ptr<MCInstBB>> seen0;
		DEBUG(printCFG(firstBB, seen0));
		DEBUG(dbgs() << "------- END -------\n\n");

	/* 	remove element e from worklist */
		pair<shared_ptr<MCInstBB>, vector<std::pair<LabelledInst &, unsigned int>>> e = workList.back();
		workList.pop_back();
	/* 	process-block(e.block, e.list) */
		processBlock(e.first, e.second, workList, bblist);
	}
#undef DEBUG_TYPE
}

void CFGReconstr::processBlock( shared_ptr<MCInstBB> block,
								vector<std::pair<LabelledInst &, unsigned int>> &counterList,
								list<pair<shared_ptr<MCInstBB>, vector<std::pair<LabelledInst &, unsigned int>>>> &worklist,
								list<shared_ptr<MCInstBB>> bblist)
{
#define DEBUG_TYPE "clp-wcet"
	DEBUG(dbgs() << "DEBUG processBlock\n");
	// vector of pairs. each pair is a pair between a block and a counterList.
	static vector<pair<shared_ptr<MCInstBB>, vector<std::pair<LabelledInst &, unsigned int>>>> seenBefore;

	/* if block has been seen with counter list before */
	/* 	break */
	if (find(seenBefore.begin(), seenBefore.end(), make_pair(block, counterList)) != seenBefore.end())
		return;

	if (!block) {
		DEBUG(dbgs() << "error! given block is a nullptr.\n");
		return;
	}

	/* for each instruction i in block */
	auto I = block->begin(); 
	unsigned int BBIndex = 0;
	for (; I != block->end(); I++, BBIndex++) {
	/* 	decrement counters in counter list */
		for (auto &counter : counterList) {
			DEBUG(dbgs() << "before: " << counter.second << "\n");
			counter.second--;
			DEBUG(dbgs() << "after: " << counter.second << "\n");
		}
	/* 	if i is a branch */
		if (isBranch(*I)) {
	/* 		counter list = counter list + {i : branch-latency} */
			counterList.emplace_back(*I, 1); // CLP branches always have a latency of one
		}
	/* 	if any counter in counter list = 0 */
	/* 		break for */
		bool mustBreakFor = false;
		for (auto counter : counterList ) {
			if (counter.second == 0)
				mustBreakFor = true;
		}
		if (mustBreakFor)
			break;
	}

	/* if i is not at end of block */
	if (I != block->Insts.end() && ++I != block->Insts.end()) {
	/* 	create new block with remaining instructions in block */
		shared_ptr<MCInstBB> newBB = make_shared<MCInstBB>();
		splitMCInstBB(block, BBIndex, newBB);
		// Put outgoing labels into new block
		newBB->jump = block->jump;
		newBB->fall_through = block->fall_through;
		block->jump = nullptr;
	/* 	add edge from block to new block */
		block->fall_through = newBB;
	}

	/* if no counter in counter list = 0 */
	bool noCounterIsZero = true;
	LabelledInst dummyLinst; // due to reference
	std::pair<LabelledInst &, unsigned int> counterWithZero(dummyLinst, 0);
	for (auto counter : counterList ) {
		if (counter.second == 0) {
			noCounterIsZero = false;
			counterWithZero = counter;
		}
	}
	if (noCounterIsZero) {
	/* 	let f = block’s fall through block */
		auto fallthrough = findFallthrough(bblist, block);
		if (fallthrough) {
			for (auto x : *block) {
				DEBUG(printLabelledInst(x));
			}
		/* 	worklist = worklist + { f : counter list } */
			worklist.emplace_back(fallthrough, counterList);
		}
	/* else */
	} else {
	/* 	let j = branch instruction in counter list with (counter = 0) */
		// (assume here that counterWithZero is set) 
		auto j = counterWithZero.first;
	/* 	for each target block t of instruction j */ // (only one target possible for CLP)
		auto t = findBranchTarget(bblist, j);
	/* 		add edge from block to target t */
		block->jump = t;
	/* 		worklist = worklist + { t : counter list - {j : 0}} */
		counterList.erase(std::remove(counterList.begin(), counterList.end(), counterWithZero), counterList.end());
		worklist.emplace_back(t, counterList);
	}

#undef DEBUG_TYPE
}

void CFGReconstr::reconstructCFG()
{
#define DEBUG_TYPE "clp-wcet"
	DEBUG(dbgs() << "DEBUG Printing Instructions:\n\n");
	for (LabelledInst &lInst : ds) {
		DEBUG(printLabelledInst(lInst, true, true));
	}
	DEBUG(dbgs() << "\n\n");

	std::list<shared_ptr<MCInstBB>> bblist = CFGReconstrStepOne();

	std::list<shared_ptr<MCInstBB>> list_unreachable;
	list<shared_ptr<MCInstBB>> bblist_reconstr = CFGReconstrStepTwo(bblist, list_unreachable);
	/* CFGReconstrStepThree(firstbb, list); */
	/* list<shared_ptr<MCInstBB>> bblist_bbmerged = CFGMergeBBs(bblist_reconstr); */

	LLVMContext ctx;
	Module m("CFG", ctx);
	m.setDataLayout(TM.createDataLayout());
	Function *f = Function::Create(FunctionType::get(Type::getVoidTy(ctx), false), GlobalValue::CommonLinkage, "main", &m);

	shared_ptr<MachineFunction> mf = makeMI(f, bblist_reconstr);
	doARMALFWriter(mf);

	OutputFileManager OFM;
	std::shared_ptr<OutputFileInterface> ofd = make_shared<OutputFileDOT>(instPrinter, STI);
	OFM.addOutput(ofd);
	OFM.doOutputs(bblist_reconstr, list_unreachable);
#undef DEBUG_TYPE
}

shared_ptr<MachineFunction> CFGReconstr::makeMI(Function *f, std::list<shared_ptr<MCInstBB>> bblist)
{
	if (f) {
		MachineModuleInfo MMI(&TM);
		shared_ptr<MachineFunction> mf = make_shared<MachineFunction>(f, TM, 0, MMI);
		for (auto bb : bblist) {
			MachineBasicBlock *mbb = mf->CreateMachineBasicBlock();
			mf->push_back(mbb);
			bb->mbb = mbb;
		}

		// loop again over the MBBs to fix the successors/predecessors
		for (auto bb : bblist) {
			if (bb->jump)
				bb->mbb->addSuccessorWithoutProb(bb->jump->mbb);
			if (bb->fall_through)
				bb->mbb->addSuccessorWithoutProb(bb->fall_through->mbb);

			// now we add the operands (bb->mbb was needed)
			for (auto linst : *bb) {
				MCInst &mci = linst.inst;
				if (AW) {
					AW->HigherMCInstToMachineInstr(bb, bb->mbb, mci);
				}
			}
		}
		mf->dump();
		return mf;
	}
	return nullptr;
}

void CFGReconstr::doARMALFWriter(shared_ptr<MachineFunction> mf)
{
	/* for (auto mbb : *mf) */
	/* 	for (auto mi : mbb) { */
			/* switch (mi.getOpcode()) { */
			/* 	/1* case ARM::tBX: *1/ */
			/* 		/1* break; *1/ */
			/* } */
		/* } */
	// fix 'tBX lr' back to tBX_RET


	if (mf) {
		ARMALFWriter aaw;
		aaw.runOnMachineFunction(*mf);
	}
}

void CFGReconstr::printLabelledInst(LabelledInst &lInst, bool printlabels, bool printOrigIndex)
{
#define DEBUG_TYPE "clp-wcet"
	if (printlabels) {
		DEBUG(prettyPrintMCInst(lInst.inst));
		DEBUG(dbgs() << "\t//  label:");
		DEBUG(dbgs() << " " << lInst.label);
	}
	if (printOrigIndex) {
		DEBUG(dbgs() << "\tindex:" << lInst.origIndex);
	}
	DEBUG(dbgs() << "\n");
#undef DEBUG_TYPE
}

void CFGReconstr::prettyPrintMCInst(MCInst &inst)
{
/* #define DEBUG_TYPE "clp-wcet" */
	instPrinter.printInst(&inst, dbgs(), StringRef(""), STI);
	dbgs() << "\t(" << MCII.getName(inst.getOpcode()) << ") \n";
/* #undef DEBUG_TYPE */
}

void CFGReconstr::printMCInstBBlist(list<shared_ptr<MCInstBB>> &list)
{
#define DEBUG_TYPE "clp-wcet"
	for (auto &bb : list) {
		DEBUG(dbgs() << "BB '" << bb->getBBLabel() << "':" << "\n");
		for (auto &linst : bb->Insts) {
			DEBUG(printLabelledInst(linst, true, true));
		}
		if (bb->size() == 0) {
			DEBUG(dbgs() << " -\n");
		}
		DEBUG(dbgs() << "\n");
	}
	if (list.empty()) {
		DEBUG(dbgs() << " none\n");
	}
#undef DEBUG_TYPE
}

bool CFGReconstr::splitMCInstBB(shared_ptr<MCInstBB> given, unsigned int index, shared_ptr<MCInstBB> bComma)
{
#define DEBUG_TYPE "clp-wcet"
	static unsigned int newBBlabelCounter = 0;

	unsigned int counter = 0;

	DEBUG(dbgs() << "DEBUG splitMCInstBB - going to the following split bb at index " << index << ":\n");

	for (auto &linst : given->Insts) {
		DEBUG(printLabelledInst(linst, true, true));
	}

	// empty basic block should never happen
	assert(!given->Insts.empty() && "cancelling: given basic block was empty.");

	// set counter to the index of the first instr of the given basic block
	counter = given->Insts[0].origIndex;

	// we cannot split if the BB has only one instr
	if (given->Insts.size() <= 1) {
		DEBUG(dbgs() << "cancelling: basic block has size 1 or empty.\n\n");
		return false;
	}

	// we cannot split if the given index is equal to the BB size
	if (index == given->Insts.size()-1) {
		DEBUG(dbgs() << "cancelling: given index equal to basic block size\n\n");
		return false;
	}

	/* dbgs() << "doing my thing now\n"; */
	given->Insts.erase(remove_if(
						given->Insts.begin(),
						given->Insts.end(), 
						[&](LabelledInst &linst) {
							if (counter++ > index + given->Insts[0].origIndex) {
								bComma->Insts.push_back(linst);
								return true;
							} else
								return false;
						}), 
						given->Insts.end());

	// add unique label to first instr of Comma
	if (!bComma->Insts.empty()) {
		bComma->Insts[0].label = string("newBB") + std::to_string(newBBlabelCounter++);
	}

	DEBUG(dbgs() << "\n resulting b:\n");
	for (auto &linst : given->Insts) {
		DEBUG(printLabelledInst(linst, true, true));
	}
	DEBUG(dbgs() << "\n and bComma:\n");
	for (auto &linst : bComma->Insts) {
		DEBUG(printLabelledInst(linst, true, true));
	}
	DEBUG(dbgs() << "\n");

	return true;
#undef DEBUG_TYPE
}

shared_ptr<MCInstBB> CFGReconstr::findFallthrough(list<shared_ptr<MCInstBB>> bblist, shared_ptr<MCInstBB> given)
{
#define DEBUG_TYPE "clp-wcet"

	// pick the last instruction of the given BB
	LabelledInst lInst = given->Insts.back();
	// if the basic-block has an unconditional branch, return a nullpointer
	if (MIA.isUnconditionalBranch(lInst.inst))
		return nullptr;
	// check if ends in a jump, if so, return nullptr

	unsigned int idxToLookFor = lInst.origIndex;
	// add one
	idxToLookFor += 2;
	DEBUG(dbgs() << "DEBUG findFallthrough - Looking for fallthrough of following bb: \n");
	for (auto &linst : given->Insts) {
		DEBUG(printLabelledInst(linst, true, true));
	}
	DEBUG(dbgs() << "Looking for index: " << idxToLookFor << "\n");

	// loop through the basic blocks, try to find the next index
	for (auto &bb : bblist) {
		DEBUG(dbgs() << "Looking at index " << bb->Insts[0].origIndex << "\n");
		// check the first instr.
		if (bb->Insts[0].origIndex == idxToLookFor) {
			DEBUG(dbgs() << "Found bb with idx " << idxToLookFor << "\n\n");
			return bb;
		}
	}
	DEBUG(dbgs() << "not found\n");
	return nullptr;

#undef DEBUG_TYPE
}

void CFGReconstr::printCFG(shared_ptr<MCInstBB> bb, set<shared_ptr<MCInstBB>> &seen)
{
#define DEBUG_TYPE "clp-wcet"
	bb->walkCFG(bb, seen, [&](shared_ptr<MCInstBB> bbb) {
				// print the current bb
				DEBUG(dbgs() << "BB: \n");
				for (auto &linst : bbb->Insts) {
					DEBUG(printLabelledInst(linst, true, true));
				}
	});
#undef DEBUG_TYPE
}


