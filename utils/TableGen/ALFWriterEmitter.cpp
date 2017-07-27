#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"
#include "CodeGenRegisters.h"
#include "CodeGenTarget.h"
#include "CodeGenInstruction.h"
#include "CodeGenDAGPatterns.h"

#include "llvm/ADT/StringExtras.h"

#include <vector>

using namespace llvm;
using namespace std;

namespace {

class ALFWriterEmitter {
	RecordKeeper &Records;
	CodeGenTarget Target;
	vector<const CodeGenInstruction *> NumberedInstructions;
	CodeGenDAGPatterns CGDP;

	// Info needed for register
	struct ALFReg {
		const CodeGenRegister* reg;
		unsigned int bitwidth;
	};

	vector<ALFReg> RootRegs;


public:
	ALFWriterEmitter(RecordKeeper &R) : Records(R), Target(R), CGDP(Records)
	{
	}

	void run(raw_ostream &O)
	{
		/* outputCortexM3InstrsTEST(O); */
		/* outputCortexM3AssemblerPredicatesTEST(O); */
		/* outputCortexM3PredicatesTEST(O); */

		/* outputCortexM0AssemblerPredicatesTEST(O); */
		/* outputCortexM0InstrsTEST(O); */

		/* outputALFRegisterDefinitionsTEST(O); */
		/* outputALFInstrMapping(O); */

		outputRegDefALF(O);
		outputPrintInstructionALF(O);
	}

    /* case ARM::tSUBspi: { */
		/* // for each operand of the MachinInstr */
		/* //  find the regclass if reg. */
		/* //  output imm if immediate */

		/* SExpr *expr; */

		/* int index = 0; */
		/* for (auto op : MI.operands()) { */
			/* const MCOperandInfo &MCOI = MI.getDesc().OpInfo[index]; */
			/* if (MCOI.isPredicate()) */
			  /* continue; */
			/* index++; */

			/* if (op.isReg()) { */
			  /* dbgs() << TRI->getName(op.getReg()); */
			/* } else if (op.isImm()) { */
			  /* dbgs() << op.getImm(); */
			/* } */
		    /* dbgs() << "\n"; */
		/* } */

		/* if (index == 2) { */
			/* for (auto op : MI.operands()) { */
				/* const MCOperandInfo &MCOI = MI.getDesc().OpInfo[index]; */
				/* if (MCOI.isPredicate()) */
				  /* continue; */
				/* if (op.isReg()) { */
				  /* dbgs() << TRI->getName(op.getReg()); */
				/* } else if (op.isImm()) { */
				  /* dbgs() << op.getImm(); */
				/* } */
			/* } */
		/* } */

		/* dbgs() << "numops: " << (index==0 ? 0 : index-1) << "\n"; */

		/* SExpr *op1 = ctx->load(32, std::string("kek")); */
		/* SExpr *op2 = ctx->load(32, std::string("kek1")); */
    /*     SExpr *expr = ctx->sub(32, op1, op2, 0); */
		/* alfbb.addStatement("test", "test", expr); */
    /*   break; */
    /* } */


private:
	SmallVector<StringRef, 2> findInstrALFOperation(Record *R)
	{
		SmallVector<StringRef, 2> result; 

		const DAGInstruction &daginst = CGDP.getInstruction(R);
		const std::string &InstName = R->getName().str();
		auto treepattern = daginst.getPattern();
		if (treepattern) {

			/* 	// Look at specific pattern  (set <reg> (add <reg>, <other>)) */
			/* 	// and pick the operation:				 ^^^ */
			auto treepatternnode = treepattern->getOnlyTree();
			if (treepatternnode) {
				if (!treepatternnode->isLeaf()) {
					/* O << treepatternnode->getOperator()->getName() << ", "; */
					result.push_back(StringRef(treepatternnode->getOperator()->getName()));
				}
				if (treepatternnode->getNumChildren() == 2) {
					auto child = treepatternnode->getChild(1);
					if (child) {
						if (!child->isLeaf()) {
							/* O << child->getOperator()->getName(); */
							result.push_back(StringRef(child->getOperator()->getName()));
						}
					}
				}
				/* for (unsigned i = 0, e = treepatternnode->getNumChildren(); i != e; ++i) { */
				/* 	auto child = treepatternnode->getChild(i); */
				/* 	if (child) { */
				/* 		if (child->isLeaf()) */
				/* 			O << *child->getLeafValue() << ", "; */
				/* 		else */
				/* 			O << child->getOperator()->getName(); */
				/* 	} */
				/* } */
			}
		}

		return result;
	}

	bool findInstrPatternInfo(Record *R, StringRef &operation, vector<Init*> &ops)
	{
		const DAGInstruction &daginst = CGDP.getInstruction(R);
		const std::string &InstName = R->getName().str();
		auto treepattern = daginst.getPattern();
		if (treepattern) {

			/* 	// Look at specific pattern  (set <reg> (add <reg>, <other>)) */
			/* 	// and pick the operation:				 ^^^ */
			auto treepatternnode = treepattern->getOnlyTree();
			if (treepatternnode) {
				if (!treepatternnode->isLeaf()) { // set
				}
				if (treepatternnode->getNumChildren() == 2) { // e.g.: reg, (add .. .. )
					auto targetRegChild = treepatternnode->getChild(0); // reg,
					if (targetRegChild->isLeaf())
						ops.push_back(targetRegChild->getLeafValue()); //ops[0]
					auto operationChild = treepatternnode->getChild(1); // (add .. .. )
					if (operationChild) {
						if (!operationChild->isLeaf()) { 
							operation = operationChild->getOperator()->getName();  // add 

							if (operationChild->getNumChildren() == 2) {
								if (operationChild->getChild(0)->isLeaf()) {
									ops.push_back(operationChild->getChild(0)->getLeafValue()); // ops[1]
								}
								if (operationChild->getChild(1)->isLeaf()) {
									ops.push_back(operationChild->getChild(1)->getLeafValue()); // ops[2]
								}
								return true;
							}
						}
					}
				}
			}
		}

		return false;
	}

	void outputALFInstrMapping(raw_ostream &O)
	{
		std::vector<Record*> Insts = Records.getAllDerivedDefinitions("Instruction");
		for (std::vector<Record*>::iterator IC = Insts.begin(), EC = Insts.end();
				IC != EC; ++IC) {
			Record *R = *IC;
			if (R->getValueAsString("Namespace") == "TargetOpcode" ||
					R->getValueAsBit("isPseudo"))
				continue;

			const DAGInstruction &daginst = CGDP.getInstruction(R);
			const std::string &InstName = R->getName().str();
			O << InstName << ": ";
			for (auto str : findInstrALFOperation(R)) {
				O << str << ", ";
			}
			O << "ops:  ";

			vector<Record*> inOps, outOps;
			findInstrArguments(R, inOps, outOps);
			O << "in: ";
			for (auto i : inOps)
				O << i->getName() << ", "; 
			O << "out: ";
			for (auto i : outOps)
				O << i->getName() << ", "; 
			O << "\n";
		}
	}

	void outputRegDefALF(raw_ostream &O)
	{
		O <<
			"/// printInstructionALF - This method is automatically generated by tablegen\n"
			"/// from the instruction set description.\n"
			"void ARMALFWriter::regDefALF(ALFBuilder &b) {\n";

  		CodeGenRegBank &RegBank = Target.getRegBank();
		RegBank.computeDerivedInfo();

  		for (unsigned i = 0, e = RegBank.getNumNativeRegUnits(); i != e; ++i) {
			ArrayRef<const CodeGenRegister*> Roots = RegBank.getRegUnit(i).getRoots();
			assert(!Roots.empty() && "All regunits must have a root register.");
			assert(Roots.size() <= 2 && "More than two roots not supported yet.");

			const CodeGenRegister *Reg = Roots.front();
			
			// get the first one, although there could be two root regs.. (?)
			const StringRef &regName = Roots.front()->getName();
			int64_t size = 0;

			// determine offset of the register classes of Reg:
			for (const auto &RC : RegBank.getRegClasses()) {
				if (RC.getDef() && RC.contains(Reg)) {
					size = std::max(size, RC.getDef()->getValueAsInt("Alignment"));
				}
			}

			// skip is zero regwidth
			if (size == 0)
				continue;

			O << "  b.addFrame(\"" << regName << "\", " << size << ", InternalFrame);";
		}

		O << "}\n";
	}

	void outputALFRegisterDefinitionsTEST(raw_ostream &O)
	{
  		CodeGenRegBank &RegBank = Target.getRegBank();
		RegBank.computeDerivedInfo();

  		for (unsigned i = 0, e = RegBank.getNumNativeRegUnits(); i != e; ++i) {
			ArrayRef<const CodeGenRegister*> Roots = RegBank.getRegUnit(i).getRoots();
			assert(!Roots.empty() && "All regunits must have a root register.");
			assert(Roots.size() <= 2 && "More than two roots not supported yet.");

			const CodeGenRegister *Reg = Roots.front();
			
			// get the first one, although there could be two root regs.. (?)
			const StringRef &regName = getQualifiedName(Roots.front()->TheDef);
			int64_t size = 0;

			// determine offset of the register classes of Reg:
			for (const auto &RC : RegBank.getRegClasses()) {
				if (RC.getDef() && RC.contains(Reg)) {
					size = std::max(size, RC.getDef()->getValueAsInt("Alignment"));
				}
			}

			// skip is zero regwidth
			if (size == 0)
				continue;

			O << regName << "  ";

			// print the register classes 
			if (RegBank.getRegClasses().empty())
				O << "\n";
			else {
				for (const auto &RC : RegBank.getRegClasses()) {
					if (RC.getDef() && RC.contains(Reg)) {
						O << RC.getName() << ", ";
					}
				}
				O << "\n";
			}
		}
	}

	void findInstrArguments(Record *R, vector<Record*> &inOps, vector<Record*> &outOps)
	{
		const DAGInstruction &daginst = CGDP.getInstruction(R);

		for (unsigned i = 0, e = daginst.getNumResults(); i != e; ++i) {
			outOps.push_back(daginst.getResult(i));
		}
		for (unsigned i = 0, e = daginst.getNumOperands(); i != e; ++i) {
			inOps.push_back(daginst.getOperand(i));
		}
	}


	void buildCaseMItoALF(raw_ostream &O, const CodeGenInstruction *I)
	{
		/* O << "      alfbb.addStatement("; */ 
		/*   O << "\"" << I->TheDef->getName() << "\"" << ", "; */
		/*   O << "\"" << I->TheDef->getName() << "\"" << ", "; */
		/*   O << "SExpr(\"test\")" << ");\n"; */

		/* SExpr *expr; */

		/* const DAGInstruction &daginst = CGDP.getInstruction(R); */

		/* // TODO: */ 
		/* // Consider one simple pattern e.g. add $1, $2, $3. extend later. */
		/* // 1) get access to operands. Discriminate between input and output. */
		/* // 2) get access to operand values during run-time */
		/* // 3) create Expr */

		/* // for 1) use I, daginst and MI.operands ?? */

		/* // (outs) */
		/* for (unsigned i = 0, e = daginst.getNumResults(); i != e; ++i) { */
		/* 	daginst.getResult(i); // record of operand */
		/* } */
		/* // (ins) */
		/* for (unsigned i = 0, e = daginst.getNumOperands(); i != e; ++i) { */
		/* 	daginst.getOperand(i); // record of operand */
		/* } */

		/* int index = 0; */
		/* for (auto op : MI.operands()) { */
		/* 	const MCOperandInfo &MCOI = MI.getDesc().OpInfo[index]; */
		/* 	if (MCOI.isPredicate()) */
		/* 	  continue; */
		/* 	index++; */

		/* 	if (op.isReg()) { */
		/* 	  dbgs() << TRI->getName(op.getReg()); */
		/* 	} else if (op.isImm()) { */
		/* 	  dbgs() << op.getImm(); */
		/* 	} */
		/*     dbgs() << "\n"; */
		/* } */

		/* if (index == 2) { */
		/* 	for (auto op : MI.operands()) { */
		/* 		const MCOperandInfo &MCOI = MI.getDesc().OpInfo[index]; */
		/* 		if (MCOI.isPredicate()) */
		/* 		  continue; */
		/* 		if (op.isReg()) { */
		/* 		  dbgs() << TRI->getName(op.getReg()); */
		/* 		} else if (op.isImm()) { */
		/* 		  dbgs() << op.getImm(); */
		/* 		} */
		/* 	} */
		/* } */

		/* dbgs() << "numops: " << (index==0 ? 0 : index-1) << "\n"; */

		/* SExpr *op1 = ctx->load(32, std::string("kek")); */
		/* SExpr *op2 = ctx->load(32, std::string("kek1")); */
        /* SExpr *expr = ctx->sub(32, op1, op2, 0); */
		/* alfbb.addStatement("test", "test", expr); */
	}

	void outputPrintInstructionALF(raw_ostream &O)
	{

		// Get the instruction numbering.
		NumberedInstructions = Target.getInstructionsByEnumValue();

		O <<
			"/// printInstructionALF - This method is automatically generated by tablegen\n"
			"/// from the instruction set description.\n"
			"void ARMALFWriter::printInstructionALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx) {\n";

		O << "  const unsigned opcode = MI.getOpcode();\n";
	    O << "  const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();\n";
		O << "  const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();\n";
		O << "  switch (opcode) {\n";

		for (unsigned i = 0, e = NumberedInstructions.size(); i != e; ++i) {
			const CodeGenInstruction *I = NumberedInstructions[i];
			/* Record *R = I->TheDef; */
			/* if (R->getValueAsString("Namespace") == "TargetOpcode" || */
			/* 		R->getValueAsBit("isPseudo")) */
			/* 	continue; */
			if (!I->AsmString.empty() && I->TheDef->getName() != "PHI") {

				const std::string &InstName = I->TheDef->getName().str();
				O << "    case " << I->Namespace << "::" << InstName << ": {\n";



				/* // obtain ALFString */
				/* std::string ALFString = I->TheDef->getValueAsString("ALFString"); */
				/* O << "      MI.dump();\n"; */
				/* if (!ALFString.empty()) { */
				/* 	O << "      O << \"" << ALFString  << "\" << \"(\";\n"; */

      				/* O << "      int index = 0;\n"; */
				/* 	O << "      for (auto &op : MI.operands()) {\n"; */
				/* 	O << "        const MCOperandInfo &MCOI = MI.getDesc().OpInfo[index];\n"; */
				/* 	O << "        if (MCOI.isPredicate())\n"; */
				/* 	O << "          continue;\n"; */
				/* 	O << "        if (op.isReg()) {\n"; */
		  			/* O << "          if (TRI->getName(op.getReg()) != StringRef(\"CPSR\"));\n"; */
				/* 	O << "            O << \"reg:\" << TRI->getName(op.getReg()) << \",\";\n"; */
				/* 	O << "        } else if (op.isImm()) {\n"; */
				/* 	O << "          O << \"imm:\" << op.getImm() << \",\";\n"; */
				/* 	O << "        }\n"; */
      				/* O << "      index++;\n"; */
				/* 	O << "      }\n"; */
				/* 	O << "      O << \")\\n\"; \n"; */
				/* } */

				/* buildCaseMItoALF(O, I); */

				/* Record *R, StringRef &operation, SmallVector<Init*, 3> &ops */
				StringRef oper;
				vector<Init*> ops;
				if (findInstrPatternInfo(I->TheDef, oper, ops)) {

				// assume first reg is target, then next two reg or imm are operands.
				O << "      std::string targetReg;\n";
				O << "      SExpr *op1, *op2;\n";

				O << "      int index = 0;\n";
				O << "      int regOrImm_counter = 0;\n";
				O << "      for (auto &op : MI.operands()) {\n";
				O << "        const MCOperandInfo &MCOI = MI.getDesc().OpInfo[index];\n";
				O << "        if (MCOI.isPredicate())\n";
				O << "        	continue;\n";
				O << "        if (op.isReg()) {\n";
				O << "          if (TRI->getName(op.getReg()) == StringRef(\"CPSR\"))\n";
				O << "            continue;\n";
				O << "        	if (regOrImm_counter == 0)\n";
				O << "        		targetReg = TRI->getName(op.getReg());\n";
				O << "        	if (regOrImm_counter == 1)\n";
				O << "        		op1 = ctx->load(32, TRI->getName(op.getReg()));\n";
				O << "        	if (regOrImm_counter == 2)\n";
				O << "        		op2 = ctx->load(32, TRI->getName(op.getReg()));\n";
				O << "        	regOrImm_counter++;\n";
				O << "        } else if (op.isImm()) {\n";
				O << "        	if (regOrImm_counter == 1)\n";
				O << "        		op1 = ctx->dec_unsigned(32, op.getImm());\n";
				O << "        	if (regOrImm_counter == 2) \n";
				O << "        		op2 = ctx->dec_unsigned(32, op.getImm());\n";
				O << "        	regOrImm_counter++;\n";
				O << "        }\n";
				O << "        index++;\n";
				O << "      }\n";

				O << "      SExpr *expr;\n";
				if (oper == "sub") {
				O << "      expr = ctx->sub(32, op1, op2, 0);\n";
				} else if (oper == "add") {
				O << "      expr = ctx->add(32, op1, op2, 0);\n";
				}

				O << "      if (expr) {\n";
				O << "        SExpr *stor = ctx->store(ctx->address(targetReg), expr);\n";
				O << "        alfbb.addStatement(\"test\", \"test\", stor);\n";
				O << "      }\n";
				}

				O << "      break;\n";
				O << "    }\n";

				int OpIdx = 0;
				for (auto &opInfo : I->Operands.OperandList) {
					CGIOperandList::OperandInfo OpInfo = I->Operands[OpIdx];
					/* O << " " << OpInfo.PrinterMethodName << "\n"; */
					/* unsigned MIOp = OpInfo.MIOperandNo; */

					OpIdx++;
				}
			}
		}

		// Default case: unhandled opcode
		O << "    default: {\n";
		/* O << "      O << \"Unknown instr: \" << MI;\n"; */
		O << "    }\n";
		O << "  }\n";

		O << "}\n\n";
	}

	void outputCortexM3InstrsTEST(raw_ostream &O)
	{
		CodeGenTarget Target(Records);
		std::vector<Record*> Insts = Records.getAllDerivedDefinitions("Instruction");

		// derived by hand by looking at the cortex-m3 subtarget def
		std::vector<std::string> lookingFor {
			"HasV7",
			"HasV6T2",
			"HasV8MBaseline",
			"HasV6M",
			"HasV6",
			"HasV5TE",
			"HasV5T",
			"HasV4T",
			"HasV6K",
			"IsThumb",
			"IsThumb2",
			"HasDB",
			"HasDivide",
			"IsMClass",
		};

		// predicates with empty AssemblerCondString that can be skipped
		std::vector<std::string> canBeIgnored {
			"DontUseFusedMAC",
			"DontUseMovt",
			"DontUseNEONForFP",
			"DontUseNaClTrap",
			"DontUseVMOVSR",
			"GenExecuteOnly",
			"HasFastVDUP32",
			"HasFastVGETLNi32",
			"HasSlowVDUP32",
			"HasSlowVGETLNi32",
			"HasZCZ",
			"IsBE",
			"IsLE",
			"IsMachO",
			"IsNaCl",
			"IsNotMachO",
			"IsNotWindows",
			"IsThumb1Only",
			"IsWindows",
			"NoHonorSignDependentRounding",
			"NoV4T",
			"NoV6",
			"NoV6K",
			"NoV6T2",
			"NoVFP",
			"UseFPVMLx",
			"UseFusedMAC",
			"UseMovt",
			"UseMulOps",
			"UseNEONForFP",
			"UseVMOVSR",
		};

		// Construct all cases statement for each opcode
		for (std::vector<Record*>::iterator IC = Insts.begin(), EC = Insts.end();
				IC != EC; ++IC) {
			Record *R = *IC;
			if (R->getValueAsString("Namespace") == "TargetOpcode" ||
					R->getValueAsBit("isPseudo"))
				continue;

			const std::string &InstName = R->getName().str();
			const auto &preds = R->getValueAsListOfDefs( StringLiteral("Predicates") );


			bool allFound = true;
			for (const Record *pred : preds) {
				auto result = std::find(lookingFor.begin(), lookingFor.end(), pred->getName());

				/* check if pred is canBeIgnored */
				if (std::end(canBeIgnored) != std::find(canBeIgnored.begin(), canBeIgnored.end(), pred->getName()))
					continue;

				if (result != std::end(lookingFor)) {
					/* O << "  > " << pred->getName() << "\n"; */
					/* O << "     " << InstName << ": " << pred->getName() << "\n"; */
				} else 
					allFound = false;
			}

			if (allFound)
				O << InstName << "\n";

			/* O << "case " << InstName << ": {\n"; */

		}
	}

	void outputCortexM3PredicatesTEST(raw_ostream &O)
	{
		CodeGenTarget Target(Records);
		std::vector<Record*> Insts = Records.getAllDerivedDefinitions("Predicate");

		// Construct all cases statement for each opcode
		for (std::vector<Record*>::iterator IC = Insts.begin(), EC = Insts.end();
				IC != EC; ++IC) {
			Record *R = *IC;

			const std::string &InstName = R->getName().str();
			std::string AsmCondString = R->getValueAsString("AssemblerCondString");

			/* print all */ 
			/* O << InstName << ": " << (AsmCondString.empty() ? "empty" : AsmCondString) << "\n"; */
			/* print empty */ 
			if (AsmCondString.empty()) {
				O << InstName << "\n";
			}
		}
	}

	void outputCortexM3AssemblerPredicatesTEST(raw_ostream &O)
	{
		CodeGenTarget Target(Records);
		std::vector<Record*> Insts = Records.getAllDerivedDefinitions("AssemblerPredicate");
		std::vector<std::string> lookingFor {
			"ProcM3",
			"ARMv7m",
			"HasV7Ops",
			"HasV6T2Ops",
			"HasV8MBaseLineOps",
			"HasV6MOps",
			"HasV6Ops",
			"HasV5TEOps",
			"HasV5TOps",
			"HasV4TOps",
			"HasV6KOps",
			"FeatureThumb2",
			"FeatureNoARM",
			"ModeThumb",
			"FeatureDB",
			"FeatureHWDiv",
			"FeatureMClass",
		};

		// Construct all cases statement for each opcode
		for (std::vector<Record*>::iterator IC = Insts.begin(), EC = Insts.end();
				IC != EC; ++IC) {
			Record *R = *IC;

			const std::string &InstName = R->getName().str();
			std::string AsmCondString = R->getValueAsString("AssemblerCondString");

			// AsmCondString has syntax [!]F(,[!]F)*
			SmallVector<StringRef, 4> Ops;
			SplitString(AsmCondString, Ops, ",");
			assert(!Ops.empty() && "AssemblerCondString cannot be empty");

			O << R->getName() << ": ";
			for (StringRef str : Ops) {
				O << str << ", ";
			}
			O << "\n";
		}
	}

	void outputCortexM0AssemblerPredicatesTEST(raw_ostream &O)
	{
		CodeGenTarget Target(Records);
		std::vector<Record*> Insts = Records.getAllDerivedDefinitions("AssemblerPredicate");
		std::vector<std::string> lookingFor {
			"HasV6MOps",
			"HasV6Ops",
			"HasV5TEOps",
			"HasV5TOps",
			"HasV4TOps",
			"FeatureNoARM",
			"ModeThumb",
			"FeatureDB",
			"FeatureMClass",
		};

		// Construct all cases statement for each opcode
		for (std::vector<Record*>::iterator IC = Insts.begin(), EC = Insts.end();
				IC != EC; ++IC) {
			Record *R = *IC;

			const std::string &InstName = R->getName().str();
			std::string AsmCondString = R->getValueAsString("AssemblerCondString");

			// AsmCondString has syntax [!]F(,[!]F)*
			SmallVector<StringRef, 4> Ops;
			SplitString(AsmCondString, Ops, ",");
			assert(!Ops.empty() && "AssemblerCondString cannot be empty");

			O << R->getName() << ": ";
			for (StringRef str : Ops) {
				O << str << ", ";
			}
			O << "\n";
		}
	}

	void outputCortexM0InstrsTEST(raw_ostream &O)
	{
		CodeGenTarget Target(Records);
		std::vector<Record*> Insts = Records.getAllDerivedDefinitions("Instruction");

		// derived by hand by looking at the cortex-m3 subtarget def
		std::vector<std::string> lookingFor {
			"HasV6M",
			"HasV6",
			"HasV5TE",
			"HasV5T",
			"HasV4T",
			"IsThumb",
			"HasDB",
			"IsMClass",
		};

		// predicates with empty AssemblerCondString that can be skipped
		std::vector<std::string> canBeIgnored {
			"DontUseFusedMAC",
			"DontUseMovt",
			"DontUseNEONForFP",
			"DontUseNaClTrap",
			"DontUseVMOVSR",
			"GenExecuteOnly",
			"HasFastVDUP32",
			"HasFastVGETLNi32",
			"HasSlowVDUP32",
			"HasSlowVGETLNi32",
			"HasZCZ",
			"IsBE",
			"IsLE",
			"IsMachO",
			"IsNaCl",
			"IsNotMachO",
			"IsNotWindows",
			"IsThumb1Only",
			"IsWindows",
			"NoHonorSignDependentRounding",
			"NoV4T",
			"NoV6",
			"NoV6K",
			"NoV6T2",
			"NoVFP",
			"UseFPVMLx",
			"UseFusedMAC",
			"UseMovt",
			"UseMulOps",
			"UseNEONForFP",
			"UseVMOVSR",
		};

		// Construct all cases statement for each opcode
		for (std::vector<Record*>::iterator IC = Insts.begin(), EC = Insts.end();
				IC != EC; ++IC) {
			Record *R = *IC;
			if (R->getValueAsString("Namespace") == "TargetOpcode" ||
					R->getValueAsBit("isPseudo"))
				continue;

			const std::string &InstName = R->getName().str();
			const auto &preds = R->getValueAsListOfDefs( StringLiteral("Predicates") );


			bool allFound = true;
			for (const Record *pred : preds) {
				auto result = std::find(lookingFor.begin(), lookingFor.end(), pred->getName());

				/* check if pred is canBeIgnored */
				if (std::end(canBeIgnored) != std::find(canBeIgnored.begin(), canBeIgnored.end(), pred->getName()))
					continue;

				if (result != std::end(lookingFor)) {
					/* O << "  > " << pred->getName() << "\n"; */
					/* O << "     " << InstName << ": " << pred->getName() << "\n"; */
				} else 
					allFound = false;
			}

			if (allFound)
				O << InstName << "\n";

			/* O << "case " << InstName << ": {\n"; */

		}
	}
};

} // end anonymous namespace


namespace llvm {

	void EmitALFWriter(RecordKeeper &RK, raw_ostream &OS)
	{
		emitSourceFileHeader("ALF Writer Source Fragment", OS);
		ALFWriterEmitter(RK).run(OS);
	}

}
