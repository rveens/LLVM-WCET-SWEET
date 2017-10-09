#include "llvm/TableGen/Record.h"
#include "llvm/TableGen/TableGenBackend.h"
#include "CodeGenRegisters.h"
#include "CodeGenTarget.h"
#include "CodeGenInstruction.h"
#include "CodeGenDAGPatterns.h"

#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Debug.h"

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

private:
	void findTreePatternOpNames(TreePatternNode *n, std::vector<string> &output)
	{
		if (!n)
			return;

		if (!n->getName().empty())
			output.push_back(n->getName());

		if (!n->isLeaf()) {
			if (n->getNumChildren() != 0) {
				for (unsigned i = 0, e = n->getNumChildren(); i != e; ++i) {
					findTreePatternOpNames(n->getChild(i), output);
				}
			}
		}
	}

	bool findInstrPatternInfo(const CodeGenInstruction *I, StringRef &operation, vector<int> &opsMap)
	{
		const DAGInstruction &daginst = CGDP.getInstruction(I->TheDef);
		const std::string &InstName = I->TheDef->getName().str();
		auto treepattern = daginst.getPattern();

		// find the Machineinstr. indexes of the $named values of the Pattern field.
		// example:
		//
		// dag OutOperandList = (outs tGPR:$Rd, s_cc_out:$s);
		// dag InOperandList = (ins tGPR:$Rm, imm0_7:$imm3, pred:$p);
		// list<dag> Pattern = [(set tGPR:$Rd, (add tGPR:$Rm, imm0_7:$imm3))];
		//
		// The machineinstr will have the operands:
		// 	tGPR:$Rd, s_cc_out:$s, tGPR:$Rm, imm0_7:$imm3, pred:$p
		//
		// output: 
		// Rd:0
		// Rm:2
		// imm0_7:3
				
		// opsMap[index_of_pattern_$operand] = index_of_machineinstr_operand
		/* if (treepattern) { */
		/* 	auto tpn = treepattern->getOnlyTree(); */
		/* 	if (tpn) { */
		/* 		/1* dbgs() << InstName << ": "; *1/ */
		/* 		std::vector<string> patOps; */
		/* 		findTreePatternOpNames(tpn, patOps); */
		/* 		/1* for (auto str : patOps) *1/ */
		/* 		/1* 	dbgs() << str << ", "; *1/ */
		
		/* 		/1* dbgs() << "\n"; *1/ */

		/* 		/1* for (unsigned i = 0, e = I->Operands.size(); i != e; ++i) { *1/ */
		/* 		/1* 	string op = I->Operands[i].Name; *1/ */
		/* 		/1* 	dbgs() << op << ", "; *1/ */
		/* 		/1* } *1/ */
		/* 		/1* dbgs() << "\n"; *1/ */

		/* 		// loop through the full set of operands, find the index of the pattern name */
		/* 		for (int j = 0; j < patOps.size(); j++) { */
		/* 			for (unsigned i = 0, e = I->Operands.size(); i != e; ++i) { */
		/* 				string op = I->Operands[i].Name; */
		/* 				if (op == patOps[j]) { */
		/* 					opsMap.push_back(i); */
		/* 				} */
		/* 			} */
		/* 		} */
				
		/* 		/1* for (int i = 0 ; i < opsMap.size(); i++) { *1/ */
		/* 		/1* 	dbgs() << i << ": " << opsMap[i] << "\n"; *1/ */
		/* 		/1* } *1/ */
		/* 	} */
		/* } */

		if (treepattern) {

			/* 	// Look at specific pattern  (set <reg> (add <reg>, <other>)) */
			/* 	// and pick the operation:				 ^^^ */
			auto treepatternnode = treepattern->getOnlyTree();
			if (treepatternnode) {
				if (!treepatternnode->isLeaf()) { // set
				}
				if (treepatternnode->getNumChildren() == 2) { // e.g.: reg, (add .. .. )
					auto targetRegChild = treepatternnode->getChild(0); // reg,
					/* if (targetRegChild->isLeaf()) */
						//ops.push_back(targetRegChild->getLeafValue()); //ops[0]
					auto operationChild = treepatternnode->getChild(1); // (add .. .. )
					if (operationChild) {
						if (!operationChild->isLeaf()) { 
							operation = operationChild->getOperator()->getName();  // add 

							if (operationChild->getNumChildren() == 2) {
								if (operationChild->getChild(0)->isLeaf()) {
									/* ops.push_back(operationChild->getChild(0)->getLeafValue()); // ops[1] */
								}
								if (operationChild->getChild(1)->isLeaf()) {
									/* ops.push_back(operationChild->getChild(1)->getLeafValue()); // ops[2] */
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

			O << "  b.addFrame(\"" << regName << "\", " << size << ", InternalFrame);\n";
		}

		O << "}\n";
	}

	void outputPrintInstructionALF(raw_ostream &O)
	{
		// Get the instruction numbering.
		NumberedInstructions = Target.getInstructionsByEnumValue();

		O <<
			"/// printInstructionALF - This method is automatically generated by tablegen\n"
			"/// from the instruction set description.\n"
			"void ARMALFWriter::printInstructionALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label) {\n";

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


				/* Record *R, StringRef &operation, SmallVector<Init*, 3> &ops */
				StringRef oper;
				vector<int> ops;
				if (findInstrPatternInfo(I, oper, ops)) {
					const std::string &InstName = I->TheDef->getName().str();
					O << "    case " << I->Namespace << "::" << InstName << ": {\n";

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
					} else if (oper == "set") { 
						O << "      expr = ctx->add(32, op1, op2, 0);\n";
					}

					O << "      if (expr) {\n";
					O << "        SExpr *stor = ctx->store(ctx->address(targetReg), expr);\n";
					O << "        alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);\n";
					O << "      }\n";

					O << "      break;\n";
					O << "    }\n";
				}


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
		O << "        alfbb.addStatement(label, TII->getName(MI.getOpcode()), ctx->null());\n";
		O << "    }\n";
		O << "  }\n";

		O << "}\n\n";
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
