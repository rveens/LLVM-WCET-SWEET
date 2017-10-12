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
	void findTreePatternLeafNames(TreePatternNode *n, std::vector<string> &output)
	{
		if (!n)
			return;

		if (!n->getName().empty())
			output.push_back(n->getName());

		if (!n->isLeaf()) {
			if (n->getNumChildren() != 0) {
				for (unsigned i = 0, e = n->getNumChildren(); i != e; ++i) {
					findTreePatternLeafNames(n->getChild(i), output);
				}
			}
		}
	}

	void findTreePatternOperatorNames(TreePatternNode *n, std::vector<string> &output)
	{
		if (!n)
			return;

		if (!n->isLeaf()) {
			if (!n->getOperator()->getName().empty())
				output.push_back(n->getOperator()->getName());

			if (n->getNumChildren() != 0) {
				for (unsigned i = 0, e = n->getNumChildren(); i != e; ++i) {
					findTreePatternOperatorNames(n->getChild(i), output);
				}
			}
		}
	}

	void findMachineInstrIndexes_ForPattern(const CodeGenInstruction *I, vector<int> &indexesForMI, vector<string> &operatorNames)
	{
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
		//
		const DAGInstruction &daginst = CGDP.getInstruction(I->TheDef);
		const std::string &InstName = I->TheDef->getName().str();
		auto treepattern = daginst.getPattern();

		// opsMap[index_of_pattern_$operand] = index_of_machineinstr_operand
		if (treepattern) {
			auto tpn = treepattern->getOnlyTree();
			if (tpn) {
				dbgs() << InstName << ": ";
				std::vector<string> patOps;
				findTreePatternLeafNames(tpn, patOps);
				for (auto str : patOps)
					dbgs() << str << ", ";

				dbgs() << "\n";

				// small test beg
				findTreePatternOperatorNames(tpn, operatorNames);
				dbgs() << "operators: ";
				for (auto str : operatorNames)
					dbgs() << str << ", ";

				dbgs() << "\n";
				// small test end

				for (unsigned i = 0, e = I->Operands.size(); i != e; ++i) {
					string op = I->Operands[i].Name;
					dbgs() << op << ", ";
				}
				dbgs() << "\n";

				// loop through the full set of operands, find the index of the pattern name
				for (int j = 0; j < patOps.size(); j++) {
					for (unsigned i = 0, e = I->Operands.size(); i != e; ++i) {
						string op = I->Operands[i].Name;
						if (op == patOps[j]) {
							indexesForMI.push_back(i);
						}
					}
				}

				for (int i = 0 ; i < indexesForMI.size(); i++) {
					dbgs() << i << ": " << indexesForMI[i] << "\n";
				}
			}
		}
	}

	bool findInstrPatternInfo(const CodeGenInstruction *I, StringRef &operation)
	{
		const DAGInstruction &daginst = CGDP.getInstruction(I->TheDef);
		const std::string &InstName = I->TheDef->getName().str();
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
	
	void make_case(raw_ostream &O, std::vector<int> indexesForMI, vector<string> operatorNames)
	{
		if (operatorNames.size() >= 2 &&
				operatorNames[0] == "set") { 
			O << "      ALFStatement *statement;\n";
			O << "      std::string targetReg;\n";
			O << "      std::vector<SExpr *> alfOps;\n";
			// one argument
			if (operatorNames[1] == "imm") {
				// assume first reg is target, then next two reg or imm are operands.
				O << "      targetReg = TRI->getName(MI.getOperand(" << indexesForMI[0] << ").getReg());\n";

				O << "      for (auto mcop : { MI.getOperand(" << indexesForMI[1] << ") } ) {\n";
				O << "        if (mcop.isReg()) {\n";
				O << "          alfOps.push_back(ctx->load(32, TRI->getName(mcop.getReg())));\n";
				O << "        } else if (mcop.isImm()) {\n";
				O << "          alfOps.push_back(ctx->dec_unsigned(32, mcop.getImm()));\n";
				O << "        }\n";
				O << "      }\n";

				O << "      SExpr *expr = alfOps[0];\n";

				O << "      SExpr *stor = ctx->store(ctx->address(targetReg), expr);\n";
				O << "      statement = alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);\n";

			// two arguments
			} else if (operatorNames[1] == "add") {
				// assume first reg is target, then next two reg or imm are operands.
				O << "      targetReg = TRI->getName(MI.getOperand(" << indexesForMI[0] << ").getReg());\n";

				O << "      for (auto mcop : { MI.getOperand(" << indexesForMI[1] << "), MI.getOperand(" << indexesForMI[2] << ") } ) {\n";
				O << "        if (mcop.isReg()) {\n";
				O << "          alfOps.push_back(ctx->load(32, TRI->getName(mcop.getReg())));\n";
				O << "        } else if (mcop.isImm()) {\n";
				O << "          alfOps.push_back(ctx->dec_unsigned(32, mcop.getImm()));\n";
				O << "        }\n";
				O << "      }\n";

				O << "      SExpr *expr = ctx->add(32, alfOps[0], alfOps[1], 0);\n";

				O << "      SExpr *stor = ctx->store(ctx->address(targetReg), expr);\n";
				O << "      statement = alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);\n";

			} else {
				O << "      goto default_label;\n";
			}
			O << "      customCodeAfterSET(alfbb, ctx, statement, targetReg, alfOps);\n";
		} else {
			O << "      goto default_label;\n";
		}
	}

	void make_NZCV(raw_ostream &O)
	{
		O << "\n";
		O << "        alfbb.addStatement(\"\", TII->getName(MI.getOpcode()), stor);\n";
		O << "        SExpr *expr_nzcv = ctx->conc(2, 30,\n";
		O << "          ctx->conc(1, 1, \n";
		O << "            ctx->if_(1, \n";
		O << "          	  ctx->s_lt(32, ctx->load(32, targetReg), ctx->dec_unsigned(32, 0)),\n";
		O << "          	  ctx->dec_unsigned(1, 1),\n";
		O << "          	  ctx->dec_unsigned(1, 0)),\n";
		O << "            ctx->if_(1, \n";
		O << "          	  ctx->eq(32, ctx->load(32, targetReg), ctx->dec_unsigned(32, 0)),\n";
		O << "          	  ctx->dec_unsigned(1, 1),\n";
		O << "          	  ctx->dec_unsigned(1, 0))\n";
		O << "          ),\n";
		O << "          ctx->dec_unsigned(30, 0)\n";
		O << "        );\n";
		O << "        SExpr *stor_nzcv = ctx->store(ctx->address(\"APSR_NZCV\"), expr_nzcv); \n";
		O << "        alfbb.addStatement(label+\"NZCV\", \"setting NZCV status flags\", stor_nzcv);\n";
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
				/* StringRef oper; */
				/* vector<int> ops; */
				/* if (findInstrPatternInfo(I, oper)) { */
				const std::string &InstName = I->TheDef->getName().str();
				O << "    case " << I->Namespace << "::" << InstName << ": {\n";

				std::vector<int> indexesForMI;
				vector<string> operatorNames;
				findMachineInstrIndexes_ForPattern(I, indexesForMI, operatorNames);

				const DAGInstruction &daginst = CGDP.getInstruction(I->TheDef);
				auto treepattern = daginst.getPattern();
				if (treepattern) {
					O << "      // ";
					treepattern->print(O);
					O << "\n";
				}
				O << "      //indexesForMI ";
				for (auto i : indexesForMI) {
					O << to_string(i) << " ";
				}
			  	O << "\n";

				O << "      //operatorNames ";
				for (auto s : operatorNames) {
					O << s << " ";
				}
			  	O << "\n";

				make_case(O, indexesForMI, operatorNames);

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
		O << "      default_label:\n";
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
