#include "ARMALFWriter.h"

#include <string>

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"

#include "../MCTargetDesc/ARMBaseInfo.h"

static void customCodeAfterSET(ALFStatementGroup &alfbb,
						ALFContext *ctx,
						ALFStatement *SETstatement,
						const MachineInstr &MI,
						string targetReg, 
						vector<SExpr *> operands)
{
	if (!SETstatement)
		return;

	string label = string(SETstatement->getLabel()) + "_NZCV";

	SExpr *expr_nzcv = ctx->conc(2, 30, 
		ctx->conc(1, 1, 
		  ctx->if_(1, 
		  	  ctx->s_lt(32, ctx->load(32, targetReg), ctx->dec_unsigned(32, 0)),
		  	  ctx->dec_unsigned(1, 1),
		  	  ctx->dec_unsigned(1, 0)),
		  ctx->if_(1, 
		  	  ctx->eq(32, ctx->load(32, targetReg), ctx->dec_unsigned(32, 0)),
		  	  ctx->dec_unsigned(1, 1),
		  	  ctx->dec_unsigned(1, 0))
		),
	  ctx->dec_unsigned(30, 0)
	);
	SExpr *stor_nzcv = ctx->store(ctx->address("APSR_NZCV"), expr_nzcv); 
	alfbb.addStatement(label, "setting status flags", stor_nzcv);
}

static SExpr *t_addrmode_sp_customALF(ALFContext *ctx, const TargetRegisterInfo *TRI, const MachineInstr &MI)
{
	/* tSTRspi %R0<kill>, %SP, 2, pred:14, pred:%noreg; mem:ST4[%a] */
	// make an ALFAddressExpr* using arguments 1 (SP) and 2 (imm)
	string SP = TRI->getName(MI.getOperand(1).getReg());
	auto I = MI.getOperand(2).getImm();
	// compute I*4
	SExpr *mul1 = ctx->u_mul(32, 32, ctx->dec_unsigned(32, I), ctx->dec_unsigned(32, 4));
	SExpr *mul1_sel = ctx->select(64, 0, 31, mul1);

    return ctx->list("addr")->append(32)
		->append(ctx->fref("mem"))
		->append(mul1_sel);
}

static void tADDrSPi_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* %R7<def> = tADDrSPi %SP, 0, pred:14, pred:%noreg; flags: FrameSetup */
	string SP = TRI->getName(MI.getOperand(1).getReg());
	auto I = MI.getOperand(2).getImm();

	// compute I*4
	SExpr *mul1 = ctx->u_mul(32, 32, ctx->dec_unsigned(32, I), ctx->dec_unsigned(32, 4));
	SExpr *mul1_sel = ctx->select(64, 0, 31, mul1);

	// subtract SP by mul2_sel
	SExpr *add = ctx->add(32, ctx->load(32, SP), mul1_sel);

	SExpr *stor = ctx->store(ctx->address(TRI->getName(MI.getOperand(0).getReg())), add);
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);
}

static void tADDspi_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* %SP<def,tied1> = tSUBspi %SP<tied0>, 3, pred:14, pred:%noreg; flags: FrameSetup */
	// store in SP the SP value subtracted with an operand 
	string SP = TRI->getName(MI.getOperand(1).getReg());
	auto I = MI.getOperand(2).getImm();

	// compute I*4
	SExpr *mul1 = ctx->u_mul(32, 32, ctx->dec_unsigned(32, I), ctx->dec_unsigned(32, 4));
	SExpr *mul1_sel = ctx->select(64, 0, 31, mul1);

	// subtract SP by mul2_sel
	SExpr *add = ctx->add(32, ctx->load(32, SP), mul1_sel);

	SExpr *stor = ctx->store(ctx->address(SP), add);
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);
}

static void tSUBspi_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* %SP<def,tied1> = tSUBspi %SP<tied0>, 3, pred:14, pred:%noreg; flags: FrameSetup */
	// store in SP the SP value subtracted with an operand 
	string SP = TRI->getName(MI.getOperand(1).getReg());
	auto I = MI.getOperand(2).getImm();

	// compute I*4 (or: I*32)
	SExpr *mul1 = ctx->u_mul(32, 32, ctx->dec_unsigned(32, I), ctx->dec_unsigned(32, 4));
	SExpr *mul1_sel = ctx->select(64, 0, 31, mul1);

	// subtract SP by mul1_sel
	SExpr *subtr = ctx->sub(32, ctx->load(32, SP), mul1_sel);

	SExpr *stor = ctx->store(ctx->address(SP), subtr);
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);
}

static void tPUSH_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* push {r3} is alias for: */ 
	/* str r3, [sp, #-4]! */
	/* tPUSH pred:14, pred:%noreg, %R7<kill>, %LR<kill>, %SP<imp-def>, %SP<imp-use>; flags: FrameSetup */
	// add store statements for each register starting from index 3 until size-2

	for (unsigned i = 2, NumOps = MI.getNumOperands() - 2;
			i != NumOps; ++i) {
        const MachineOperand &MO = MI.getOperand(i);
		string MO_name = TRI->getName(MO.getReg());

		// do sub sp #-4
		SExpr *SP_decr = ctx->sub(32, ctx->load(32, "SP"), ctx->dec_unsigned(32, 4));
		SExpr *subtr = ctx->store(ctx->address("SP"), SP_decr);
		alfbb.addStatement(label + "_" + MO_name + "_decrSP", "tPUSH: decrement SP by 4", subtr);
		
		// do str r3 [sp, #-4]
		SExpr *str = ctx->store(ctx->address("mem", ctx->load(32, "SP")), ctx->load(32, MO_name));
		alfbb.addStatement(label + "_" + MO_name, string("tPUSH: push {") + MO_name + " }", str);
	}
}

static void tPOP_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* push {r3} is alias for: */ 
	/* ldr r3, [sp], #4 */
	/* tPOP_RET pred:14, pred:%noreg, %R7<def>, %PC<def>, %SP<imp-def>, %SP<imp-use>, %R0<imp-use> */
	// add store statements for each register starting from index 3 until size-3

	for (unsigned i = 2, NumOps = MI.getNumOperands() - 3;
			i != NumOps; ++i) {
        const MachineOperand &MO = MI.getOperand(i);
		string MO_name = TRI->getName(MO.getReg());

		// do ldr r3 [sp]    ( store in r3 the value from RMEM at address in SP )
		SExpr *addrInSP = ctx->load(32, ctx->address("mem", ctx->load(32, "SP")));
		SExpr *ldr = ctx->store(ctx->address(MO_name), addrInSP);

		alfbb.addStatement(label + "_" + MO_name, string("tPOP: pop {") + MO_name + " }", ldr);

		// do add sp #4
		SExpr *SP_incr = ctx->add(32, ctx->load(32, "SP"), ctx->dec_unsigned(32, 4));
		SExpr *incr = ctx->store(ctx->address("SP"), SP_incr);
		alfbb.addStatement(label + "_" + MO_name + "_incrSP", "tPOP: increment SP by 4", incr);
	}
}

static void tPOP_RET_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	tPOP_customALF(MI, alfbb, ctx, label);

	// add return statement 
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), ctx->ret());
}

static void tBL_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* BL pred:14, pred:%noreg, <ga:@test>, <regmask %LR %D8 %D9 %D10 %D11 %D12 %D13 %D14 %D15 %Q4 %Q5 %Q6 %Q7 %R4 %R5 %R6 %R7 %R8 %R9 %R10 %R11 %S16 %S17 %S18 %S19 %S20 %S21 %S22 %S23 %S24 %S25 %S26 %S27 %S28 %S29 %S30 %S31 %D8_D10 %D9_D11 %D10_D12 %D11_D13 %D12_D14 %D13_D15 %Q4_Q5 %Q5_Q6 %Q6_Q7 %Q4_Q5_Q6_Q7 %R4_R5 %R6_R7 %R8_R9 %R10_R11 %D8_D9_D10 %D9_D10_D11 %D10_D11_D12 %D11_D12_D13 %D12_D13_D14 %D13_D14_D15 %D8_D10_D12 %D9_D11_D13 %D10_D12_D14 %D11_D13_D15 %D8_D10_D12_D14 %D9_D11_D13_D15 %D9_D10 %D11_D12 %D13_D14 %D9_D10_D11_D12 %D11_D12_D13_D14>, %LR<imp-def,dead>, %SP<imp-use>, %R0<imp-use>, %SP<imp-def>, %R0<imp-def> */
	// find the label in the 3ith ([2]) argument.
	string jumpLabel = MI.getOperand(2).getGlobal()->getName();
	SExpr *call = ctx->call(ctx->labelRef(jumpLabel));
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), call);
}

static void tBcc_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* tBcc <BB#3>, pred:11, pred:%CPSR<kill> */
	// build switch based on predicate
	//
	// { switch 
	//   { ... }
	//   { default }
	// } 
	// { label tBcc }
	// { jump }
	//
	// fall-through-BB
	// { label.. }
	//
	string jumpLabel = "BB#" + std::to_string(MI.getOperand(0).getMBB()->getNumber());

	string FTlabelName = "";
	// find the fall through basic block (bit of a hack)
	for (auto sucI = MI.getParent()->succ_begin() ; sucI != MI.getParent()->succ_end(); sucI++)
	{
		string sucLabel = "BB#" + std::to_string((*sucI)->getNumber());
		if (sucLabel != jumpLabel) {
			FTlabelName = sucLabel;
		}
	}

	SExpr *cond = ctx->dec_unsigned(32, 1); // TODO
	SExpr *Branch = ctx->target(ctx->dec_unsigned(32, 0), ctx->labelRef(jumpLabel));
	SExpr *Fallthrough = ctx->default_(ctx->labelRef(FTlabelName));
	SExpr *swtch = ctx->switch_(cond, Branch, Fallthrough);

	alfbb.addStatement(label, TII->getName(MI.getOpcode()), swtch);
}

#include "ARMGenALFWriter.inc"


void ARMALFWriter::extraFrames(ALFBuilder &b)
{
	/* b.addFrame("", 32, InternalFrame); */
}

void ARMALFWriter::initFrames(ALFBuilder &b)
{
	SExpr *zero = b.dec_unsigned(32, 0);

	b.addInit("APSR_NZCV", 0, zero, false);
	b.addInit("CPSR", 0, zero, false);
	b.addInit("LR" , 0, zero, false);
	b.addInit("PC" , 0, zero, false);
	b.addInit("SP" , 0, zero, false);
	b.addInit("R0" , 0, zero, false);
	b.addInit("R1" , 0, zero, false);
	b.addInit("R2" , 0, zero, false);
	b.addInit("R3" , 0, zero, false);
	b.addInit("R4" , 0, zero, false);
	b.addInit("R5" , 0, zero, false);
	b.addInit("R6" , 0, zero, false);
	b.addInit("R7" , 0, zero, false);
	b.addInit("R8" , 0, zero, false);
	b.addInit("R9" , 0, zero, false);
	b.addInit("R10", 0, zero, false);
	b.addInit("R11", 0, zero, false);
	b.addInit("R12", 0, zero, false);
}

bool ARMALFWriter::runOnMachineFunction(MachineFunction &MF)
{
	std::string Filename = "arm.alf";

	std::error_code EC;
	raw_fd_ostream File(Filename, EC, sys::fs::F_Text);

	if (EC)
		return false;

	static ALFOutput o(File, 8);
	static ALFBuilder b(o);

	static bool init = false;
	if (!init) {
		b.setBitWidths(32, 32, 32);
		b.setLittleEndian(true);

		regDefALF(b); // TableGen
		extraFrames(b);
		initFrames(b);
		init = true;
	}

	auto alffunc = b.addFunction(MF.getName(), MF.getName(), "dit is een test");
	assert(alffunc && "Error creating ALF function!");

	for (MachineBasicBlock &mbb : MF) {
		unsigned instrCounter = 0;
		string BBname = "BB#"+ std::to_string(mbb.getNumber());
		auto alfbb = alffunc->addBasicBlock(BBname, BBname);
		for (MachineInstr &mi : mbb) {
			if (mi.isCFIInstruction())
				continue;
			string labelName = BBname + ":" + std::to_string(instrCounter);
			printInstructionALF(mi, *alfbb, alffunc, labelName); // TableGen
			instrCounter++;
		}
	}
	b.writeToFile(o);
}

FunctionPass *llvm::createARMALFWriterPass() {
	return new ARMALFWriter();
}
