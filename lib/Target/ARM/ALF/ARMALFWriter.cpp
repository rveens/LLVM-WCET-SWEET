#include "ARMALFWriter.h"

#include <string>

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"

static void customCodeAfterSET(ALFStatementGroup &alfbb,
						ALFContext *ctx,
						ALFStatement *SETstatement,
						string targetReg,
						std::vector<SExpr *> &operands)
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

	// compute I*4 * 8 (or: I*32)
	SExpr *mul1 = ctx->u_mul(32, 32, ctx->dec_unsigned(32, I), ctx->dec_unsigned(32, 4));
	SExpr *mul1_sel = ctx->select(64, 0, 31, mul1);

	// subtract SP by mul1_sel
	SExpr *subtr = ctx->sub(32, ctx->load(32, SP), mul1_sel);

	SExpr *stor = ctx->store(ctx->address(SP), subtr);
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);
}

static void tBX_RET_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	SExpr *loadR0 = ctx->load(32, "R0");
	SExpr *ret = ctx->ret(loadR0);

	alfbb.addStatement(label, TII->getName(MI.getOpcode()), ret);
}

#include "ARMGenALFWriter.inc"

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

	b.setBitWidths(32, 32, 32);
	b.setLittleEndian(true);

	regDefALF(b); // TableGen
	initFrames(b);

	auto alffunc = b.addFunction(MF.getName(), MF.getName(), "dit is een test");
	assert(alffunc && "Error creating ALF function!");

	for (MachineBasicBlock &mbb : MF) {
		unsigned instrCounter = 0;
		auto alfbb = alffunc->addBasicBlock(mbb.getFullName() + std::to_string(instrCounter), mbb.getFullName() + std::to_string(instrCounter));
		for (MachineInstr &mi : mbb) {
			string labelName = mbb.getFullName() + std::to_string(instrCounter);
			printInstructionALF(mi, *alfbb, alffunc, labelName); // TableGen
			instrCounter++;
		}
	}
	b.writeToFile(o);
}

FunctionPass *llvm::createARMALFWriterPass() {
	return new ARMALFWriter();
}
