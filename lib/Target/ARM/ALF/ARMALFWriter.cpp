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
	// compute load(SP) * I*4  * 8 
	// load(SP) * I
	SExpr *mul1 = ctx->u_mul(32, 32, ctx->load(32, SP), ctx->dec_unsigned(32, I));
	SExpr *mul1_sel = ctx->select(0, 31, 64, mul1);
	// * 4 * 8 
	SExpr *mul2 = ctx->u_mul(32, 32, mul1_sel, ctx->dec_unsigned(32, 32));
	SExpr *mul2_sel = ctx->select(0, 31, 64, mul2);

    return ctx->list("addr")->append(32)
		->append(ctx->fref("mem"))
		->append(mul2_sel);
}

#include "ARMGenALFWriter.inc"


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
	b.setLittleEndian(false);

	regDefALF(b); // TableGen

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
