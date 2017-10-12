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
