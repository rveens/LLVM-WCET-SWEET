#include "ARMALFWriter.h"

#include <string>

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/IR/Module.h"

#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "ARMConstantPoolValue.h"


#include "../MCTargetDesc/ARMBaseInfo.h"

#define GET_ALF_HEADERS
#include "ARMGenALFWriter.inc"

// custom leaf nodes
static SExpr *t_addrmode_sp_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* tSTRspi %R0<kill>, %SP, 2, pred:14, pred:%noreg; mem:ST4[%a] */
	// make an ALFAddressExpr* using arguments 1 (SP) and 2 (imm)
	string SP = TRI->getName(MI.getOperand(1).getReg());
	auto I = MI.getOperand(2).getImm();
	// compute offset = I*4*8
	SExpr *offset = ctx->dec_unsigned(32, I*4*8);
	SExpr *SP_times_8 = ctx->select(64, 0, 31, ctx->u_mul(32, 32, ctx->load(32, SP), ctx->dec_unsigned(32, 8)));
	SExpr *add = ctx->add(32, SP_times_8, offset, 0);

    return ctx->list("addr")->append(32)
		->append(ctx->fref("mem"))
		->append(add);
}

static SExpr *t_addrmode_rr_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* tSTRr %R4<kill>, %R0, %R3<kill>, pred:14, pred:%noreg; mem:ST4[%arrayidx12] dbg:insertsort.c:76:7 */
	/* %R3<def> = tLDRr %R0, %R2, pred:14, pred:%noreg; mem:LD4[%arrayidx] dbg:insertsort.c:70:14 */

	// make an ALFAddressExpr* using arguments 1 (SP) and 2 (imm)
	string Reg = TRI->getName(MI.getOperand(1).getReg());
	string RegOff = TRI->getName(MI.getOperand(2).getReg());

	// compute offset = RegOff*8
	SExpr *offset = ctx->select(64, 0, 31, ctx->u_mul(32, 32, ctx->load(32, RegOff), ctx->dec_unsigned(32, 8)));

	SExpr *bytes_to_bits = ctx->select(64, 0, 31, ctx->u_mul(32, 32, ctx->load(32, Reg), ctx->dec_unsigned(32, 8)));
	SExpr *add = ctx->add(32, bytes_to_bits, offset, 0);

    return ctx->list("addr")->append(32)
		->append(ctx->fref("mem"))
		->append(add);
}

static SExpr *t_addrmode_is2_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* %R0<def> = tLDRHi %R4<kill>, 0, pred:14, pred:%noreg; mem:LD2[getelementptr inbounds ([64 x i16], [64 x i16]* @block, i64 0, i64 0)](align=16)(dereferenceable) dbg:fdtc.c:243:10*/

	// make an ALFAddressExpr* using arguments 1 (R4) and 2 (imm)
	string SP = TRI->getName(MI.getOperand(1).getReg());
	auto I = MI.getOperand(2).getImm();
	// compute offset = I*8
	SExpr *offset = ctx->dec_unsigned(32, I*8*2);
	SExpr *Reg_times_8 = ctx->select(64, 0, 31, ctx->u_mul(32, 32, ctx->load(32, SP), ctx->dec_unsigned(32, 8)));
	SExpr *add = ctx->add(32, Reg_times_8, offset, 0);

    return ctx->list("addr")->append(32)
		->append(ctx->fref("mem"))
		->append(add);
}

/* static SExpr *t_addrmode_is4_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label) */
/* { */
/* 	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo(); */
/* 	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo(); */

/*   	/1* tSTRi %R1<kill>, %R0, 0, pred:14, pred:%noreg; mem:ST4[getelementptr inbounds ([11 x i32], [11 x i32]* @a, i64 0, i64 0)](align=16) dbg:insertsort.c:58:8 *1/ */
	
/* 	/1* string Reg = TRI->getName(MI.getOperand(1).getReg()); *1/ */
/* 	/1* auto I = MI.getOperand(2).getImm(); *1/ */
/* 	/1* // compute offset = I*4*8 *1/ */
/* 	/1* SExpr *offset = ctx->dec_unsigned(32, I*4*8); *1/ */
/* 	/1* SExpr *SP_times_8 = ctx->select(64, 0, 31, ctx->u_mul(32, 32, ctx->load(32, "SP"), ctx->dec_unsigned(32, 8))); *1/ */
/* 	/1* SExpr *add = ctx->add(32, SP_times_8, offset, 0); *1/ */

/*     return ctx->list("addr")->append(32) */
/* 		->append(ctx->fref("mem")) */
/* 		->append(add); */
/* } */

// custom operator nodes
static SExpr *ARMcmp_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* tCMPi8 %R0<kill>, 56, pred:14, pred:%noreg, %CPSR<imp-def> */
	string Rn = TRI->getName(MI.getOperand(0).getReg());
	SExpr *arg2_sexpr;
	auto arg2 = MI.getOperand(1);
	if (arg2.isImm()) {
		arg2_sexpr = ctx->dec_unsigned(32, arg2.getImm());
	} else if (arg2.isReg()) {
		arg2_sexpr = ctx->load(32, TRI->getName(arg2.getReg()));
	}

	SExpr *csub = ctx->c_sub(32, ctx->load(32, Rn), arg2_sexpr, 1);
	SExpr *output = ctx->sub(32, ctx->load(32, Rn), arg2_sexpr, 1);

	// overflow on substract
	// overflow if pos - neg = neg
	// overflow if neg - pos = pos
	SExpr *arg1_pos = ctx->s_ge(32, ctx->load(32, Rn), ctx->dec_unsigned(32, 0));
	SExpr *arg2_pos = ctx->s_ge(32, arg2_sexpr, ctx->dec_unsigned(32, 0));
	SExpr *output_pos = ctx->s_ge(32, output, ctx->dec_unsigned(32, 0));
	SExpr *arg1_neg = ctx->s_lt(32, ctx->load(32, Rn), ctx->dec_unsigned(32, 0));
	SExpr *arg2_neg = ctx->s_lt(32, arg2_sexpr, ctx->dec_unsigned(32, 0));
	SExpr *output_neg = ctx->s_lt(32, output, ctx->dec_unsigned(32, 0));
	SExpr *V = ctx->or_(1,
			ctx->and_(1, ctx->and_(1, arg1_pos, arg2_neg), output_neg), 
			ctx->and_(1, ctx->and_(1, arg1_neg, arg2_pos), output_pos)
			);

	SExpr *expr_nzcv = ctx->conc(4, 28, 
		ctx->conc(2, 2, 
			ctx->conc(1, 1, 
			  ctx->s_lt(32, output, ctx->dec_unsigned(32, 0)),
			  ctx->eq(32, output, ctx->dec_unsigned(32, 0))
			),
			ctx->conc(1, 1, 
				csub,
				V
			)
		),
		ctx->dec_unsigned(28, 0)
	);
	SExpr *stor_nzcv = ctx->store(ctx->address("CPSR"), expr_nzcv); 

	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor_nzcv);
}

// custom instructions
static void tADDrSPi_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* %R7<def> = tADDrSPi %SP, 0, pred:14, pred:%noreg; flags: FrameSetup */
	string SP = TRI->getName(MI.getOperand(1).getReg());
	auto I = MI.getOperand(2).getImm();

	// compute I*4
	SExpr *offset = ctx->dec_unsigned(32, I*4);

	// incr SP by offset
	SExpr *add = ctx->add(32, ctx->load(32, SP), offset);

	SExpr *stor = ctx->store(ctx->address(TRI->getName(MI.getOperand(0).getReg())), add);
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);
}

static void tADDspi_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* %SP<def,tied1> = tSUBspi %SP<tied0>, 3, pred:14, pred:%noreg; flags: FrameSetup */
	// store in SP the SP value added with an operand 
	string SP = TRI->getName(MI.getOperand(1).getReg());
	auto I = MI.getOperand(2).getImm();

	// compute I*4
	SExpr *offset = ctx->dec_unsigned(32, I*4);

	// subtract SP by mul2_sel
	SExpr *add = ctx->add(32, ctx->load(32, SP), offset);

	SExpr *stor = ctx->store(ctx->address(SP), add);
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);
}

static void tSUBi_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

  /* %R0<def,tied2>, %CPSR<def,dead> = tSUBi8 %R0<kill,tied0>, 10, pred:14, pred:%noreg; dbg:janne_complex.c:45:13 */
	string R = TRI->getName(MI.getOperand(0).getReg());
	string Rop1 = TRI->getName(MI.getOperand(2).getReg());
	auto op2 = MI.getOperand(3).getImm();

	SExpr *sub = ctx->sub(32, ctx->load(32, Rop1), ctx->dec_signed(32, op2));
	SExpr *sub_c = ctx->c_sub(32, ctx->load(32, Rop1), ctx->dec_signed(32, op2));

	SExpr *stor = ctx->store(ctx->address(R), sub);
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);

	SExpr *stor_condflags = calcNZCV(ctx, ctx->load(32, Rop1), ctx->dec_signed(32, op2), sub, sub_c, 32, 32, 32);
	alfbb.addStatement(label + "_NZCV", "", stor_condflags);
}

static void tSUBspi_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* %SP<def,tied1> = tSUBspi %SP<tied0>, 3, pred:14, pred:%noreg; flags: FrameSetup */
	// store in SP the SP value subtracted with an operand 
	string SP = TRI->getName(MI.getOperand(1).getReg());
	auto I = MI.getOperand(2).getImm();

	// compute I*4
	SExpr *offset = ctx->dec_unsigned(32, I*4);

	// subtract SP by mul1_sel
	SExpr *subtr = ctx->sub(32, ctx->load(32, SP), offset);

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

	for (unsigned i = MI.getNumOperands() - 3, NumOps = 1;
			i != NumOps; --i) {
        const MachineOperand &MO = MI.getOperand(i);
		string MO_name = TRI->getName(MO.getReg());

		// do sub sp #-4
		SExpr *SP_decr = ctx->sub(32, ctx->load(32, "SP"), ctx->dec_unsigned(32, 4));
		SExpr *subtr = ctx->store(ctx->address("SP"), SP_decr);
		alfbb.addStatement(label + "_" + MO_name + "_decrSP", "tPUSH: decrement SP by 4", subtr);
		
		// do str r3 [sp, #-4]
		SExpr *SP_times_8 = ctx->select(64, 0, 31, ctx->u_mul(32, 32, ctx->load(32, "SP"), ctx->dec_unsigned(32, 8)));
		SExpr *str = ctx->store(ctx->address("mem", SP_times_8), ctx->load(32, MO_name));
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
	/* tPOP_RET pred:14, pred:%noreg, %R4<def>, %R5<def>, %R6<def>, %R7<def>, %PC<def>, %SP<imp-def>, %SP<imp-use>; dbg:fdtc.c:231:1 */

	// add store statements for each register starting from index 3 until size-3

	const MCInstrDesc &Desc = MI.getDesc();
    unsigned NumRegs = MI.getNumOperands() - Desc.getNumOperands() + 1;

	for (unsigned i = 2;
			i < NumRegs; ++i) {
        const MachineOperand &MO = MI.getOperand(i);
		string MO_name = TRI->getName(MO.getReg());

		// do ldr r3 [sp]    ( store in r3 the value from RMEM at address in SP )
		SExpr *SP_times_8 = ctx->select(64, 0, 31, ctx->u_mul(32, 32, ctx->load(32, "SP"), ctx->dec_unsigned(32, 8)));
		SExpr *addrInSP = ctx->load(32, ctx->address("mem", SP_times_8));
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

	// special marker for debugging:
	alfbb.addStatement(MI.getParent()->getParent()->getName() + string(":debugmarker"), "marker for reading values at the end", ctx->null());

	// add return statement 
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), ctx->ret());
}

static void tBL_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* BL pred:14, pred:%noreg, <ga:@test>, <regmask %LR %D8 %D9 %D10 %D11 %D12 %D13 %D14 %D15 %Q4 %Q5 %Q6 %Q7 %R4 %R5 %R6 %R7 %R8 %R9 %R10 %R11 %S16 %S17 %S18 %S19 %S20 %S21 %S22 %S23 %S24 %S25 %S26 %S27 %S28 %S29 %S30 %S31 %D8_D10 %D9_D11 %D10_D12 %D11_D13 %D12_D14 %D13_D15 %Q4_Q5 %Q5_Q6 %Q6_Q7 %Q4_Q5_Q6_Q7 %R4_R5 %R6_R7 %R8_R9 %R10_R11 %D8_D9_D10 %D9_D10_D11 %D10_D11_D12 %D11_D12_D13 %D12_D13_D14 %D13_D14_D15 %D8_D10_D12 %D9_D11_D13 %D10_D12_D14 %D11_D13_D15 %D8_D10_D12_D14 %D9_D11_D13_D15 %D9_D10 %D11_D12 %D13_D14 %D9_D10_D11_D12 %D11_D12_D13_D14>, %LR<imp-def,dead>, %SP<imp-use>, %R0<imp-use>, %SP<imp-def>, %R0<imp-def> */
	// find the label in the 3ith ([2]) argument.
	string jumpLabel = "ERROR";
	if (MI.getOperand(2).isGlobal())
	 	jumpLabel = MI.getOperand(2).getGlobal()->getName();
	SExpr *call = ctx->call(ctx->labelRef(jumpLabel));
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), call);
}

static void tBcc_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* tBcc <BB#3>, pred:11, pred:%CPSR<kill> */
	// build switch based on predicate
	auto jumpBB = MI.getOperand(0).getMBB();
	string jumpLabel = string(jumpBB->getParent()->getName()) + ":BB#" + std::to_string(jumpBB->getNumber());

	string FTlabelName = "";
	// find the fall through basic block (bit of a hack)
	for (auto sucI = MI.getParent()->succ_begin() ; sucI != MI.getParent()->succ_end(); sucI++)
	{
		string sucLabel = string((*sucI)->getParent()->getName()) + ":BB#" + std::to_string((*sucI)->getNumber());
		if (sucLabel != jumpLabel) {
			FTlabelName = sucLabel;
		}
	}

	// make a condition based on the ARM ARMCC:CondCodes.
	SExpr *loadN = ctx->select(32, 31, 31, ctx->load(32, "CPSR"));
	SExpr *loadZ = ctx->select(32, 30, 30, ctx->load(32, "CPSR"));
	SExpr *loadC = ctx->select(32, 29, 29, ctx->load(32, "CPSR"));
	SExpr *loadV = ctx->select(32, 28, 28, ctx->load(32, "CPSR"));
	SExpr *cond; // TODO
	switch (MI.getOperand(1).getImm()) {
		case (ARMCC::EQ):
			cond = loadZ;
			break;
		case (ARMCC::NE):
			cond = ctx->eq(1, loadZ, ctx->dec_unsigned(1, 0));
			break;
		case (ARMCC::HS):
			cond = loadC;
			break;
		case (ARMCC::LO):
			cond = ctx->eq(1, loadC, ctx->dec_unsigned(1, 0));
			break;
		case (ARMCC::MI):
			cond = loadN;
			break;
		case (ARMCC::PL):
			cond = ctx->eq(1, loadN, ctx->dec_unsigned(1, 0));
			break;
		case (ARMCC::VS):
			cond = loadV;
			break;
		case (ARMCC::VC):
			cond = ctx->eq(1, loadV, ctx->dec_unsigned(1, 0));
			break;
		case (ARMCC::HI):
			cond = ctx->and_(1, 
					ctx->eq(1, loadC, ctx->dec_unsigned(1, 1)),
					ctx->eq(1, loadZ, ctx->dec_unsigned(1, 0))
					);
			break;
		case (ARMCC::LS):
			cond = ctx->or_(1, 
					ctx->eq(1, loadC, ctx->dec_unsigned(1, 0)),
					ctx->eq(1, loadZ, ctx->dec_unsigned(1, 1))
					);
			break;
		case (ARMCC::GE):
			cond = ctx->eq(1, loadN, loadV);
			break;
		case (ARMCC::LT):
			cond = ctx->neq(1, loadN, loadV);
			break;
		case (ARMCC::GT):
			cond = ctx->and_(1, 
					ctx->eq(1, loadZ, ctx->dec_unsigned(1, 0)),
					ctx->eq(1, loadN, loadV)
					);
			break;
		case (ARMCC::LE):
			cond = ctx->or_(1, 
					ctx->eq(1, loadZ, ctx->dec_unsigned(1, 1)),
					ctx->neq(1, loadN, loadV)
					);
			break;
		case (ARMCC::AL):
			cond = ctx->dec_unsigned(1, 1);
			break;
		default:
			assert(false && "Unknown ARMCC!\n");
				break;
	}
	SExpr *Branch = ctx->target(ctx->dec_unsigned(1, 1), ctx->labelRef(jumpLabel));
	SExpr *Fallthrough = ctx->default_(ctx->labelRef(FTlabelName));
	SExpr *swtch = ctx->switch_(cond, Branch, Fallthrough);

	alfbb.addStatement(label, TII->getName(MI.getOpcode()), swtch);
}

static void tLDRpci_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* %R0<def> = tLDRpci <cp#0>, pred:14, pred:%noreg; mem:LD4[ConstantPool] */

	const MachineConstantPool *MCP = MI.getParent()->getParent()->getConstantPool();
	MachineConstantPoolEntry mcpe = MCP->getConstants()[MI.getOperand(1).getIndex()];
	const Constant *cv = mcpe.Val.ConstVal;

	SExpr *cpVal;
	if (cv->getName().empty()) {
		const APInt &aint = cv->getUniqueInteger();
		cpVal = ctx->dec_unsigned(32, aint.getZExtValue());
	} else {
		string cpString = "%" + string(cv->getName());
		cpVal = ctx->load(32, cpString);
	}
	

	/* SExpr *bytes_to_bits = ctx->select(64, 0, 31, ctx->u_mul(32, 32, cpVal, ctx->dec_unsigned(32, 8))); */

	SExpr *stor = ctx->store(ctx->address(TRI->getName(MI.getOperand(0).getReg())), cpVal);

	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);
}

static void tMOVr_customALF(const MachineInstr &MI, ALFStatementGroup &alfbb, ALFContext *ctx, string label)
{
	const TargetInstrInfo *TII = MI.getParent()->getParent()->getSubtarget().getInstrInfo();
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* %R3<def> = tMOVr %R4<kill>, pred:14, pred:%noreg */
	string target = TRI->getName(MI.getOperand(0).getReg());
	string source = TRI->getName(MI.getOperand(1).getReg());
	SExpr *load = ctx->load(32, source);

	SExpr *stor = ctx->store(ctx->address(target), load);
	alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor);
}

#include "ARMGenALFWriter.inc"


void ARMALFWriter::extraFrames(ALFBuilder &b, const MachineConstantPool *MCP)
{
	auto csts = MCP->getConstants();
	for (MachineConstantPoolEntry mcpe : csts) {
		const Constant *cv = mcpe.Val.ConstVal;
		if (!cv->getName().empty())
			b.addFrame(string("%") + cv->getName(), 32, InternalFrame);
	}

	/* SExpr *stor = ctx->store(ctx->address(target), load); */
	/* alfbb.addStatement(label, TII->getName(MI.getOpcode()), stor); */
}

void ARMALFWriter::initFrames(ALFBuilder &b, MachineFunction &MF)
{
	b.addInit("APSR_NZCV", 0, b.dec_unsigned(32, 0), false);
	b.addInit("CPSR", 0, b.dec_unsigned(32, 536871283), false);
	b.addInit("LR" , 0, b.dec_unsigned(32, 33119), false);
	b.addInit("PC" , 0, b.dec_unsigned(32, 33232), false);
	b.addInit("SP" , 0, b.dec_unsigned(32, 134217720), false);
	b.addInit("R0" , 0, b.dec_unsigned(32, 1), false);
	b.addInit("R1" , 0, b.dec_unsigned(32, 134217720), false);
	b.addInit("R2" , 0, b.dec_unsigned(32, 134217720), false);
	b.addInit("R3" , 0, b.dec_unsigned(32, 0), false);
	b.addInit("R4" , 0, b.dec_unsigned(32, 1), false);
	b.addInit("R5" , 0, b.dec_unsigned(32, 134217720), false);
	b.addInit("R6" , 0, b.dec_unsigned(32, 0), false);
	b.addInit("R7" , 0, b.dec_unsigned(32, 0), false);
	b.addInit("R8" , 0, b.dec_unsigned(32, 0), false);
	b.addInit("R9" , 0, b.dec_unsigned(32, 0), false);
	b.addInit("R10", 0, b.dec_unsigned(32, 0), false);
	b.addInit("R11", 0, b.dec_unsigned(32, 0), false);
	b.addInit("R12", 0, b.dec_unsigned(32, 0), false);


	// go through global values, if there is a constant try to set initializer
	// values in the ALF.
	MachineModuleInfo &mmi = MF.getMMI();
	const Module *m = mmi.getModule();
	auto &gvl = m->getGlobalList();
	for (auto &gv : gvl) {
		gv.dump();
		const Constant *c = gv.getInitializer();
		if (c) {
			c->dump();
			// if not array, print value
			if (c->getAggregateElement(0U) == nullptr) {
				const APInt &ap1 = c->getUniqueInteger();
				b.addInit("mem", 0, b.dec_unsigned(ap1.getBitWidth(), ap1.getZExtValue()), false);
			} else { // else its an array thing, look through elements
				Constant *c_el;
				unsigned i = 0;
				// take bitwidth (assumption: all have the same size)
				unsigned bitwidth = c->getAggregateElement(0U)->getUniqueInteger().getBitWidth();
				vector<uint64_t> values;
				while ( (c_el = c->getAggregateElement(i++)) != nullptr) {
					c_el->dump();
					const APInt &ap = c_el->getUniqueInteger();
					values.push_back(ap.getZExtValue());
					/* dbgs() << "el waarde: " <<  << "\n"; */
					/* dbgs() << "el bit-width" << ap.getBitWidth() << "\n"; */
				}
				b.addInit("mem", 0, b.dec_list(bitwidth, values), false);
			}
		}
		gv.getType()->getElementType()->dump();
		/* auto type = gv.getType()->getElementType(); */
	}
	/* Constant *c = gv->getInitializer(); */
	/* c->dump(); */
}

bool ARMALFWriter::shouldSetCondFlags(const MachineInstr &MI)
{
	const TargetRegisterInfo *TRI = MI.getParent()->getParent()->getSubtarget().getRegisterInfo();

	/* unsigned Cond = MI.getOperand(MI.findFirstPredOperandIdx()).getImm(); */
	/* string s = ARMCondCodeToString((ARMCC::CondCodes) Cond); */
	/* dbgs() << "Deze instr. heeft de ARMCC: " << s << "\n"; */

	bool setsStatusFlags = false;

	for (unsigned i = 0; i < MI.getNumOperands(); i++) {
		if (MI.getOperand(i).isReg() && 
				TRI->getName(MI.getOperand(i).getReg()) == string("CPSR")) {
			MI.print(dbgs());
			setsStatusFlags = true;
		}
	}
	return setsStatusFlags;
}

unsigned ARMALFWriter::computeBBcycles(MachineBasicBlock &mbb)
{
  const TargetInstrInfo *TII = mbb.getParent()->getSubtarget().getInstrInfo();
  const InstrItineraryData *ItinData = mbb.getParent()->getSubtarget().getInstrItineraryData();
  unsigned count = 0;

  /* for (MachineInstr &mi : mbb) { */
	  /* count += TII->getInstrLatency(ItinData, mi); */
	  /* dbgs() << "cycles van de volgende instructie: " << std::to_string(TII->getInstrLatency(ItinData, mi)); */
	  /* mi.dump(); */
  /* } */

  for (MachineInstr &mi : mbb) {
	  unsigned Opcode = mi.getOpcode();

	  switch (Opcode) {
		  case ARM::tPUSH:
			  for (unsigned i = mi.getNumOperands() - 3, NumOps = 1;
					  i != NumOps; --i) {
				  count += 1;
			  }
			  count += 1;
			  break;
		  case ARM::tPOP_RET:
			  for (unsigned i = 2, NumOps = mi.getNumOperands() - 3;
					  i != NumOps; ++i) {
				  count += 1;
			  }
			  count += 4;
			  break;
		  case ARM::tADDrr:
		  case ARM::tADDi3:
		  case ARM::tCMPi8:
		  case ARM::tCMPr:
		  case ARM::tADDrSPi:
		  case ARM::tADDspi:
		  case ARM::tSUBspi:
		  case ARM::tSUBrr:
		  case ARM::tSUBi3:
		  case ARM::tMOVi8:
		  case ARM::tMOVr:
		  case ARM::tLSLri:
		  case ARM::tLSRri:
		  case ARM::tBIC:
		  case ARM::tASRri:
		  case ARM::tMVN:
		  case ARM::tMUL:
		  case ARM::tSXTH:
		  case ARM::tAND:
			  count += 1;
			  break;
		  case ARM::tSTRHr:
		  case ARM::tSTRHi:
		  case ARM::tSTRspi:
		  case ARM::tLDRspi:
		  case ARM::tLDRBi:
		  case ARM::tLDRpci:
		  case ARM::tSTRi:
		  case ARM::tSTRr:
		  case ARM::tSTRBi:
		  case ARM::tSTRBr:
		  case ARM::tLDRi:
		  case ARM::tLDRr:
		  case ARM::tLDRSH:
		  case ARM::tLDRHi:
			  count += 2;
			  break;
		  case ARM::tBcc:
		  case ARM::tB:
		  case ARM::tBL:
		  case ARM::tBX_RET:
			  count += 4;
			  break;
		  case ARM::CFI_INSTRUCTION:
		  case ARM::DBG_VALUE:
		  case ARM::JUMPTABLE_TBB:
		  case ARM::tTBB_JT:
		  case ARM::CONSTPOOL_ENTRY:
			  count += 0;
			  break;
		  default:
			  dbgs() << "Geen cycle-count voor instr: ";
			  mi.dump();
	  }
  }
  return count;
}

bool ARMALFWriter::runOnMachineFunction(MachineFunction &MF)
{
	// ALF code for sweet
	std::string Filename = "arm.alf";

	std::error_code EC;
	raw_fd_ostream File(Filename, EC, sys::fs::F_Text);

	if (EC)
		return false;

	static ALFOutput o(File, 1);
	static ALFBuilder b(o);

	static bool init = false;
	if (!init) {
		b.setBitWidths(32, 32, 32);
		b.setLittleEndian(true);

		regDefALF(b); // TableGen
		initFrames(b, MF);
		init = true;
	}
	extraFrames(b, MF.getConstantPool());

	vector<pair<string, unsigned>> BasicBlockCycles;

	auto alffunc = b.addFunction(MF.getName(), MF.getName(), "dit is een test");
	assert(alffunc && "Error creating ALF function!");

	for (MachineBasicBlock &mbb : MF) {
		unsigned instrCounter = 0;
		string BBname = string(MF.getName()) + ":BB#" + std::to_string(mbb.getNumber());
		auto alfbb = alffunc->addBasicBlock(BBname, BBname);
		for (MachineInstr &mi : mbb) {
			if (mi.isCFIInstruction())
				continue;
			string labelName = BBname + ":" + std::to_string(instrCounter);
			printInstructionALF(mi, *alfbb, alffunc, labelName); // TableGen
			instrCounter++;
		}
		unsigned cycleCount = computeBBcycles(mbb);
		BasicBlockCycles.push_back({BBname, cycleCount});
	}

	b.writeToFile(o);

	// cycle count per basic block for SWEET
	std::string Filename2 = "arm.tdb";

	std::error_code EC2;
	raw_fd_ostream File2(Filename2, EC2, sys::fs::F_Append);

	if (EC2)
		return false;

	for (auto pr : BasicBlockCycles) {
		File2 << std::get<0>(pr) << " " << std::get<1>(pr) << "\n";
	}
}

FunctionPass *llvm::createARMALFWriterPass() {
	return new ARMALFWriter();
}
