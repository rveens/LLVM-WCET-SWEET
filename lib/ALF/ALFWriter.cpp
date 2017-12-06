#include "llvm/ALF/ALFWriter.h"

ALFWriter::ALFWriter(string Filename) :
	o(nullptr), b(nullptr), MachineFunctionPass(ID) 
{
	std::error_code EC;
	raw_fd_ostream File(Filename, EC, sys::fs::F_Text);

	o = make_shared<ALFOutput>(File, 1);
	b = make_shared<ALFBuilder>(*o);

	b->setBitWidths(32, 32, 32);
	b->setLittleEndian(true);
}

ALFWriter::~ALFWriter()
{

}

bool ALFWriter::runOnMachineFunction(MachineFunction &MF)
{
	auto alffunc = b->addFunction(MF.getName(), MF.getName(), "dit is een test");
	assert(alffunc && "Error creating ALF function!");

	extraFrames(MF.getConstantPool());

	for (MachineBasicBlock &mbb : MF) {
		unsigned instrCounter = 0;
		string BBname = makeBBALFname(MF, mbb);
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

	b->writeToFile(*o);

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

string ALFWriter::makeBBALFname(MachineFunction &MF, MachineBasicBlock &mbb)
{
	return string(MF.getName()) + ":BB#" + std::to_string(mbb.getNumber());
}
