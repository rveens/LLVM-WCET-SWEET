class ARMALFPrinter : public MachineFunctionPass {
public:
	static char ID;

	ARMALFPrinter() : MachineFunctionPass(ID) {  }

	bool runOnMachineFunction(MachineFunction &MF) override;

	// Table'gen'd
	void printInstructionALF(const llvm::MachineInstr&, llvm::raw_ostream&);
};
char ARMALFPrinter::ID = 0;
