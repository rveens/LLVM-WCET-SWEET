namespace {

	class ARMALFWriter : public MachineFunctionPass {
		public:
			static char ID;

			ARMALFWriter() : MachineFunctionPass(ID) {  }

			bool runOnMachineFunction(MachineFunction &MF) override;

			// Table'gen'd
			void printInstructionALF(const llvm::MachineInstr&, llvm::raw_ostream&);
	};
	char ARMALFWriter::ID = 0;
}
