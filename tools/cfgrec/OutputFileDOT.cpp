#include "OutputFileDOT.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringExtras.h"

#include <algorithm>
#include <regex>


OutputFileDOT::OutputFileDOT(MCInstPrinter &_instPrinter, MCSubtargetInfo const &_STI)
		: instPrinter(_instPrinter), STI(_STI)
{

}

OutputFileDOT::~OutputFileDOT()
{

}

void OutputFileDOT::make_label(raw_fd_ostream &File, std::shared_ptr<MCInstBB> bb, string color)
{
	File << "\"" << bb->Insts[0].label << "\""; 
	File << "[label=<<TABLE bgcolor=\"grey\" cellspacing=\"0\" border=\"0\" cellborder=\"1\">";

	if (bb->isInitialBasicBlock())
		File << "<TR><TD BGCOLOR=\"blue\">" << bb->Insts[0].label << "</TD></TR>";
	else
		File << "<TR><TD BGCOLOR=\"lightblue\">" << bb->Insts[0].label << "</TD></TR>";

	// printing label of node
	MCInstBB::iterator I = bb->begin();
	for (; I != bb->end(); I++) {
		std::string s;
		llvm::raw_string_ostream ss(s);

		string idx = utohexstr(I->origIndex);
		int spaces = 3 - idx.length();
		if (spaces <0)
			spaces = 0;
		ss << idx << string(spaces, ' ');

		MCInst *inst = &I->inst;
		/* instPrinter.printInstruction(inst, ss); */
		instPrinter.printInst(inst, ss, StringRef(""), STI);

		s = ss.str();
		std::replace(s.begin(), s.end(), '\t', ' '); 
		s = std::regex_replace(s, std::regex(","), ", ");

		File << "<TR><TD>" << ss.str() << "</TD></TR>";
	}

	File << "</TABLE>>\n";
	File <<  "  shape = \"none\" color=" << color << "];\n";
}

void OutputFileDOT::make_arrow(raw_fd_ostream &File, std::shared_ptr<MCInstBB> bb_from, std::shared_ptr<MCInstBB> bb_to, string label)
{
	if (!bb_from || bb_from->Insts.empty() || bb_from->Insts[0].label.empty())
		return;
	if (!bb_to || bb_to->Insts.empty() || bb_to->Insts[0].label.empty())
		return;
	File << "\"" << bb_from->Insts[0].label << "\""
		<< "->"
		<< "\"" << bb_to->Insts[0].label << "\""
		<< " [label = \"" << label << "\"]\n";
}

void OutputFileDOT::output(std::list<shared_ptr<MCInstBB>> bblist, std::list<shared_ptr<MCInstBB>> bblist_unreachable)
{
	std::string Filename = "cfg.dot";

	std::error_code EC;
	raw_fd_ostream File(Filename, EC, sys::fs::F_Text);

	if (!EC) {
		File << "digraph g {\n";
		File << "size=\"3,2\"; ratio = fill;\n";
		File << "graph [ rankdir = \"TB\" ]\n";

		for (auto bb : bblist) {
			if (bb->isUnreachable())
				make_label(File, bb, "red");
			else if (bb->isReturn || bb->isCall)
				make_label(File, bb, "red");
			else
				make_label(File, bb, "black");
			make_arrow(File, bb, bb->jump, "jump");
			make_arrow(File, bb, bb->fall_through, "fall_through");
		}

		File << "}\n"; // end
	} else
		errs() << "  error opening file for writing!";
}
