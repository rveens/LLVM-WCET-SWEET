#ifndef LIB_TARGET_CLP_CLPWCET_MCINSTBB_H
#define LIB_TARGET_CLP_CLPWCET_MCINSTBB_H

#include <vector>
#include <set>
#include <memory>

#include "LabelledInst.h"

using namespace std;

struct MCInstBB;

struct MCInstBB {

	vector<LabelledInst> Insts;
	shared_ptr<MCInstBB> fall_through;
	shared_ptr<MCInstBB> jump;

	set<shared_ptr<MCInstBB>> incoming;

	bool isReturn = false;
	bool isCall = false;

	bool isInitialBasicBlock()
	{
		if (!Insts.empty()) {
			if (Insts[0].origIndex == 0)
				return true;
		}
		return false;
	}
	bool isUnreachable() { return incoming.empty() && !isInitialBasicBlock(); }

	std::string getBBLabel()
	{
		if (!Insts.empty()) {
			return Insts[0].label;
		}
		return string("<nolabel>");
	}
	bool operator==(const MCInstBB& rhs)
	{
		if (Insts.size() != rhs.Insts.size())
			return false;

		// assume they have equal size now 
		for (unsigned int i = 0; i < Insts.size(); i++) {
			if (this->Insts[i] != rhs.Insts[i])
				return false;
		}
		return true;
	}

	bool operator!=(const MCInstBB& rhs)
	{
		return !this->operator==(rhs);
	}

	/* functions for iterator */
	using iterator = vector<LabelledInst>::iterator;
	using const_iterator = vector<LabelledInst>::const_iterator;
	void clear() { Insts.clear(); }
	void erase(iterator I) { Insts.erase(I); }
	size_t size() const { return Insts.size(); }
	iterator begin() { return Insts.begin(); }
	const_iterator begin() const { return Insts.begin(); }
	iterator end() { return Insts.end(); }
	const_iterator end() const { return Insts.end(); }

	iterator insert(iterator I, const LabelledInst &inst) {
		return Insts.insert(I, inst);
	}

	void walkCFG(shared_ptr<MCInstBB> bb, set<shared_ptr<MCInstBB>> &seen, std::function<void(shared_ptr<MCInstBB> &bb)> task)
	{
		if (!bb)
			return;
		for (auto seenBB : seen) {
			if (seenBB && *seenBB == *bb)
				return;
		}

		// do the thing
		task(bb);

		seen.insert(bb);

		walkCFG(bb->fall_through, seen, task);
		walkCFG(bb->jump, seen, task);
	}

};

#endif
