#ifndef LIB_TARGET_CLP_CLPWCET_ILLEGALPATTERNS_H
#define LIB_TARGET_CLP_CLPWCET_ILLEGALPATTERNS_H

struct illegal_pattern {
	set<unsigned> first; 		// set of first instructions
	set<unsigned> first_units; 	// set of units of first instructions (e.g. 11 (AB))
	unsigned delay; 			// delay for e.g. fdiv
	set<unsigned> second; 		// set of second instructions after 'first' that are not allowed to happend
	set<unsigned> second_units; // set of units of first instructions (e.g. 11 (AB))

	illegal_pattern(set<unsigned> f, set<unsigned> fu, unsigned d, set<unsigned> s, set<unsigned> su)
		: first(f), first_units(fu), delay(d), second(s), second_units(su)
	{ }

	bool check(MCInstBB::iterator I, std::shared_ptr<MCInstBB> bb) 
	{
		// make a copy of I, because we are not going to increase I
		auto I_copy = I;

		// get opcode and unit from I (=I_copy atm)
		const uint8_t &opcode = (GetCLPOpCode(I_copy->inst) & 0xFF000000) >> 24; 
		const uint8_t &unit = (GetCLPOpCode(I_copy->inst) & 0x0C00000) >> 22; // 2 bits stored int a byte
		
		// check if the current instruction is any of 'first'
		//  also check the unit / activity ('first_units')
		bool found = false;
		for (auto i : first)
			for (auto u : first_units)
				if (i == opcode && u == unit) {
					found = true;
				}
		// if we did not find a possibly illegal pattern, return right here.
		if (!found)
			return false;

		// look ahead 'delay' instructions (only in BB). Use I_copy not I
		for (unsigned i = 0; I_copy != bb->end(); I_copy++, i++) {
			if (i == delay+1) // use delay+1
				break;
		}
		// if we have reached the end of the basic block, 
		// we stop searching
		if (I_copy == bb->end())
			return false;

		// Finally, we check if I_copy points to an illegal second instruction
		//  listed in 'second' having a unit from one listed in 'second_units'
		// NOTE: re-use opcode, unit, found
		const uint8_t &second_opcode = (GetCLPOpCode(I_copy->inst) & 0xFF000000) >> 24; 
		const uint8_t &second_unit = (GetCLPOpCode(I_copy->inst) & 0x0C00000) >> 22; // 2 bits stored int a byte
		found = false;
		for (auto i : second)
			for (auto u : second_units)
				if (i == second_opcode && u == second_unit) {
					found = true;
				}

		return found; // found contains our answer
	}
};

illegal_pattern row_1
{
	{ // first set of instructions
		0x1, // 0b00000001 fadd
		0x2, // 0b00000010 fsub
		0x3, // 0b00000011 fmul
	},
	{ // set of activities of first instructions
		0x3, // AB
	},
	0, // delay
	{ // second instructions
		0x4, // 0b00000100 fcomp
		0x6, // 0b00000110 intof
		0x8, // 0b00001000 ladds
		0x9, // 0b00001001 laddu
		0xa, // 0b00001010 hadds
		0xb, // 0b00001011 haddu
		0xc, // 0b00001100 hadds32
		0xd, // 0b00001101 haddu32
		0x10, // 0b00010000 lsubs
		0x11, // 0b00010001 lsubu
		0x12, // 0b00010010 hsubs
		0x13, // 0b00010011 hsubu
		0x14, // 0b00010100 hsubs32
		0x15, // 0b00010101 hsubu32
		0x18, // 0b00011000 lmulsl
		0x19, // 0b00011001 lmulsh
		0x1a, // 0b00011010 lmulul
		0x1b, // 0b00011011 lmuluh
		0x1c, // 0b00011100 hmulsl
		0x1d, // 0b00011101 hmulsh
		0x1e, // 0b00011110 hmulul
		0x1f, // 0b00011111 hmuluh
		0x20, // 0b00100000 lor
		0x21, // 0b00100001 hor
		0x28, // 0b00101000 land
		0x29, // 0b00101001 hand
		0x30, // 0b00110000 lcompl
		0x31, // 0b00110001 hcompl
		0x40, // 0b01000000 lcompl2
		0x41, // 0b01000001 hcompl2
		0x48, // 0b01001000 shifta
		0x49, // 0b01001001 shiftzero
		0x4b, // 0b01001011 shiftone
	},
	{ // set of activities of second instructions
		0x1, // A
		0x2, // B
		0x3, // AB
	}
};

illegal_pattern row_2
{
	{ // first set of instructions
		0x1, // 0b00000001 fadd
		0x2, // 0b00000010 fsub
		0x3, // 0b00000011 fmul
	},
	{ // set of activities of first instructions
		0x1, // A
	},
	0, // delay
	{ // second instructions
		0x4, // 0b00000100 fcomp
		0x6, // 0b00000110 intof
		0x8, // 0b00001000 ladds
		0x9, // 0b00001001 laddu
		0xa, // 0b00001010 hadds
		0xb, // 0b00001011 haddu
		0xc, // 0b00001100 hadds32
		0xd, // 0b00001101 haddu32
		0x10, // 0b00010000 lsubs
		0x11, // 0b00010001 lsubu
		0x12, // 0b00010010 hsubs
		0x13, // 0b00010011 hsubu
		0x14, // 0b00010100 hsubs32
		0x15, // 0b00010101 hsubu32
		0x18, // 0b00011000 lmulsl
		0x19, // 0b00011001 lmulsh
		0x1a, // 0b00011010 lmulul
		0x1b, // 0b00011011 lmuluh
		0x1c, // 0b00011100 hmulsl
		0x1d, // 0b00011101 hmulsh
		0x1e, // 0b00011110 hmulul
		0x1f, // 0b00011111 hmuluh
		0x20, // 0b00100000 lor
		0x21, // 0b00100001 hor
		0x28, // 0b00101000 land
		0x29, // 0b00101001 hand
		0x30, // 0b00110000 lcompl
		0x31, // 0b00110001 hcompl
		0x40, // 0b01000000 lcompl2
		0x41, // 0b01000001 hcompl2
		0x48, // 0b01001000 shifta
		0x49, // 0b01001001 shiftzero
		0x4b, // 0b01001011 shiftone
	},
	{ // set of activities of second instructions
		0x1, // A
		0x3, // AB
	}
};

illegal_pattern row_3
{
	{ // first set of instructions
		0x1, // 0b00000001 fadd
		0x2, // 0b00000010 fsub
		0x3, // 0b00000011 fmul
	},
	{ // set of activities of first instructions
		0x2, // B
	},
	0, // delay
	{ // second instructions
		0x4, // 0b00000100 fcomp
		0x6, // 0b00000110 intof
		0x8, // 0b00001000 ladds
		0x9, // 0b00001001 laddu
		0xa, // 0b00001010 hadds
		0xb, // 0b00001011 haddu
		0xc, // 0b00001100 hadds32
		0xd, // 0b00001101 haddu32
		0x10, // 0b00010000 lsubs
		0x11, // 0b00010001 lsubu
		0x12, // 0b00010010 hsubs
		0x13, // 0b00010011 hsubu
		0x14, // 0b00010100 hsubs32
		0x15, // 0b00010101 hsubu32
		0x18, // 0b00011000 lmulsl
		0x19, // 0b00011001 lmulsh
		0x1a, // 0b00011010 lmulul
		0x1b, // 0b00011011 lmuluh
		0x1c, // 0b00011100 hmulsl
		0x1d, // 0b00011101 hmulsh
		0x1e, // 0b00011110 hmulul
		0x1f, // 0b00011111 hmuluh
		0x20, // 0b00100000 lor
		0x21, // 0b00100001 hor
		0x28, // 0b00101000 land
		0x29, // 0b00101001 hand
		0x30, // 0b00110000 lcompl
		0x31, // 0b00110001 hcompl
		0x40, // 0b01000000 lcompl2
		0x41, // 0b01000001 hcompl2
		0x48, // 0b01001000 shifta
		0x49, // 0b01001001 shiftzero
		0x4b, // 0b01001011 shiftone
	},
	{ // set of activities of second instructions
		0x2, // B
		0x3, // AB
	}
};

illegal_pattern row_4
{
	{ // first set of instructions
		0x0, // 0b00000000 fdiv
	},
	{ // set of activities of first instructions
		0x3, // AB
	},
	7, // delay
	{ // second instructions
		0x1, // 0b00000001 fadd
		0x2, // 0b00000010 fsub
		0x3, // 0b00000011 fmul
	},
	{ // set of activities of second instructions
		0x1, // A
		0x2, // B
		0x3, // AB
	}
};

illegal_pattern row_5
{
	{ // first set of instructions
		0x0, // 0b00000000 fdiv
	},
	{ // set of activities of first instructions
		0x1, // A
	},
	7, // delay
	{ // second instructions
		0x1, // 0b00000001 fadd
		0x2, // 0b00000010 fsub
		0x3, // 0b00000011 fmul
	},
	{ // set of activities of second instructions
		0x1, // A
		0x3, // AB
	}
};

illegal_pattern row_6
{
	{ // first set of instructions
		0x0, // 0b00000000 fdiv
	},
	{ // set of activities of first instructions
		0x2, // B
	},
	7, // delay
	{ // second instructions
		0x1, // 0b00000001 fadd
		0x2, // 0b00000010 fsub
		0x3, // 0b00000011 fmul
	},
	{ // set of activities of second instructions
		0x2, // B
		0x3, // AB
	}
};

illegal_pattern row_7
{
	{ // first set of instructions
		0x0, // 0b00000000 fdiv
	},
	{ // set of activities of first instructions
		0x3, // AB
	},
	8, // delay
	{ // second instructions
		0x4, // 0b00000100 fcomp
		0x6, // 0b00000110 intof
		0x8, // 0b00001000 ladds
		0x9, // 0b00001001 laddu
		0xa, // 0b00001010 hadds
		0xb, // 0b00001011 haddu
		0xc, // 0b00001100 hadds32
		0xd, // 0b00001101 haddu32
		0x10, // 0b00010000 lsubs
		0x11, // 0b00010001 lsubu
		0x12, // 0b00010010 hsubs
		0x13, // 0b00010011 hsubu
		0x14, // 0b00010100 hsubs32
		0x15, // 0b00010101 hsubu32
		0x18, // 0b00011000 lmulsl
		0x19, // 0b00011001 lmulsh
		0x1a, // 0b00011010 lmulul
		0x1b, // 0b00011011 lmuluh
		0x1c, // 0b00011100 hmulsl
		0x1d, // 0b00011101 hmulsh
		0x1e, // 0b00011110 hmulul
		0x1f, // 0b00011111 hmuluh
		0x20, // 0b00100000 lor
		0x21, // 0b00100001 hor
		0x28, // 0b00101000 land
		0x29, // 0b00101001 hand
		0x30, // 0b00110000 lcompl
		0x31, // 0b00110001 hcompl
		0x40, // 0b01000000 lcompl2
		0x41, // 0b01000001 hcompl2
		0x48, // 0b01001000 shifta
		0x49, // 0b01001001 shiftzero
		0x4b, // 0b01001011 shiftone
	},
	{ // set of activities of second instructions
		0x1, // A
		0x2, // B
		0x3, // AB
	}
};

illegal_pattern row_8
{
	{ // first set of instructions
		0x0, // 0b00000000 fdiv
	},
	{ // set of activities of first instructions
		0x1, // A
	},
	8, // delay
	{ // second instructions
		0x4, // 0b00000100 fcomp
		0x6, // 0b00000110 intof
		0x8, // 0b00001000 ladds
		0x9, // 0b00001001 laddu
		0xa, // 0b00001010 hadds
		0xb, // 0b00001011 haddu
		0xc, // 0b00001100 hadds32
		0xd, // 0b00001101 haddu32
		0x10, // 0b00010000 lsubs
		0x11, // 0b00010001 lsubu
		0x12, // 0b00010010 hsubs
		0x13, // 0b00010011 hsubu
		0x14, // 0b00010100 hsubs32
		0x15, // 0b00010101 hsubu32
		0x18, // 0b00011000 lmulsl
		0x19, // 0b00011001 lmulsh
		0x1a, // 0b00011010 lmulul
		0x1b, // 0b00011011 lmuluh
		0x1c, // 0b00011100 hmulsl
		0x1d, // 0b00011101 hmulsh
		0x1e, // 0b00011110 hmulul
		0x1f, // 0b00011111 hmuluh
		0x20, // 0b00100000 lor
		0x21, // 0b00100001 hor
		0x28, // 0b00101000 land
		0x29, // 0b00101001 hand
		0x30, // 0b00110000 lcompl
		0x31, // 0b00110001 hcompl
		0x40, // 0b01000000 lcompl2
		0x41, // 0b01000001 hcompl2
		0x48, // 0b01001000 shifta
		0x49, // 0b01001001 shiftzero
		0x4b, // 0b01001011 shiftone
	},
	{ // set of activities of second instructions
		0x1, // A
		0x3, // AB
	}
};

illegal_pattern row_9
{
	{ // first set of instructions
		0x0, // 0b00000000 fdiv
	},
	{ // set of activities of first instructions
		0x2, // B
	},
	8, // delay
	{ // second instructions
		0x4, // 0b00000100 fcomp
		0x6, // 0b00000110 intof
		0x8, // 0b00001000 ladds
		0x9, // 0b00001001 laddu
		0xa, // 0b00001010 hadds
		0xb, // 0b00001011 haddu
		0xc, // 0b00001100 hadds32
		0xd, // 0b00001101 haddu32
		0x10, // 0b00010000 lsubs
		0x11, // 0b00010001 lsubu
		0x12, // 0b00010010 hsubs
		0x13, // 0b00010011 hsubu
		0x14, // 0b00010100 hsubs32
		0x15, // 0b00010101 hsubu32
		0x18, // 0b00011000 lmulsl
		0x19, // 0b00011001 lmulsh
		0x1a, // 0b00011010 lmulul
		0x1b, // 0b00011011 lmuluh
		0x1c, // 0b00011100 hmulsl
		0x1d, // 0b00011101 hmulsh
		0x1e, // 0b00011110 hmulul
		0x1f, // 0b00011111 hmuluh
		0x20, // 0b00100000 lor
		0x21, // 0b00100001 hor
		0x28, // 0b00101000 land
		0x29, // 0b00101001 hand
		0x30, // 0b00110000 lcompl
		0x31, // 0b00110001 hcompl
		0x40, // 0b01000000 lcompl2
		0x41, // 0b01000001 hcompl2
		0x48, // 0b01001000 shifta
		0x49, // 0b01001001 shiftzero
		0x4b, // 0b01001011 shiftone
	},
	{ // set of activities of second instructions
		0x2, // B
		0x3, // AB
	}
};

#endif
