//===-- ALFBuilder.h - Creating ALF (Artist2 Language for Flow Analysis) modules --------------===//
//
//                     Benedikt Huber, <benedikt@vmars.tuwien.ac.at>
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#ifndef __ALF_CONTEXT_H__
#define __ALF_CONTEXT_H__

#include <iostream>
#include <vector>
#include "llvm/IR/Type.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Twine.h"

#include "SExpr.h"

namespace alf {

/// Configuration for ALF generation
class ALFConfiguration {
    static const unsigned DEFAULT_POINTER_BIT_WIDTH = 32;

    /// Bits FRef, LRef and Offset take
    unsigned BitsFRef, BitsLRef, BitsOffset;

    /// Whether to use macros (always false at the moment)
    bool UseMacros;

    /// Whether this is a little-endian module
    bool IsLittleEndian;

    /// Least Addressable unit (fixed to 8 at the moment)
    unsigned LeastAddrUnit;

public:
    ALFConfiguration(unsigned leastAddrUnit) {
        setBitWidths(DEFAULT_POINTER_BIT_WIDTH,DEFAULT_POINTER_BIT_WIDTH,DEFAULT_POINTER_BIT_WIDTH);
        LeastAddrUnit = leastAddrUnit;
        UseMacros = false;
    }
    void setBitWidths(unsigned bitsLRef, unsigned bitsFRef, unsigned bitsOffset) {
        BitsLRef = bitsLRef;
        BitsFRef = bitsFRef;
        BitsOffset = bitsOffset;
    }
    bool isLittleEndian() {
        return IsLittleEndian;
    }
    void setLittleEndian(bool isLittleEndian) {
        IsLittleEndian = isLittleEndian;
    }
    /// Bits a frameref occupies
    unsigned getBitsFRef()   { return BitsFRef; }

    /// Bits a lablref occupies
    unsigned getBitsLRef()   { return BitsLRef; }

    /// Bits occupied by an offset
    unsigned getBitsOffset() { return BitsOffset; }

    /// Get Least Addressable Unit
    unsigned getLAU() { return LeastAddrUnit; }
};
// forward declaration
class ALFAddressExpr;

/// Context for creating ALF expressions
class ALFContext : public SExprContext {
    ALFConfiguration *Config;
public:
    ALFContext(ALFConfiguration* config) : Config(config ){
    }
    ALFConfiguration *getConfig() {
        return Config;
    }
    // * basic builders

    /// undefined
    SExpr* undefined(unsigned BitWidth) {
        return list("undefined")->append(BitWidth);
    }
    /// array of values
    SExpr* const_repeat(SExpr *Value, uint64_t N) {
        return list("const_repeat")->append(Value)->append(N);
    }
    /// decimal value
    SExpr* dec_unsigned(unsigned BitWidth, uint64_t Value) {
        return list("dec_unsigned")->append(BitWidth)->append(Value);
    }
    /// decimal value (signed)
    SExpr* dec_signed(unsigned BitWidth, int64_t Value) {
        if(Value < 0) {
            return list("dec_signed")->append(BitWidth)->append(list("minus")->append(-Value));
        } else {
            return list("dec_signed")->append(BitWidth)->append(Value);
        }
    }
    /// float value
    SExpr* float_val(unsigned ExpBitWidth, unsigned FracBitWidth, const APFloat& Value) {

        unsigned totalBitWidth = 1 + ExpBitWidth + FracBitWidth;
        if(Value.isInfinity() || Value.isNaN()) {
            return undefined(totalBitWidth);
        }
        SExprList *FV = list("float_val")
                ->append(ExpBitWidth)
                ->append(FracBitWidth);
        llvm::SmallString<64> buffer;
        Value.toString(buffer);

        /* if there are only digits in the buffer, we need to add a '.' */
        bool onlyDigits = true;
        for(llvm::SmallString<64>::iterator I = buffer.begin(), E = buffer.end(); I!=E; ++I) {
            if(! isdigit(*I) && *I != '-') onlyDigits=false;
        }
        if(onlyDigits) {
            buffer.append(StringRef("."));
        }

        FV->append(buffer.str().str()); // WTF?
        return FV;
    }

    /// identifier
    SExpr* identifier(const Twine& Name);

    ALFAddressExpr* address(const Twine& Name, uint64_t OffsetInBits = 0);

    ALFAddressExpr* address(const Twine& Name, SExpr *offset);

    SExpr* fref(const Twine& Name) {
        return list("fref")->append(Config->getBitsFRef())
                            ->append(identifier(Twine(Name)));
    }

    SExpr* lref(const Twine& Name) {
        return list("lref")->append(Config->getBitsLRef())
                            ->append(identifier(Twine(Name)));
    }
    // Offset of Address or Label
    // Parameter is in Bits, and converted to LAU
    SExpr* offset(unsigned OffsetInBits) {
        assert(OffsetInBits % Config->getLAU() == 0 && "Addressing Error (unaligned offset)");
        return list("dec_unsigned")
                ->append(Config->getBitsOffset())
                ->append(OffsetInBits / Config->getLAU());
    }

    // * Statements / Expressions
    SExpr* jump(const Twine &Id, uint64_t Offset = 0, uint64_t Leaving = 0) {
        return list("jump")
                ->append(labelRef(Id,Offset))
                ->append("leaving")
                ->append(Leaving);
    }
    SExpr* l_shift(unsigned BitWidth1, unsigned BitWidth2, SExpr *Op1, SExpr *Op2) {
        return list("l_shift")
                ->append(BitWidth1)
                ->append(BitWidth2)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* add(unsigned BitWidth, SExpr *Op1, SExpr *Op2, uint8_t Carry = 0) {
        return list("add")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2)
                ->append(dec_unsigned(1,Carry));
    }
    SExpr* add2(unsigned BitWidth, SExpr *Op1, SExpr *Op2, SExpr *carry) {
        return list("add")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2)
                ->append(carry);
    }
    SExpr* c_add(unsigned BitWidth, SExpr *Op1, SExpr *Op2, uint8_t Carry = 0) {
        return list("c_add")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2)
                ->append(dec_unsigned(1,Carry));
    }
    SExpr* c_add2(unsigned BitWidth, SExpr *Op1, SExpr *Op2, SExpr *carry) {
        return list("c_add")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2)
                ->append(carry);
    }
    SExpr* sub(unsigned BitWidth, SExpr *Op1, SExpr *Op2, uint8_t Carry = 1) {
        return list("sub")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2)
                ->append(dec_unsigned(1,Carry));
    }
    SExpr* c_sub(unsigned BitWidth, SExpr *Op1, SExpr *Op2, uint8_t Carry = 1) {
        return list("c_sub")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2)
                ->append(dec_unsigned(1,Carry));
    }
    SExpr* u_mul(unsigned BitWidthM, unsigned BitWidthN, SExpr *Op1, SExpr *Op2) {
        return list("u_mul")
                ->append(BitWidthM)
                ->append(BitWidthN)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* load(unsigned BitWidth, SExpr* ref) {
        return list("load")->append(BitWidth)->append(ref);
    }
    SExpr* load(unsigned BitWidth, const Twine& Fref, unsigned Offset = 0);

    SExpr* null() {
        return list("null");
    }
    SExprList* ret() {
        return list("return");
    }
    SExpr* ret(SExpr *ReturnValue) {
        return list("return")->append(ReturnValue);
    }
    SExpr* store(SExpr *Adress, SExpr *Expr) {
        return list("store")->append(Adress)->append("with")->append(Expr);
    }
    SExpr* store(std::vector<SExpr*> Addresses, std::vector<SExpr*> Exprs) {
        SExprList *Stmt = list("store");
        for(std::vector<SExpr*>::iterator I = Addresses.begin(), E = Addresses.end(); I!=E; ++I) {
            Stmt->append(*I);
        }
        Stmt->append("with");
        for(std::vector<SExpr*>::iterator I = Exprs.begin(), E = Exprs.end(); I!=E; ++I) {
            Stmt->append(*I);
        }
        return Stmt;
    }
    SExpr* labelRef(const Twine& Id, unsigned Offset = 0) {
        return list("label")->append(Config->getBitsLRef())
                             ->append(lref(Twine(Id)))
                             ->append(offset(Offset));
    }
    SExpr* if_(unsigned BitWidth, SExpr *Cond, SExpr *IfTrueStat, SExpr *IfFalseStat) {
        return list("if")
                ->append(BitWidth)
                ->append(Cond)
                ->append(IfTrueStat)
                ->append(IfFalseStat);
    }
    SExpr* neg(unsigned BitWidth, SExpr *Op1) {
        return list("neg")
                ->append(BitWidth)
                ->append(Op1);
    }
    SExpr* eq(unsigned BitWidth, SExpr *Op1, SExpr *Op2) {
        return list("eq")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* neq(unsigned BitWidth, SExpr *Op1, SExpr *Op2) {
        return list("neq")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* and_(unsigned BitWidth, SExpr *Op1, SExpr *Op2) {
        return list("and")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* or_(unsigned BitWidth, SExpr *Op1, SExpr *Op2) {
        return list("or")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* xor_(unsigned BitWidth, SExpr *Op1, SExpr *Op2) {
        return list("xor")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* s_lt(unsigned BitWidth, SExpr *Op1, SExpr *Op2) {
        return list("s_lt")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* s_ge(unsigned BitWidth, SExpr *Op1, SExpr *Op2) {
        return list("s_ge")
                ->append(BitWidth)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* conc(unsigned BitWidth1, unsigned BitWidth2, SExpr *Op1, SExpr *Op2) {
        return list("conc")
                ->append(BitWidth1)
                ->append(BitWidth2)
                ->append(Op1)
                ->append(Op2);
    }
    SExpr* select(unsigned BitWidthK, unsigned BitWidthM, unsigned BitWidthN, SExpr *Op1) {
        return list("select")
                ->append(BitWidthK)
                ->append(BitWidthM)
                ->append(BitWidthN)
                ->append(Op1);
    }

    SExpr* call(SExpr *faddr, SExpr *retval) {
        return list("call")
                ->append(faddr)
                ->append("result")
                ->append(retval);
    }
    SExpr* call(SExpr *faddr) {
        return list("call")
                ->append(faddr)
                ->append("result");
    }
    SExpr* switch_(SExpr *condition, SExpr* &target, SExpr *def) {
		return list("switch")
			->append(condition)
			->append(target)
			->append(def);
    }
    SExpr* switch_(SExpr *condition, std::vector<SExpr*> &targets, SExpr *def) {
		auto swtch = list("switch");
		swtch->append(condition);
		for (auto s : targets) {
			swtch->append(s);
		}
		return swtch->append(def);
    }
    SExpr* target(SExpr *val, SExpr *label) {
        return list("target")
                ->append(val)
                ->append(label);
    }
    SExpr* default_(SExpr *label) {
        return list("default")
                ->append(label);
    }
};

enum ALFSExprTypes { GenericSExpr = 0, ALFAddressSExpr = 1, ALFBegin = ALFAddressSExpr, ALFEnd = ALFAddressSExpr };

/// ALF specific s-expression representing an address
class ALFAddressExpr : public SExprList {
    std::string Name;
    uint64_t OffsetInBits;
public:
    ALFAddressExpr(ALFContext *Ctx, const Twine& name, uint64_t offsetInBits) :
        SExprList(Ctx), Name(name.str()), OffsetInBits(offsetInBits) {
        append("addr");
        append(Ctx->getConfig()->getBitsFRef());
        append(Ctx->fref(Twine(name)));
        append(Ctx->offset(offsetInBits));
    }
    ALFAddressExpr(ALFContext *Ctx, const Twine& name, SExpr *offset) :
        SExprList(Ctx), Name(name.str()), OffsetInBits(0) {
        append("addr");
        append(Ctx->getConfig()->getBitsFRef());
        append(Ctx->fref(Twine(name)));
        append(offset);
    }

    virtual ~ALFAddressExpr() {}

    ALFAddressExpr *withOffset(uint64_t AdditionalOffset) {
        return ((ALFContext*)getContext())->address(Twine(Name), OffsetInBits + AdditionalOffset);
    }
    /// support for isa<> and friends
    virtual unsigned getValueID() const {
        return ALFAddressSExpr;
    }
    static inline bool classof(const ALFAddressExpr *) {
        return true;
    }
    static inline bool classof(const SExpr *V) {
        return V->getValueID() == ALFAddressSExpr;
    }

};

} // end namespace alf

#endif
