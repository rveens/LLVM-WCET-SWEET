//===-- ALFOutput.h - Module for generating ALF code --------------===//
//
//                     Benedikt Huber, <benedikt@vmars.tuwien.ac.at>
//                     Using The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This library produce ALF code, to be fed into the Swedish
// Execution Time Tool (SWEET).
//
//===----------------------------------------------------------------------===//
#ifndef __ALF_OUTPUT_H__
#define __ALF_OUTPUT_H__

#include <stdint.h>

#include "llvm/ADT/APInt.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Support/FormattedStream.h"

#include "SExpr.h"

using namespace std;
using namespace llvm;

namespace llvm {


    class ALFOutput {

      private:
          raw_ostream& Out;
          std::vector<string> NamedListStack;
          std::vector<unsigned> LayoutStack;

          unsigned MaxIndent,Indent;
          enum { LAYOUT_NONE = 0, LAYOUT_ONELINE = 1, LAYOUT_MULTILINE = 2 };
          unsigned StateLayout;

          bool UseMacros;
          unsigned LeastAddrUnit;
          unsigned BitsFRef, BitsLRef, BitsOffset;

      public:
          static const unsigned MAX_RIBBON_LENGTH = 60;

          ALFOutput(raw_ostream &o,
                    unsigned leastAddrUnit,
                    unsigned defaultBitWidth = 32)
            : Out(o) {
              StateLayout = LAYOUT_NONE;
              Indent = 0;
              MaxIndent = 40;

              UseMacros = false;
              LeastAddrUnit = leastAddrUnit;
              setBitWidths(defaultBitWidth, defaultBitWidth, defaultBitWidth);
          }

          unsigned getBitsFRef()   { return BitsFRef; }
          unsigned getBitsOffset() { return BitsOffset; }
          unsigned getLeastAddrUnit() { return LeastAddrUnit; }
          void setBitWidths(unsigned bitsLRef,unsigned bitsFRef, unsigned bitsOffset) {
              BitsLRef = bitsLRef;
              BitsFRef = bitsFRef;
              BitsOffset = bitsOffset;
          }

          raw_ostream& getOutStream() {
              return Out;
          }

          void incrementIndent() {
              ++Indent;
          }

          void decrementIndent() {
              --Indent;
          }

          void insertSpace(bool Oneline) {
              if(Oneline && StateLayout == LAYOUT_ONELINE) {
                  Out << ' ';
              } else if(StateLayout != LAYOUT_NONE) {
                  StateLayout = LAYOUT_MULTILINE;
                  Out << '\n';
                  for(unsigned i = 0; i < Indent && i < MaxIndent; ++i) Out << ' ';
              } else if(StateLayout == LAYOUT_NONE) {
                  StateLayout = LAYOUT_ONELINE;
              }
          }

          void newline() {
              Out << '\n';
          }

          /// start an S-Expression list; only use this if no specific builder instruction is available
          void startList(const string& Cmd, bool Oneline = false) {
              NamedListStack.push_back(Cmd);
              if(Oneline) {
                  LayoutStack.push_back(StateLayout);
              } else {
                  LayoutStack.push_back(LAYOUT_MULTILINE);
              }
              insertSpace(Oneline); // if ! Oneline, start new line if neccessary
              Out << "{ " << Cmd;
              StateLayout = LAYOUT_ONELINE; // put one-line elements on the same line
              ++Indent;
          }

          /// end an S-Expression; Cmd is used to validate the nesting
          void endList(const string& Cmd) {
              if(NamedListStack.empty()) {
                  errs() << "[llvm2alf] Internal Error: Detected Inconsistence when producing ALF code: TOS=[] in endList\n";
                  assert(0 && "[llvm2alf] endList: ! NamedListStack.empty");
              }
              string Top = NamedListStack.back();
              if(Top != Cmd) {
                  errs() << "[llvm2alf] Internal Error: Detected Inconsistency when producing ALF code: TOS=" << Top << ", but expected " << Cmd << '\n';
                  assert(0 && "[llvm2alf] endList:Top == Cmd");
              }
              NamedListStack.pop_back();
              --Indent;
              insertSpace(true);
              Out << "}";
              StateLayout = LayoutStack.back();
              LayoutStack.pop_back();
          }

          template<typename T>
          void atom(T Atom) {
              insertSpace(true);
              Out << Atom;
          }

          void identifier(const std::string& Ident) {
              insertSpace(true);
              Out << "\"";
              for (unsigned i = 0, e = Ident.size(); i != e; ++i) {
                  if (Ident[i] == '"' || Ident[i] == '\\') {
                      Out << "\\";
                  }
                  Out << Ident[i];
              }
              Out << "\"";
          }

          //===----------------------------------------------------------------------===//
          //                        ALF s-expressions
          //===----------------------------------------------------------------------===//

          /// macro defs to make code more readable; assumes fixed width for addresses
          void macroDefs() {
              UseMacros = false;
              // saddr x ~ addr x 0
              startList("def");
              startList("saddr",true); atom("@frefid"); endList("saddr");
              address("@frefid",0);
              endList("def");
              // slabel x ~ label x 0
              startList("def");
              startList("slabel",true); atom("@lrefid"); endList("slabel");
              labelRef("@lrefid",0);
              endList("def");
              // true ~ dec_unsigned 1 1
              startList("def");
              startList("true",true); endList("true");
              dec_unsigned(1,1);
              endList("def");
              // false = dec_unsigned 1 0
              startList("def");
              startList("false",true); endList("false");
              dec_unsigned(1,0);
              endList("def");
              UseMacros = true;
          }

          /// ALF has C-Style comments
          void comment(const Twine& comment, bool Inline = true) {
              std::string Comment = comment.str(); // XXX: probably unefficient
              if(!Inline) {
                  Out << '\n';
                  for(unsigned i = 0; i < Indent && i < MaxIndent; ++i) Out << ' ';
              } else {
                  Out << ' ';
              }
              Out << "/* ";
              for(string::const_iterator i = Comment.begin(),
                                   e = Comment.end(); i != e; ++i) {
                  if(*i == '/') {
                      Out << "\\/";
                  } else {
                      Out << *i;
                  }
                  if(*i == '\n') {
                      for(unsigned i = 0; i < Indent+2 && i < MaxIndent; ++i) Out << ' ';
                  }
              }
              Out << " */";
              if(Inline) {
                  Out << ' ';
              }
          }

          // recursive printing of SExprs
          void sexpr(alf::SExpr *SE, bool ForceLineBreak = false) {
              if(alf::SExprList* List = SE->asList()) {
                  alf::SExprList::list_iterator I = List->begin(), E = List->end();
                  if(I == E) {
                      report_fatal_error("Invalid ALF SExpr: empty list");
                  } else if(alf::SExprAtom *ListHead = (*I)->asAtom()) {
                      bool PrintInline = List->isInline() && !ForceLineBreak && List->getLength() <= MAX_RIBBON_LENGTH;
                      startList(ListHead->getValue(), PrintInline);
                      I++;
                      while(I != E) {
                          sexpr(*I++);
                      }
                      endList(ListHead->getValue());
                  } else {
                      report_fatal_error("Invalid ALF SExpr: list head not an atom");
                  }
              } else {
                  alf::SExprAtom* Atom = SE->asAtom();
                  atom(Atom->getValue());
              }
          }

          // Stuff below here will be move to ALFContext/ALFBuilder eventually
          void undefined(unsigned BitWidth) {
            startList("undefined",true);
            atom(BitWidth);
            endList("undefined");
          }

          void lauDef() {
            startList("least_addr_unit", true);
            atom(LeastAddrUnit);
            endList("least_addr_unit");
          }

          void fref(const StringRef Id, bool OneLine=true) {
              startList("fref", OneLine);
              atom(BitsFRef);
              identifier(Id);
              endList("fref");
          }

          void address(string Id, uint64_t offs = 0) {
              if(UseMacros && offs == 0) {
                  startList("!saddr", true);
                  identifier(Id);
                  endList("!saddr");
                  return;
              }
              startList("addr", true);
              atom(BitsFRef);
              fref(Id);
              offset(offs);
              endList("addr");
          }

          void lref(string Id, bool OneLine = true) {
              startList("lref", OneLine);
              atom(BitsLRef);
              identifier(Id);
              endList("lref");
          }

          void labelRef(const string& Id, uint64_t offs = 0) {
              if(UseMacros && offs == 0) {
                  startList("!slabel", true);
                  identifier(Id);
                  endList("!slabel");
                  return;
              }
              startList("label");
              atom(BitsLRef);
              lref(Id);
              offset(offs);
              endList("label");
          }

          void ref(string Id, uint64_t Offset = 0) {
              startList("ref",true);
              identifier(Id);
              offset(Offset);
              endList("ref");
          }

          // Offset of Address or Label
          // Parameter is in Bits, and converted to LAU
          void offset(uint64_t OffsetBits) {
              assert(OffsetBits % LeastAddrUnit == 0 && "Addressing Error (unaligned offset)");
              startList("dec_unsigned", true);
              atom(BitsOffset);
              atom(OffsetBits / LeastAddrUnit);
              endList("dec_unsigned");
          }

          void dec_unsigned(unsigned BitWidth, uint64_t Value) {
              startList("dec_unsigned",true);
              atom(BitWidth);
              atom(Value);
              endList("dec_unsigned");
          }

          void dec_unsigned(unsigned BitWidth, const APInt& Value) {
              startList("dec_unsigned",true);
              atom(BitWidth);
              atom(Value.toString(10, false));
              endList("dec_unsigned");
          }

          void float_val(unsigned ExpBitWidth, unsigned FracBitWidth, const APFloat& Value) {

              unsigned totalBitWidth = 1 + ExpBitWidth + FracBitWidth;
              if(Value.isInfinity() || Value.isNaN()) {
                  undefined(totalBitWidth);
                  return;
              }

              startList("float_val",true);
              atom(ExpBitWidth);
              atom(FracBitWidth);
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

              atom(buffer);
              endList("float_val");
          }

          void alloc(string Id, uint64_t Size) {
              startList("alloc");
              atom(BitsFRef);
              identifier(Id);
              if(Size == ~0ULL) {
                  atom("inf");
              } else {
                  atom(Size);
              }
              endList("alloc");
          }
    };

}

#endif
