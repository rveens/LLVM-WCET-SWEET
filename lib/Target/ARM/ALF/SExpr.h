#ifndef __SEXPR_H__
#define __SEXPR_H__

#include <iostream>
#include <vector>
#include "llvm/IR/Type.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/Twine.h"

using namespace llvm;

namespace alf {

// Context for creating s-expressions
class SExprContext;
class SExprList;
class SExprAtom;

// Base class for s-expressions (LISP style)
class SExpr {

  /// Context this expression was created in
  SExprContext* Ctx;

  /// Whether this expression may be printed inline
  bool Inline;

  /// Optional comment
  std::string Comment;

public:
  /// Constructor for s-expressions
  SExpr(SExprContext* ctx) : Ctx(ctx) {
  }
  /// Constructor for s-expressions
  SExpr(SExprContext* ctx, const Twine& comment) : Ctx(ctx), Comment(comment.str()) {
  }
  virtual ~SExpr() {
  }
  /// set whether the expression can be printed inline
  void setInline(bool inl = true) {
      Inline = inl;
  }
  /// whether the expression may be printed inline
  bool isInline() {
      return Inline;
  }
  /// set comment
  SExpr* setComment(const Twine& comment) {
      Comment = comment.str();
      return this;
  }
  /// get SExprContext
  SExprContext* getContext() {
    return Ctx;
  }


  /// get number of characters
  virtual unsigned getLength() const = 0;
  /// print SExpr
  virtual void print(std::ostream& out) const = 0;

  /// cast to list
  virtual SExprList* asList() { return 0; }
  /// cast to atom
  virtual SExprAtom* asAtom() { return 0; }
  /// support for isa<> and friends
  static inline bool classof(const SExpr *V) {
      return true;
  }
  /// Returns 0 for generic SExpressions
  /// Override this method in application-specific contexts
  /// to get support for isa<>, dyn_cast<> and friends
  virtual unsigned getValueID() const {
      return 0;
  }
};

/// Operator << for s-expressions
std::ostream& operator<<(std::ostream& out, const SExpr& Expr);


/// s-expression atoms (uninterpreted strings)
class SExprAtom: public SExpr {
  std::string Value;
public:
  SExprAtom(SExprContext *ctx, std::string value) : SExpr(ctx) {
    Value = value;
    setInline();
  }
  SExprAtom(SExprContext *ctx, std::string value, const Twine& comment) : SExpr(ctx,comment) {
    Value = value;
    setInline();
  }
  virtual ~SExprAtom() {
  }
  virtual SExprAtom* asAtom() {
    return this;
  }
  virtual unsigned getLength() const {
      return Value.length();
  }

  StringRef getValue() {
    return StringRef(Value);
  }

  virtual void print(std::ostream& out) const {
    out << Value;
  }
};

/// list of s-expression
class SExprList : public SExpr {
    /// List elements
    std::vector<SExpr*> Children;
    unsigned Length;
public:
    typedef std::vector<SExpr*>::iterator list_iterator;
    SExprList(SExprContext *ctx) : SExpr(ctx) {
        setInline();
        Length = 2;
    }
    SExprList(SExprContext *ctx, const Twine& comment) : SExpr(ctx,comment) {
        setInline();
        Length = 2;
    }
    virtual ~SExprList() {
    }
    virtual SExprList* asList() {
      return this;
    }
    virtual unsigned getLength() const {
        return Length;
    }
    /// ALF s-expressions always have an atom as first element of the list
    StringRef getHead() {
        assert(Children.size() > 0 && "SExpr::getHead(): empty list");
        alf::SExprAtom *ListHead = (*Children.begin())->asAtom();
        assert(ListHead && "SExpr::getHead(): first element of list is not an atom");
        return ListHead->getValue();
    }
    SExprList* append(SExpr *C) {
        Children.push_back(C);
        if(! C->isInline()) setInline(false);
        if(Length > 2) ++Length; /* Separator */
        Length += C->getLength();
        return this;
    }
    list_iterator begin() {
        return Children.begin();
    }
    list_iterator end() {
        return Children.end();
    }
    SExprList* append(const Twine& Atom);
    SExprList* append(uint64_t Atom);

    virtual void print(std::ostream& out = std::cout) const {
        out << "(";
        bool First = true;
        for(std::vector<SExpr*>::const_iterator I = Children.begin(), E = Children.end();
                I!=E;++I) {
            if(First) {
                First=false;
            } else {
                out << " ";
            }
            (*I)->print(out);
        }
        out << ")";
    }
};

/// Context for creating s-expressions (owner of allocated memory)
class SExprContext {
  protected:

    std::vector<SExpr*> Pool;

  public:
    virtual ~SExprContext() {
      for(std::vector<SExpr*>::iterator I = Pool.begin(), E = Pool.end();
          I!=E;++I) {
            delete *I;
      }
    }
    SExprAtom* atom(const Twine& Value) {
      SExprAtom *Expr = new SExprAtom(this, Value.str());
      Pool.push_back(Expr);
      return Expr;
    }
    SExprAtom* atom(uint64_t Value) {
      return atom(utostr(Value));
    }
    SExprList* list() {
      SExprList *Expr = new SExprList(this);
      Pool.push_back(Expr);
      return Expr;
    }
    SExprList* list(std::string Command) {
      SExprList *Expr = new SExprList(this);
      Pool.push_back(Expr);
      Expr->append(atom(Command));
      return Expr;
    }
};


} // end namespace alf

#endif
