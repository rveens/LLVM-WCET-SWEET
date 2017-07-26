#include <iostream>
#include "SExpr.h"
#include "llvm/ADT/Twine.h"
#include "llvm/ADT/SmallString.h"
namespace alf {

/// Operator << for s-expressions
std::ostream& operator<<(std::ostream& out, const SExpr& Expr) {
  Expr.print(out);
  return out;
}

SExprList* SExprList::append(const Twine& Atom) {
    return append(getContext()->atom(Atom));
}

SExprList* SExprList::append(uint64_t Atom) {
    return append(getContext()->atom(utostr(Atom)));
}

} // end namespace alf

// short test
//using namespace alf;
//SExpr* dec(SExprContext& C, unsigned V) {
//  return C.list("dec_unsigned")->append("32")->append(V);
//}
//void test1() {
//  SExprContext C;
//  std::cout << *dec(C,12) << "\n";
//}
//int test(int argc, char**argv) {
//  test1();
//  std::cout << "End of Test 1\n";
//  return 0;
//}
