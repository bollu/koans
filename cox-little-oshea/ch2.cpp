#include <iostream>
#include <vector>

static const int NVARS = 5;
const char *vars = "xyzpq";

enum Ord { LT = '<', EQ = '=', GT = '>' };
struct Monomial {
  int pws[NVARS];

  Monomial(int pws[NVARS]) {
    for(int i = 0; i < NVARS; ++i) {
      this->pws[i] = pws[i];
    }
  }

  Ord cmp(const Monomial &other) {
    for(int i = 0; i < NVARS; ++i) {
      if (pws[i] > other.pws[i]) { return Ord::GT; }
      if (pws[i] < other.pws[i]) { return Ord::LT; }
    }
    return Ord::EQ;
  }

  bool operator < (const Monomial &other) {
    return cmp(other) == Ord::LT;
  }
};

struct Term {
  Monomial m; int c;
  Term(Monomial m) : m(m), c(1) {};
  bool operator < (const Term &other) {
    Ord mord = m.cmp(other.m);
    return mord == Ord::LT || (mord == Ord::EQ && c < other.c);  
  }
};

struct Polynomial {
  std::vector<Term> terms;
  void normlize() {
  }

};

int main() {
  return 0;
};
