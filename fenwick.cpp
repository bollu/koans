#include <assert.h>
#include <vector>
#include <iostream>
static const int LEN=100;

int A[LEN+1];
// T[i] = sum [g(i), i] where g = lsbval
int T[LEN+1];


template<typename T>
void printvec(T *arr, std::string name="" ) {
    std::cout<<name<<"[";
    for(int i = 1; i <= LEN; i++) {
        std::cout<< arr[i] <<" " ;
    }
    std::cout<<"]\n";
}

using ui = unsigned int;

// i = [as]1[0s]
// ~i = [~as]0[1s]
// 
// ~i + 1  = [~as]1[0s]
// i       = [ as]1[0s]
// &&&&&&&&&&&&&&&&&&&&
//           [ 0s]1[0s]
// Return the value of the first LSB. *not* the index. If the LSB
// at index *i* is set, return 2^i
ui lsbval(ui i) {
    int v = i & (-i);
    std::cout<<"i = " << i << " | v = " << v << "\n";
    return v;
}



int sumtill(int ix) {
    int sum = 0;
    for(int i = 0; i <= ix; i++) {
        sum += A[i];
    }
    return sum;
}

int fensumtill(int ix) {
    if (ix == 0) return 0;

    assert(ix > 0);
    // T[i] = sum [g(i)), i)]
    const ui gi = lsbval(ix);
    std::cout <<"fensumtill: ix := " << ix <<  " | gi := " << gi << "\n";
    return T[ix] + fensumtill(ix - gi);
}

// we walk the ix's, from:
// odd number -> even number -> even number -> ... -> power of 2 -> power of 2 -> ...
// 5 -> 6 -> 8 -> 16 ...
//
// This is because an even number will cover at least 1 number below it (the odd number)
//
// 6 := 0110
// 8 := 1000
//
// Is _exactly_ the value you get by the _first set LSB_.
//
// 10 := 01010
// 12 := 01100
// 16 := 10000
void fenadd(ui ix, ui delta) {
    std::cout << "fenadd(" << ix << "," << delta<<")\n";
    assert(ix >= 0);

    if (ix > LEN) {
        return;
    }

    T[ix] += delta;
    fenadd(ix + lsbval(ix), delta);
}

void feninit() {
    for(int i = 1; i <= LEN;  i++) {
        T[i] = 0;
    }

    printvec(T, "T(init)");

    int ix = 0;
    for(auto i = 1; i <= LEN; ++i) {
        fenadd(i, A[i]);
    }
}

int main() {
    srand(time(NULL));

    static const int MOD = 3;
    
    for(int i = 1; i <= LEN; i++)
        A[i] = rand() % MOD;

    printvec(A, "A");
    feninit();
    printvec(A, "A");
    printvec(T, "T");

    for(int i = 1; i <= LEN; i++) {
        int ref = sumtill(i);
        int fen = fensumtill(i);

        std::cout<<"SUMTILL("<<i<<"): ref: " << ref << " | fen: " << fen << "\n";
        assert (ref == fen);
    }
}
