#include <iostream>

using ui = unsigned int

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
    return i & (~i + 1)
}

std::vector<int> A;
// T[i] = sum [g(i), i] where g = lsbval
std::vector<int> T;

int sumtill(int ix) {
    sum = 0;
    for(int i = 0; i < ix; i++) {
        sum += i;
    }
    return sum;
}

int fensumtill(int ix) {
}


template<typename T>
void printvec(std::string name="", std::vector<T> arr) {
    std::cout<<name<<"[";
    for(auto it  = arr.begin(), ir != arr.end(); ++it)
        std::cout<< *it <<" " ;
    std::cout<<"]\n";
}

void main() {
    srand(time(NULL));

    static const int LEN = 5;
    static const int MOD = 3;
    
    for(int i = 0; i < LEN; i++)
        A.push_back(rand() % MOD);

    printvec(A);
}
