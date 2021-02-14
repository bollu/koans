// https://en.wikipedia.org/wiki/Schreier%E2%80%93Sims_algorithm
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <assert.h>

// Let G act on X. A base of G for this action is a sequence B: [n] -> X such that
//forall i, g(b(i)) = b(i) => g = id
// So having support identity on this implies that g is identity. it's like "zariski".
// We can say that the pointwise stabilizer of B is trivial.


// Strong generating set is a generating set of the permutation subgroup. Let
// B be a base of G ⊂ Sn.  Define B[:i] = B restricted to i elements.
// Define G[i] to be pointwise stabilizer of B[:i]. A strong generating set for G
// relative to base B is a set  S ⊂ G such that:
//
// <<S>> = G
// <<S ∩ G[i]>> = G[i]
//
//
// That is, the set S, restricted correctly, generates all the G[i], where the G[i]
// are gotten by considering stabilizers of B[:i]

struct Permutation {
    // sparse table
    std::map<int, int> f_, finv_;

    public:
    Permutation(){}
    void extent(int in, int out) {
        assert(f_.count(in) == 0);
        assert(finv_.count(out) == 0);
        f_[in] = out;
        finv_[out] = in;
    }
    int f(int i) const {
        auto it = f_.find(i);
        return it == f_.end() ? i : it->second;
    }

    int finv(int i) const {
        auto it = finv_.find(i);
        return it == f_.end() ? i : it->second;
    }
};

struct OrbitTree {};
using PermutationSet  = std::set<Permutation>;
struct Group {
    int stabPoint;
    OrbitTree orbitTree;
    PermutationSet transversalSet;
    PermutationSet generatorSet;
    Group *subgroup;

    Group(int stabPoint) : stabPoint(stabPoint), subgroup(nullptr) {};
    Group *mkStabChain(const PermutationSet &genset, int *base) {
        Group *group = new Group(0);
        for (const Permutation &generator : generatorSet) {
            group->extend(generator, base);
        }
    }
    void extend(const Permutation &generator, int *base) {
        // weed out stuff. 
        if (isMember(generator)) { return; }

        // our group got bigger, but our stabilizer chain is still
        // the same?
        generatorSet.insert(generator);

        // find all orbits
        auto newTerritorySet = orbitTree.grow(generator, base);

        // permutations returned are conset representatives of the cosets of
        // our subgroup
        for (Permutation permutation : newTerritorySet) {
            transversalSet.insert(permutation)
        }

        // apply schrier's lemma
        for (Permutation cosetRepr: transversalSet) {
            for(Permutation generator: generatorSet) {
                Permutation schreirGen = calcSchreirerGenerator(cosetRepr, generator);
                if (schreirGen.isIdentity()) { continue; }
                if (!subgroup) { subgroup = new Group(stabPoint+1); }
                subgroup->extend(schreirGen, base);
            }
        }
    }
};                              
