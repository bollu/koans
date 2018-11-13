# https://www.topcoder.com/community/competitive-programming/tutorials/binary-indexed-trees/
import random
# returns the number i with only the LSB set
# a = a|1|0^b
# ~a = (~a)|0|(1^b)
# ~a+1 = (~a)|1|(0^b)
# (~a+1)&a = (a&~a)|(1&1)|(0^b&0^b)
# (~a+1)&a = [000<length a>000]|1|0^b
def val_of_set_lsb(i):
    v = i & (~i + 1)
    print("val of set lsb(%d|%d) = %d|%d" % (i, "{0:b}".format(i), v, "{0:b}".format(v)))
    return v

ALEN = 5
A = [random.randint(0, 4) for _ in range(ALEN)]

# Return the begin index for which slot "i" of the tree
# is responsible for
def g(i):
    i - val_of_set_lsb(i) + 1
    pass


# T[i] = sum_{j=g(i)}^i A[j]
T = [0 for _ in range(ALEN)]


# add up elements in range [0;r]
# fensum [0;r] =  fold + [0;r] =
#              (fold + [0;(g(r) - 1)]) +  (fold + [(g(r));r] )
#           =  fensum (g(r) - 1) + t(r)
# fensum :: ix -> i
def fensum(r):
    if r == 0:
        return A[0]
    return T[r] + fensum(g(r) - 1)

# add value delta to the element at ix
# find all T's that contain ix
# T[i] = sum_{j=g(i)}^i A[j]
# Find all [g(i), i] that contain ix
# note that we need | ix \in [g(i), i] | so that means that we **don't need to 
# consider** i < ix.
# If a number is odd, we add +1, +2, +4, ...
# If a number is even, we add +2, +4, ...

# If a number is odd, it's bit representation will be:
# [as]1[0s]1
# This + 1 will give us
# [as]1[0s]0
# now, we know that any even number will cover the number right before it,
# so this number is definitely covers the original odd number
# 
# Next, notice that if we have an even number
# [as]1[0s], the next number that will cover it will be the number with
# the first 1 set to 0, and the rest incremented.
# Eg: 12 = 8 + 4 =01100 +
#                 00100
#                 v-v--
#                 10000 = 16
# 
# This will cover, because 16-4 = 12
# So in some sense, we keep looking for closest powers of 2 to cover our
# number. So, in general, it will look like:
# odd number -> even number -> closest greater power of 2 -> closest greater power of 2 -> ...
# 5 -> 6 -> 8 -> 16 -> ...
#
def feninc(ix, delta):
    while (ix < ALEN):
        T[ix] += delta
        ix = ix + val_of_set_lsb(ix)


def naivesum(r):
    sum = 0
    for i in range(0, r):
        s += A[i]
    return sum


def feninit():
    for i in range(ALEN): T[i] = 0
    for (ix, a) in enumerate(A):
        feninc(ix, a)

if __name__ == "__main__":
    print("A:")
    print(A)
    print("--")

    feninit()

    for _ in range(100):
        r = random.randint(0, length(A))
        fs = fensum(r)
        sref = naivesum(r)

        print("sum(fenwick): %d" % fs)
        print("sum(reference) %d" % sref)

        assert(fs == sref)
