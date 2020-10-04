// https://shuangz.com/courses/pbdr-course-sg20/
#include<iostream>
using namespace std;

// even if the f(x) is discontinuous, `g(x) = integral f(x)`
// is differentible!
// Explicitly integrate over boundaries.
// d/dp (integral[x=0 to x=1] x < p ? 1 : 0.5)
// d/dp (integral[x=0 to x=p](1) + integral[x=p to x=1] (0.5))
// d/dp ((p - 0) + (0.5 - 0.5p))
// d/dp 0.5
//
// Leibniz integral rule: d/dp [integral] = continuous + discontinuous part
// continuous part + (difference of discontinuities)
// interior derivative + (boundary derivative)
// - Reynolds transport theorem!
