[![Build Status](https://travis-ci.org/zatonovo/lambda.tools.png)](https://travis-ci.org/zatonovo/lambda.tools)

lambda.tools
============

Tools for functional programming in R

This package contains a collection of functions that facilitate modeling 
of data using a functional programming paradigm. The idea is that 
using tools that are more closely connected with the 
idioms of mathematics will make it easier to map 
the mathematical model to the software model. 

Functional programming concepts start with functions as the foundation.
Higher-order functions provide generalized machinery for operating
on data in an element-wise manner. Lambda.tools includes idiomatic
versions of the canonical higher-order functions, such as map and fold
for data structures common in R. In most languages the semantics are
limited to single-element iterations. In R it is common to work with
panel data or sliding windows, so lambda.tools introduces block
and range semantics to support these concepts, respectively. 
Hence lambda.tools defines `mapblock` and `maprange` and similar 
functions for `fold`. 

Block operations
----------------

The semantics of a block operation is that regular, continguous chunks of
data are passed to the function. Suppose a vector `x` has 12 elements.
Performing a mapblock operation with window of length 3 applies the 
specified function to the following sub-vectors: `x[1:3]`, `x[4:6]`, 
`x[7:9]`, `x[10:12]`. This is useful for processing any vector or 
list produced by a function that returns a regular length output.

Note that if the original sequence is not an integer multiple of the
window length, the last sub-vector will not have the same length as
the preceding sub-vectors.

Range operations
----------------

While block operations use adjacent sub-vectors, range operations
use overlapping sub-vectors. This process is analogous to a 
sliding window, where the index increments by one as opposed to
by the window size. For the same vector `x`, a maprange operation
with window of length 3 produces the following sub-vectors as
arguments: `x[1:3]`, `x[2:4]`, `x[3:5]`, ..., `x[10:12]`.

An example of a range operation is generating n-grams from a text
document. Suppose a vector `v` contains a sequence of words. Then
`maprange(v, 2, function(x) paste(x, collapse=' '))` creates bigrams.


Two-dimensional operations
--------------------------

Typically map and fold operate on 1-dimensional data structures,
but in R operations can also be applied on 2-dimensional data structures.
For example, the `apply` function works in this manner where the
`MARGIN` argument defines whether iteration operates on rows versus
columns. 
Hence `lambda.tools` introduces 2-dimensional versions of
these functions.  For simplicity, the 2-dimensional variants of 
map and fold only operate along columns.
To operate along rows requires transposing the data structure.

Consider the following code that applies multiple
rotations to a collection of points.

```
ps <- t(matrix(c(0,0, 4,0, 2,4), nrow=2))
rt <- matrix(c(cos(pi),-sin(pi),sin(pi),cos(pi), 
  cos(pi/2), -sin(pi/2), sin(pi/2), cos(pi/2)), nrow=2)
mapblock(rt, 2, function(x) ps %*% x)
```
 
The result is a 6x2 matrix that is the union of the two rotation
operations.

Other functions included are functions to manipulate sequences,
such as `pad` a sequence to a specified length, `chomp`
the head and tail off a vector, `slice` a sequence into
two pieces based on an expression. The `partition` function
is similar, while `quantize` and `confine` transform data to fit
specific ranges.


Continuous Builds
=================

The master branch of `lambda.tools` builds continuously at 
[Travis CI](https://travis-ci.org/zatonovo/lambda.tools)

Installation
============

+ From CRAN: `install.packages('lambda.tools')
+ From github, use devtools: ```install_github('lambda.tools','zatonovo')```


Acknowledgements
================
Special thanks to Eric Cox who helped write tests for the package.
