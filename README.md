Can I create a data structure library that models Chris Okasaki's Purely Functional Data Structures better now with the help of effect to model laziness?  

## Chapter 3
- Leftists Heap
  - the rank of any left child is at least at large as the rank of its right sibling
  - the rank of a node is defined to be length of right spine
  - right spine is the rightmost path from the node to an empty node

Exericise 3.1

rank of leftist heap is at most [log(n+1)]

leftiests heap of n

Prove by induction?

1. base case: size of 2
- [log(2 + 1)] = 1
- rank of empty = 0
- in this case right spine can only contain 1
- 1 == [log(n+1)]

2. inductive case: P(n-1)

Assume, right spine of leftist heap n - 1, contains at most [log(n)] element for all n,

leftist property is applied all child based on definition  of leftiest heaps,

proof by contraditction,
assume right spine contains at least [log(n+1)] + 1 elements,

then based on leftiest property, rank(leftChild) >= log(n+1) + 1, however since size of left child < n, based on the inductive hypothesis, the rank of left child must be smaller than log(n+1) + 1.

## Chapter 4 Lazy Evaluation

- reproduce example in the book

## Chapter 5 Amortized Analysis

- sum of amortized cost is greater than actual cost
- queue example
  - head always O(1)
  - tail takes O(n) in the worst time
- physician method
  - potential function as the legnth of rear list

We need to construct a function such that

amortized_cost_i = real_cost_i + potentail_i - potential_(i-1)
