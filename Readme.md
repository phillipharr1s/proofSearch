README

Proof search for Calculus of Constructions based on [1] with some optimizations for readability. 

Bindings are written on the right, `x(x:X)` instead of `(x:X)x`, so you can see the head of a whnf term at a glance. 

Each meta variable carries a list of free variables that may appear in it -- if a substitution {x=y} is applied to a non-existent free variable, it is thrown away. This helps keep the stack of substitutions on top of a meta variable short. 

We use the following heuristic to search the tree of substitutions: given a unification problem with a number of meta variables, we substitute the meta variable that yields the smallest number of viable search nodes. For example, if we have the set of constraints

?X = ?Y 
a = ?Z 
b = ?W 
c = ?W 

where a,b,c are rigid and X,Y,Z,W are meta variables, subsituting X or Y will lead to a large number of unrefuted search nodes, substituting Z will lead to 1 and substituting W  will lead to 0, so we choose to substitute W and refute the whole branch. 

[1] Muñoz, C. (2001). Proof-term synthesis on dependent-type systems via explicit substitutions. [[link]](https://www.sciencedirect.com/science/article/pii/S0304397500001961)
