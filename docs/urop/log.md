# Tuesday 28 July 2015

Partially implement integration of the existing Haskell-to-ASP infrastructure into the program learner and counterexample finder. In particular, implemented:
* Synthesis of ASP rules, complete with arbitrary rule heads and variable domains, from classical propositions such as those generated from Haskell.
* A meta-Haskell interpreter that produces Abstract.Bool values (based on the [hint](https://hackage.haskell.org/package/hint) package, which is itself based on GHC's API).
* Support for Arrays in `IterativeLearn.hs`. 

For minimal functionality, it remains to implement support for Haskell-based conditions in `IterativeLearn.hs`. Further, it wll be necessary to implement support for arrays and Haskell conditions in `TemplateLearn.hs`.

Relevant commits: [67447c1](https://github.com/JosephCrowe/ic-while-synth/commit/67447c120fac322480c64cadbf9b70a6f130a649) [19f4542](https://github.com/JosephCrowe/ic-while-synth/commit/19f4542a9acd0da4cd6446f7fed1abec7f12d688) [77bba19](https://github.com/JosephCrowe/ic-while-synth/commit/77bba1902956464874a14221de8c97092f48e177)

# Monday 27 July 2015

Implemented an algorithm to convert from Haskell-generated boolean expression abstracted over variables to disjunctions of ASP rule bodies.

This is best demonstrated at the moment by showing a Haskell interpreter session:
```
> let toASP = mapM_ print . propToBodies . boolToPropASP -- helper function
> let (z, xs) = ("Z", ["X1".."X3"])      -- variables in our domain
> toASP $ z == max (xs !! 0) (xs !! 1)   -- maximum of two elements
Z == X1, X1 >= X2
Z == X2, X1 < X2
> toASP $ all (z >=) xs && any (z ==) xs -- maximum of N elements
Z == X1, Z >= X1, Z >= X2, Z >= X3
Z == X2, Z >= X1, Z >= X2, Z >= X3
Z == X3, Z >= X1, Z >= X2, Z >= X3
```
For the above examples it works nicely, but there are problems with redundant clauses being introduced: for example, using the standard `maximum` function instead of the custom version above, a disjunct is generated for every possible ordering of the array:
```
> toASP $ z == maximum xs
Z == X1, X1 >= X2, X1 >= X3
Z == X1, X1 >= X2, X1 >= X3, X2 >= X3
Z == X2, X1 < X2, X1 >= X3, X2 >= X3
Z == X2, X1 < X2, X2 >= X3
Z == X3, X1 < X2, X2 < X3
Z == X3, X1 < X3, X1 >= X2
```
In order to deal with this, either some stronger simplifications need to be applied to the output, or use of functions like `maximum` that cause a lot of logical branching must be discouraged in favour of the `any`/`all` approach.

This completes parts 1 and 3 of the task outlined in the [Thursday 23 July](#thursday-23-july-2015) entry, leaving part 4 yet to be completed. After this, it will also be necessary to integrate the new system with the existing program synthesiser to fully automate this process.

Relevant commits: [3f148fd](https://github.com/JosephCrowe/ic-while-synth/commit/3f148fda21a71c8cf1f3e56b3439a4d37e66c9c5) [b9f220a](https://github.com/JosephCrowe/ic-while-synth/commit/b9f220a0a33f8af461c4ec257dc88b3793a40064)

# Friday 24 July 2015

Implemented in the program learner `learn.lp` the ability to annotate the variables and constants of a program with **types**. Each type represents a region of memory, say, within which data flow is restricted. Moreoever, it is possible for these regions to overlap where an element inhabits more than one type, allowing controlled data flow through the program. This generalises the previously suggested notion of designating certain variables as array "pointers."

More specifically, zero or more *types*, represented as atoms, are assigned by the user to each variable, constant, array's elements and array's indices. Programs which contain an expression, assignment or array index where the involved types are *disjoint*, i.e. there is no type common to the type-sets being unified, are statically constrained out of the search space. An element with no user-defined types, or with the type `any`, implicitly inhabits *all* types, including the `any` type if this is not explicitly written by the user.

The current implementation has immediate performance benefits; for example, the `array_sum` program annoted with types to segregate array indices from array elements, specified [here](https://github.com/JosephCrowe/ic-while-synth/blob/6aa42be55ca02b9a1ae0010e2ce21e0f138e2b1a/examples/learn/array_sum_typ.lp) with output [here](https://github.com/JosephCrowe/ic-while-synth/blob/6aa42be55ca02b9a1ae0010e2ce21e0f138e2b1a/examples_output/learn/array_sum_typ.output.txt), improves on the untyped version by halving the grounding size (of the final iteration) and reducing the total synthesis time by a factor of 2.5:

`array_sum`   | Grounding size | Synthesis time
--------------|---------------:|---------------:
Without types |        1028433 |       146.660s
With types    |         472419 |        59.075s

Specifying types also has benefits insofar as ensuring a sensibly-structured program, exemplified by the fact that, for a particular set of human-chosen examples, the `array_max` program specified [here](https://github.com/JosephCrowe/ic-while-synth/blob/6aa42be55ca02b9a1ae0010e2ce21e0f138e2b1a/examples/learn/array_max_typ.lp) with types synthesises the [correct program](https://github.com/JosephCrowe/ic-while-synth/blob/6aa42be55ca02b9a1ae0010e2ce21e0f138e2b1a/examples_output/learn/array_max_typ.output.txt), while its untyped version with the same examples results in the [wrong program](https://github.com/JosephCrowe/ic-while-synth/blob/6aa42be55ca02b9a1ae0010e2ce21e0f138e2b1a/examples_output/learn/array_max.output.txt) (counterexample: `[1,3,6]`). It is expected that when combined with a counterexample-generator supporting arrays, type-annotation will reduce the number of examples needed to synthesise the correct program..

Relevant commits: [303f2a4](https://github.com/JosephCrowe/ic-while-synth/commit/303f2a4cb6f48af4f185307113ec2382532fac60)

# Thursday 23 July 2015

Completed implementation of `Abstract.Bool` and `Abstract.Int` Haskell data types, which allow boolean and integer expressions abstracted over named variables to be constructed from Haskell expressions, including those involving lists (nominally completed; it may be found that further definitions are needed to make it useful in practice).

Started implementing the transformation from `Abstract.Bool` expressions to ASP syntax. The transformation involves multiple stages:

  1. Convert an `Abstract.Bool` expression into a classical proposition whose *atoms* are arithmetic expressions. I have defined the type `Prop` in [`Logic.hs`](https://github.com/JosephCrowe/ic-while-synth/blob/arrays/main/Logic.hs) to this end. The transformation is slightly nontrivial where `if` expressions are involved, but I have an algorithm in mind for this

  2. Put the proposition into Disjunctive Normal Form. The function [`pToDNF`](https://github.com/JosephCrowe/ic-while-synth/blob/bbe952099168ad6f67223a9a7172bd0438700f35/main/Logic.hs#L49) in `Logic.hs` implements this.

  3. Convert each arithmetic expression in the DNF into ASP syntax (i.e. the type [`Expr`](https://github.com/JosephCrowe/ic-while-synth/blob/bbe952099168ad6f67223a9a7172bd0438700f35/main/ASP.hs#L37) in `ASP.hs`). This will be relatively easy, by construction of the type `Abstract.Int`, unless it's chosen to apply arithmetic simplification.

  4. Make each disjunct of the DNF into an ASP constraint whose conjuncts correspond to the (possibly negated) conjuncts in the disjunct; add appropriate domain predicates to each constraint so that each variable is properly bound.

It may also be necessary or desired to apply logical/arithmetical simplification at various points, to reduce the complexity of the generated code.

Relevant commits: [bbe9520](https://github.com/JosephCrowe/ic-while-synth/commit/bbe952099168ad6f67223a9a7172bd0438700f35)

# Wednesday 22 July 2015

Investigated various ways of expressing facts about arrays in the condition language in a way usable in Clingo. I have settled for now on writing them in Haskell's syntax, as with curried functions and its reasonably extensive standard `List` library it seems well-suited for concisely expressing list transformations.

Firstly, I concluded that it will be necessary to instantiate a condition once for each possible combination of lengths of the involved arrays. This is because there is no way to express variable-length structures in ASP that would generate a smaller grounding. In particular, fixing the array sizes for all examples and allowing some array elements to be "unset" would generate a larger grounding due to the possible "holes" in the array. Therefore, it remains to express facts about fixed-length arrays.

I can see  several ways this could be implemented, and I ran some benchmarks to get an idea of their feasibility:

 # |Description | Time taken to solve over 46656 values
---|------------|------------------------------------------
1 | **Using Clingo's Lua scripting** interface, **run a compiled Haskell binary** which takes free variables as command-line argments, evaluates a predicate, and yields its truth value in the process's exit code. As you might expect, most of the time was taken in creating and destroying processes, so this is not tenable. | 195.266s
2 | As #1, but **keep the process open throughout grounding** and communicate through pipes/files. This is a respectable improvement, but because of limitations in Lua's standard library, it will be difficult to make it both reliable and cross-platform compatible. | 3.917s
3 | **Allow conditions to be written natively as Lua functions** rather than calling an external process. This is even more of an improvement, but Lua's syntax is not very nice at all for concisely writing predicates. | 1.701s
4 | Rather than relying on Gringo, **generate the ground constraints** corresponding to the condition **directly in Haskell**, then (if needed) include them in an ASP for further processing. The logic in the counterexample-finder is simple enough that it might be possible to eliminate altogether the need for ASP in this component. However, this would likely suffer from scalability issues for larger domains that Clingo can more intelligently mitigate. | 0.743s
5 | **Translate Haskell boolean expressions directly into ASP** constraint bodies, allowing the associated grounding to only accumulate in one place, and also allowing simplifying transformations to be made at the same time. The benchmark given here is based on a hypothetical, manually-written transformation from Haskell into ASP; but it seems if this were successful, it could afford a great performance benefit.  | 0.007s

\#5 seemed like the potentially most promising way of proceeding, so I spent some time thinking about how it might be achieved. Using Haskell's parametric polymorphism and GHC's [`RebindableSyntax`](https://downloads.haskell.org/~ghc/7.6.3/docs/html/users_guide/syntax-extns.html#rebindable-syntax) extension, it is possible in Haskell to redefine arithmetic and boolean operators (and some other Prelude functions) so that evaluating a boolean expression results in an **abstract syntax tree** representing a boolean expression over arithmetic expressions over integer variables. This could then be mechanically translated into ASP. This is the same idea used by [simple-reflect](https://hackage.haskell.org/package/simple-reflect), but takes it further insofar as making the result computationally useful.

I started writing [`Abstract.hs`](https://github.com/JosephCrowe/ic-while-synth/blob/arrays/main/Abstract.hs) which implements this idea. To show how it is useful, consider the following example, which uses a `Show` instance to print closed-form string representations of computations abstracted over named variables:
```
*Abstract> let xs = [IVar "X0", IVar "X1", IVar "X2", IVar "X3", IVar "X4"]
*Abstract> xs
[X0,X1,X2,X3,X4]
*Abstract> reverse xs
[X4,X3,X2,X1,X0]
*Abstract> sum xs
0 + X0 + X1 + X2 + X3 + X4
*Abstract> sum xs == product xs
0 + X0 + X1 + X2 + X3 + X4 == 1 * X0 * X1 * X2 * X3 * X4
```
The inputs, such as `sum xs`, are what the user would write, and the output `0 + X0 + X1 + X2 + X3 + X4` are what the ASP solver would receive.

Relevant commits: [f80c3a2](https://github.com/JosephCrowe/ic-while-synth/commit/f80c3a23553ea6c9aa4040a8405b7aa9ba11f213)

# Tuesday 21 July 2015

Implemented learning of array programs from discrete examples, but not yet with the ability to generate such examples. Example programs:
* `array_last`: the last element in an array; [specification](https://github.com/JosephCrowe/ic-while-synth/blob/a4e55ab614f8867fd2f44751fa56aaf7c2af3979/examples/learn/array_last.lp) and [output](https://github.com/JosephCrowe/ic-while-synth/blob/a4e55ab614f8867fd2f44751fa56aaf7c2af3979/examples_output/learn/array_last.output.txt).
* `array_sum`: the sum of array elements; [specification](https://github.com/JosephCrowe/ic-while-synth/blob/eee18892fc2b56e55d291a4d73dd2cea34145a26/examples/learn/array_sum.lp) and [output](https://github.com/JosephCrowe/ic-while-synth/blob/eee18892fc2b56e55d291a4d73dd2cea34145a26/examples_output/learn/array_sum.output.txt).

A problem with supporting arrays in the counterexample-finder is that it would be necessary to be able to express facts about arrays in the pre/postcondition language. This could be done by representing arrays as linked lists, but the grounding size is likely to be large when such lists are used in a lot of places. It may be better to use dynamically-generated predicates of variable arity, which are inlined when possible, or something else.

Relevant commits: [a4e55ab](https://github.com/JosephCrowe/ic-while-synth/commit/a4e55ab614f8867fd2f44751fa56aaf7c2af3979)

# Monday 20 July 2015

* Implemented a support for a form of statically-allocated arrays in the program simulator. [`run.lp`](https://github.com/JosephCrowe/ic-while-synth/blob/3cbbeed190492f8f964dc7a235cadc9518b97ad8/main/run.lp) contains commentary on the syntax and semantics decided so far. An example is given by the [specification](https://github.com/JosephCrowe/ic-while-synth/blob/3cbbeed190492f8f964dc7a235cadc9518b97ad8/examples/run/array_sum.lp) and [output](https://github.com/JosephCrowe/ic-while-synth/blob/3cbbeed190492f8f964dc7a235cadc9518b97ad8/examples_output/run/array_sum.output.txt) of running a program that finds the sum of the elements in a given array, when its (variable) length is given in the variable `n`. This work will take place on the `arrays` branch until it is deemed ready to merge with the main codebase. Relevant commits: [3cbbeed](https://github.com/JosephCrowe/ic-while-synth/commit/3cbbeed190492f8f964dc7a235cadc9518b97ad8)

* Fixed some minor concurrency issues in `IterativeLearn.hs`, allowing the `--echo-asp` option to now be usefully used with `--threads=N` for `N > 1`. These changes will also make it easier to make other improvements to the user interface. Relevant commits: [8b79f24](https://github.com/JosephCrowe/ic-while-synth/commit/8b79f2483341f5e7fa0611a652d64d30efa80f91)

* Added further content to UROP status report, a draft of which is available [here](http://www.doc.ic.ac.uk/~jjc311/while-synth/UROP_Report_WIP.pdf). Relevant commits: [976c9f2](https://github.com/JosephCrowe/ic-while-synth/commit/976c9f2335e90475a0b1a8d4cff64605c2e1e3db)

# Friday 17 July 2015

Nothing worth mentioning, as was attending [No More Sweden](http://nomoresweden.com/about) 2015.

# Thursday 16 July 2015

Continued work on UROP status report. Again, a work-in-progress report is available [here](http://www.doc.ic.ac.uk/~jjc311/while-synth/UROP_Report_WIP.pdf).

Relevant commits: [396ac2b](https://github.com/JosephCrowe/ic-while-synth/commit/396ac2b7c0dd203c6676fdba3e17719a6bcf1712)

# Wednesday 15 July 2015

Continued work on UROP status report. Again, a work-in-progress report is available [here](http://www.doc.ic.ac.uk/~jjc311/while-synth/UROP_Report_WIP.pdf).

Relevant commits: [ac1866a](https://github.com/JosephCrowe/ic-while-synth/commit/ac1866a6263d138e5c46cd614f489fed49e9aaac)

# Tuesday 14 July 2015

Continued work on UROP status report. Again, a work-in-progress report is available [here](http://www.doc.ic.ac.uk/~jjc311/while-synth/UROP_Report_WIP.pdf).

Relevant commits: [8cab2e7](https://github.com/JosephCrowe/ic-while-synth/commit/8cab2e7f7d204de07a903911b412ec750d6062cc)

# Monday 13 July 2015

Started writing a report summarising the work done so far on the UROP placement. A work-in-progress version is available [here](http://www.doc.ic.ac.uk/~jjc311/while-synth/UROP_Report_WIP.pdf).

Relevant commits: [e17f350](https://github.com/JosephCrowe/ic-while-synth/commit/e17f350970ba1b51bc3caa545aa5f1d2a4518b9b)

# Friday 10 July 2015

Mostly experimented and thought of ways to improve the synthesiser, and ways to reason about its correctness. Came up with some ideas that could acted upon, which I have written up in (some) detail in [issue #4](https://github.com/JosephCrowe/ic-while-synth/issues/4). 

I hope to explain the more clearly after I've had more time to think about it.

# Thursday 9 July 2015

Implemented an algorithm to propagate information from later to earlier conditions in a template, fixing [issue #2](https://github.com/JosephCrowe/ic-while-synth/issues/2). Also added a user-readable display of the template in its various stages to the program output.

* The `diff` program used to motivate the problem can now be synthesised from the [original template](https://github.com/JosephCrowe/ic-while-synth/blob/28d9ffea1d5f4287afe2c611cdf6b7c0e5f7dd7d/examples/template/diff.lp), and produces the [desired program](https://github.com/JosephCrowe/ic-while-synth/blob/28d9ffea1d5f4287afe2c611cdf6b7c0e5f7dd7d/examples_output/template/diff.output.txt).

* A more substantial example can be seen in the `gcd` program, which now requires a rather less complex [specification](https://github.com/JosephCrowe/ic-while-synth/blob/28d9ffea1d5f4287afe2c611cdf6b7c0e5f7dd7d/examples/template/gcd.lp) to produce the [correct program](https://github.com/JosephCrowe/ic-while-synth/blob/28d9ffea1d5f4287afe2c611cdf6b7c0e5f7dd7d/examples_output/template/gcd.output.txt).

Of interest in both of the above examples might be the output at the beginning which shows the template being transformed from the format given by the user.

Relevant commits: [6b73383](https://github.com/JosephCrowe/ic-while-synth/commit/6b733836be7ad8f084b9584055202fe4fa9bef94) [6216572](https://github.com/JosephCrowe/ic-while-synth/commit/62165728635a53e4107597714d5e7612bf5e239e)

# Wednesday 8 July 2015

Implemented transformation of templates containing (in)variants to templates containing just midconditions,  consequently allowing the user the specify their templates with less work.

For example, the program `aliquot` which classifies numbers as prime (indicated by `t=0`), deficient and nonprime (`t=1`), perfect (`t=2`) or excessive (`t=3`) based on their Aliquot sum, the sum of their proper divisors, is specified in [`examples/template/aliquot.lp`](https://github.com/JosephCrowe/ic-while-synth/blob/d222461083763a7e477b119fd1d68a564c10a6e6/examples/template/aliquot.lp), and produces a correct but difficult to read program in [`examples_output/template/aliquot.output.txt`](https://github.com/JosephCrowe/ic-while-synth/blob/d222461083763a7e477b119fd1d68a564c10a6e6/examples_output/template/aliquot.output.txt).

I suspect that the problem of correct but bizarre programs being synthesised could be mitigated by requiring them to be minimal (or close to minimal) in other senses than just the line number, for example in the set of constants used. This could also make synthesis faster, and easier for the user.

I also noticed a problem with the counterexample finder generating examples that impose unreasonable requirements on the program synthesiser, which is filed as [issue #3](https://github.com/JosephCrowe/ic-while-synth/issues/3).

Relevant commits: [2be2473](https://github.com/JosephCrowe/ic-while-synth/commit/2be2473f51ce64fe8f4935afeda419059ea08564) [947bd6c](https://github.com/JosephCrowe/ic-while-synth/commit/947bd6c231a2a75a7dad801688a576dde7e226bc)

# Tuesday 7 July 2015

Implemented a new internal format for program skeletons (using a tree instead of a flat representation, for easier manipulation; supporting more helpful error messages; and with some minor changes also to the user-written representation).

Relevant commits: [50c4e4e](https://github.com/JosephCrowe/ic-while-synth/commit/50c4e4e26a40ccad880e9760d51a7d51fff4aeae) [530626b](https://github.com/JosephCrowe/ic-while-synth/commit/530626b846bb5aafe1099536f90b30d202b48dc5)

# Monday 6 July 2015

Reworked much of how counterexamples are handled by the synthesiser, allowing non-injective postconditions to be used, i.e. those that allow more than one output per input. Consequently, programs annotated with loop variants and invariants can now be synthesised, although these must currently be expanded manually into the corresponding midcondition.

For example the `gcd` program, which computes the greatest common divisor of 2 integers, specified at [`examples/template/gcd.lp`](https://github.com/JosephCrowe/ic-while-synth/blob/48dff02d1456263c09149eb18c6a0ccfa243578b/examples/template/gcd.lp), uses the GCD of its two working variables as an invariant quantity, and the size of one as a variant, and produces the output at [`examples_output/template/gcd.output.txt`](https://github.com/JosephCrowe/ic-while-synth/blob/53fb8fc6393e87187f66fbf5bee62148546e5a29/examples_output/template/gcd.output.txt). This program synthesises in 5-10 seconds, compared to ~1 minute with just pre/postconditions.

Currently thinking about the best way to augment the midconditions given by the user to take full advantage of the information available. This would might include automatically determining loop in/variants, or performing case analysis on if statements, or augmenting midconditions with information from previous ones, as detailed in [issue #2](https://github.com/JosephCrowe/ic-while-synth/issues/2).

Relevant commits: [48dff02](https://github.com/JosephCrowe/ic-while-synth/commit/48dff02d1456263c09149eb18c6a0ccfa243578b) [0146584](https://github.com/JosephCrowe/ic-while-synth/commit/0146584116481d403a0f34063a4b345161094631) [e31978c](https://github.com/JosephCrowe/ic-while-synth/commit/e31978caf57ccc50a89fef1ecb971705f077a2e1) [cc9a593](https://github.com/JosephCrowe/ic-while-synth/commit/cc9a5939a83b3f3b3d7ddf6b6c8546627287a235)

# Friday 3 July 2015

Made a working implementation of a synthesiser, TemplateLearn.hs, capable of reading a program specification including midconditions (but not while loop bodies/variants/invariants), and synthesising the program piecewise by calling IterativeLearn.hs.

An example of this working is the program `max` which computes the maximum of 5 integers. The specification using midconditions is given in [`examples/template/max.lp`](https://github.com/JosephCrowe/ic-while-synth/blob/481e9856aa7daefc56002834128004dee49ede81/examples/template/max.lp) and the output in [`examples_output/template/max.output.txt`](https://github.com/JosephCrowe/ic-while-synth/blob/890593307ed9082ae7faae994e0be0a6e7169f9d/examples_output/template/max.output.txt).

Started preparing further changes that will allow while loop bodies to be separately synthesised.

Relevant commits: [481e985](https://github.com/JosephCrowe/ic-while-synth/commit/481e9856aa7daefc56002834128004dee49ede81) [8905933](https://github.com/JosephCrowe/ic-while-synth/commit/890593307ed9082ae7faae994e0be0a6e7169f9d)
