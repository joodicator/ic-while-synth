# Monday 18 July 2015

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
