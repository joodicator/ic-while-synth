# Friday 3 July 2015

Made a working implementation of a synthesiser, TemplateLearn.hs, capable of reading a program specification including midconditions (but not while loop bodies/variants/invariants), and synthesising the program piecewise by calling IterativeLearn.hs.

An example of this working is the program `max` which computes the maximum of 5 integers. The specification using midconditions is given in [`examples/template/max.lp`](https://github.com/JosephCrowe/ic-while-synth/blob/481e9856aa7daefc56002834128004dee49ede81/examples/template/max.lp) and the output in [`examples_output/template/max.output.txt`](https://github.com/JosephCrowe/ic-while-synth/blob/890593307ed9082ae7faae994e0be0a6e7169f9d/examples_output/template/max.output.txt).

Started preparing further changes that will allow while loop bodies to be separately synthesised.

Relevant commits: [481e985](https://github.com/JosephCrowe/ic-while-synth/commit/481e9856aa7daefc56002834128004dee49ede81) [8905933](https://github.com/JosephCrowe/ic-while-synth/commit/890593307ed9082ae7faae994e0be0a6e7169f9d)
