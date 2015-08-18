Searching for a program with 0-3 lines satisfying 0 example(s)...
Found the following program:
   (empty program)
Searching for a counterexample to falsify the postcondition...
Found the following counterexample:
    Input:    xs = [1, 6], len_xs = 2
    Expected: xs = [6, 1]
    Output:   xs = [1, 6]
Searching for a program with 0-3 lines satisfying 1 example(s)...
No such program found.
Searching for a program with 4-7 lines satisfying 1 example(s)...
Found the following program:
    sub main():
       1. i = 0
       2. j = len_xs - 1
       3. while (i < j):
       4.     swap()
       5.     i = j + j
       6. end_while
    sub swap():
       1. swap = xs[i]
       2. xs[i] = xs[j]
       3. xs[j] = swap
Searching for a counterexample to falsify the postcondition...
Found the following counterexample:
    Input:    xs = [5, 1, 1, 1, 5, 4], len_xs = 6
    Expected: xs = [4, 5, 1, 1, 1, 5]
    Output:   (none)
Searching for a program with 6-9 lines satisfying 2 example(s)...
Found the following program:
    sub main():
       1. i = 0
       2. j = len_xs - 1
       3. while (i < j):
       4.     swap()
       5.     i = i + 1
       6. end_while
    sub swap():
       1. swap = xs[i]
       2. xs[i] = xs[j]
       3. xs[j] = swap
Searching for a counterexample to falsify the postcondition...
Found the following counterexample:
    Input:    xs = [1, 2, 1], len_xs = 3
    Expected: xs = [1, 2, 1]
    Output:   xs = [1, 1, 2]
Searching for a program with 6-9 lines satisfying 3 example(s)...
Found the following program:
    sub main():
       1. i = 0
       2. j = len_xs - 1
       3. while (i < j):
       4.     swap()
       5.     j = j - 1
       6.     i = i + 1
       7. end_while
    sub swap():
       1. swap = xs[i]
       2. xs[i] = xs[j]
       3. xs[j] = swap
Searching for a counterexample to falsify the postcondition...
The postcondition could not be falsified.

real    6m2.063s
user    6m43.217s
sys     0m6.139s
