%*

    -- Initialise s = {0}, the set of nodes in the MST.
 1. s[0] = 1
 2. i = 1
 3. while (i < m):
 4.    s[i] = 0
 5.     i = i + 1
 6.  end_while

    -- Initialise t = 0, the adjacency matrix of the MST.
 7. mm = m * m
 8. i = 0
 9. while (i < mm):
10.     t[i] = 0
11.     i = i + 1
12. end_while

    -- Run Prim's algorithm.
13. any = 1
14. while (any > 0):
        -- Find an edge (i,j) such that i in s and j not in s,
        -- and add (i,j) to t and j to s.
15.     any = 0
16.     i = 0
17.     while (i < m):
18.         in_s = s[i]
19.         if in_s > 0:
20.             j = 0
21.             while (j < m):
22.                 in_s = s[j]
23.                 if in_s <= 0:                       
24.                     k = m * i
25.                     k = k + j
26.                     t[k] = 1  -- t[j,i] = 1
                  
27.                     k = m * j
28.                     k = k + i
29.                     t[k] = 1  -- t[i,j] = 1

30.                     s[j] = 1
31.                     any = 1
32.                 j = j + 1
33.             end_while
34.         i = i + 1
35.     end_while
36. end_while

*%
