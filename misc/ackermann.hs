type Depth = Integer
type Count = Integer

ackermann
  :: Integer    -- m >= 0
  -> Integer    -- n >= 0
  -> (Integer,  -- A(m,n), the Ackermann–Péter function
      Depth,    -- Maximum recursion depth without TCE
      Depth,    -- Maximum recursion depth with TCE
      Count)    -- Total number of invocations

ackermann 0 n
  | n >= 0
  = (n+1, 0, 0, 1)

ackermann m 0
  | m > 0
  = (a, 1+d, td, 1+c)
  where (a, d, td, c) = ackermann (m-1) 1

ackermann m n
  | m > 0 && n > 0
  = (a', 1 + max d d', max (1+td) td', 1+c+c')
  where
    (a,  d,  td,  c)  = ackermann m (n-1)
    (a', d', td', c') = ackermann (m-1) a

ackermann _ _
  = error "ackermann: negative argument"
