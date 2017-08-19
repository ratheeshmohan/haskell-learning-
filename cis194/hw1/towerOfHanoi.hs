type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]

-- Return list of moves to move n discs from the Peg a to b using c as transit.
-- hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
-- 1. move n-1 discs from a to c using b as temporary storage
-- 2. move the top disc from a to b
-- 3. move n-1 discs from c to b using a as temporary storage

hanoi n a b c
      | n <= 0 = []
      | n == 1 = [(a, b)]
      | otherwise = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

