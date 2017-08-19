-- Return True for a valid credit card number
-- validate 4012888888881881 = True
-- validate 4012888888881882 = False
-- validate (-4012888888881881) = False

validate n
 | n > 0 = sumDigits (doubleEveryOther n) `mod` 10  == 0
 | otherwise = False

doubleEveryOther xs = map (\t ->
                            if snd t `mod` 2 == 1 then fst t
                            else 2 * fst t
                          )$ zip (toDigitsR xs) [1..]

sumDigits = foldr (\x acc -> acc + sum (toDigitsR x)) 0

toDigits xs = reverse $ toDigitsR xs

toDigitsR n
       | n < 0     = []
       | n < 10    = [n]
       | otherwise = (n `mod` 10) : toDigitsR (n `div` 10)

