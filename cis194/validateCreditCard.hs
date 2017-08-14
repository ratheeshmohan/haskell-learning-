toDigitsR :: Int -> [Int]
toDigitsR n
       | n < 0     = []
       | n < 10    = [n]
       | otherwise = (n `mod` 10) : toDigitsR (n `div` 10)

toDigits xs = reverse $ toDigitsR xs

doubleEveryOther xs = map (\t ->
                            if snd t `mod` 2 == 1 then fst t
                            else 2 * fst t
                          )$ zip (toDigitsR xs) [1..]

sumDigits = foldr (\x acc -> acc + sum (toDigitsR x)) 0

validate n = if sumDigits (doubleEveryOther n) `mod` 10  == 0 then True
             else False
