toDigits :: Integer -> [Integer]
toDigits i
    | i < 0     = []
    | i == 0    = []
    | i <  10   = [i]
    | otherwise = toDigits (i `div` 10) ++ [i `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs)
    | odd (length zs) = x : y*2 : doubleEveryOther zs
    | otherwise       = x*2 : y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits = foldl (+) 0 . concat . map toDigits

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits) x `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi disks a b c = (hanoi (disks-1) a c b) ++ [(a, c)] ++ (hanoi (disks-1) b a c)
