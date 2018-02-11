-- Validating credit card numbers
toDigitsRev :: Integer -> [Integer]
toDigitsRev x | x < 0     = []
              | x < 10    = [x]
              | otherwise = [rem x 10] ++ (toDigitsRev (div x 10))

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = (reverseList xs) ++ [x]

toDigits :: Integer -> [Integer]
toDigits x = reverseList( toDigitsRev x)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) | x < 10    = x + sumDigits xs
                 | otherwise = (div x 10) + (rem x 10) + sumDigits xs

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther'  [] = []
doubleEveryOther' [x] = [x]
doubleEveryOther' [x,y] = [x] ++ [2*y]
doubleEveryOther' (x:y:xs) = [x] ++ [2*y] ++ (doubleEveryOther' xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverseList(doubleEveryOther' (reverseList xs))

validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0

-- Hanoi Towers.
type Peg  = String
type Move = (Peg, Peg)

moveDisk :: Peg -> Peg -> [Move]
moveDisk x y = [(x,y)]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 x y z = moveDisk x y
hanoi n x y z = hanoi (n-1) x z y ++ moveDisk x y ++ hanoi (n-1) z y x
