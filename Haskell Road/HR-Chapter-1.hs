divides :: Integer -> Integer -> Bool
divides d n = rem n d == 0

ldf :: Integer -> Integer -> Integer
ldf k n | divides k n = k
        | k^2 > n     = n
        | otherwise   = ldf (k+1) n

ld :: Integer -> Integer
ld n = ldf 2 n

isPrime :: Integer -> Bool
isPrime n | n < 0     = error "Is not a positive integer"
          | n == 1    = False
          | otherwise = ld n == n

minInt :: [Integer] -> Integer
minInt []     = error "Empty list"
minInt [x]    = x
minInt (x:xs) = min' x (minInt xs)

max' :: Integral a => a -> a -> a
max' x y | x < y     = y
         | otherwise = x

maxInt :: [Integer] -> Integer
maxInt []     = error "Empty List"
maxInt [x]    = x
maxInt (x:xs) = max' x (maxInt xs)

min' :: Integral a => a -> a -> a
min' x y | x < y     = x
         | otherwise = y

removeFst :: [Integer] -> Integer -> [Integer]
removeFst [] m = []
removeFst (x:xs) m | x == m    = (xs)
                   | otherwise = x : (removeFst xs m)

srtInt :: [Integer] -> [Integer]
srtInt [] = []
srtInt xs = m : (srtInt (removeFst xs m))where m = minInt xs

srtInts :: [Integer] -> [Integer]
srtInts [] = []
srtInts xs = let
                m = minInt xs
             in m :(srtInts (removeFst xs m))

length' :: [a] -> Integer
length' []     = 0
length' (x:xs) = 1 + length' xs

sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

average :: [Integer] -> Rational
average []   = error "Empty List"
average (xs) = toRational (sum' xs) / toRational (length' xs)

count :: Char -> String -> Int
count k [] = 0
count k (x:xs) | k == x    = 1 + (count k xs)
               | otherwise = count k xs

rep :: Int -> Char -> String
rep 0 k = []
rep n k = k : rep (n-1) k

bup :: String -> Int -> String
bup [] n     = []
bup (x:xs) n = (rep n x) ++ (bup xs (n+1))

blowup :: String -> String
blowup xs = bup xs 1

minChar :: Char -> Char -> Char
minChar x y | x < y     = x
            | otherwise = y

mnmString :: String -> Char
mnmString []     = error "Empty List"
mnmString [k]    = k
mnmString (x:xs) = minChar x (mnmString xs)

removeFstStr :: Char -> String -> String
removeFstStr k [] = []
removeFstStr k (x:xs)  | k == x = xs
                      | otherwise = x : removeFstStr k xs

sortString :: String -> String
sortString [] = []
sortString xs = m : sortString (removeFstStr m xs) where m = mnmString xs

prefix :: String -> String -> Bool
prefix [] ys         = True
prefix (x:xs) []     = False
prefix (x:xs) (y:ys) = x == y && (prefix xs ys)

substring :: String -> String -> Bool
substring [] ys = True
substring xs [] = False
substring xs (y:ys) = prefix xs (y:ys) || substring xs ys

factor :: Integer -> [Integer]
factor n | n < 1     = error "Not a positive integer"
         | n == 1    = []
         | otherwise = [ld n] ++ factor (div n (ld n))

factor' :: Integer -> [Integer]
factor' n | n < 1     = error "Not a positive integer"
          | n == 1    = []
          | otherwise = p : factor (div n p) where p = ld n

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

lengthLists :: [[a]] -> [Integer]
lengthLists [] = error "Empty list"
lengthLists xs = map (length') xs

sumLength :: [[a]] -> Integer
sumLength [] = error "Empty List"
sumLength xs = sum' (lengthLists xs)

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) | f x       = x: filter' f xs
                 | otherwise = filter' f xs

primes' :: [Integer]
primes' = 2 : filter' prime[3..]

ldp :: Integer -> Integer
ldp n = lpdf primes' n

prime :: Integer -> Bool
prime n | n < 1     = error "Not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n

lpdf :: [Integer] -> Integer -> Integer
lpdf (p:ps) n | rem n p == 0 = p
              | p^2 > n      = n
              | otherwise    = lpdf ps n
