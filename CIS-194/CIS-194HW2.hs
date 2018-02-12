{-
Name: <Nisshi>
Collaborators: <none>
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW02 where

import Words
import Data.List

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Write your code below:
removeFstStr :: Char -> String -> String
removeFstStr k [] = []
removeFstStr k (x:xs)  | k == x = xs
                      | otherwise = x : removeFstStr k xs

formableBy :: String -> Hand -> Bool
formableBy [] xs = True
formableBy a xs  = elem (a!!0) xs && formableBy (tail a)(removeFstStr (a!!0) xs)

allW :: Hand -> [String]
allW a = permutation a (length a)

permutation :: String -> Int -> [String]
permutation [] n = []
permutation a 1 = [[a!!x] | x <- [0..length a -1]]
permutation a n = [z!!x ++ [(a \\ (z!!x))!!y] | x <- [0..length z -1],
                                                y <- [0..length (a \\ z!!x)-1]]
                                                ++ z
                  where z = permutation a (n-1)

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

wordFitsTemplate' :: Template -> Hand -> String -> Bool
wordFitsTemplate' [] ys xs  = formableBy xs ys
wordFitsTemplate' zs ys [] = False
wordFitsTemplate' (z:zs) ys (x:xs) | z == '?' = wordFitsTemplate' zs ys xs
                                   | otherwise = z == x
                                   && wordFitsTemplate' zs ys xs

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate x y z =  wordFitsTemplate' x y z && formableBy z y

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate x y = [z | z <- wordsFrom y, wordFitsTemplate x y z ]

scrabbleValueWord :: String -> Int
scrabbleValueWord [] = 0
scrabbleValueWord (x:xs) = scrabbleValue x + scrabbleValueWord xs

bestWords' :: [String] -> [(Int,String)]
bestWords' [] = []
bestWords' xs= [(scrabbleValueWord(y),y) | y <- xs]

bestWords :: [String] -> [String]
bestWords xs = [ snd(y) | y <- bestWords' xs,
                          fst(maximum (bestWords' xs)) == fst(y)]

scrabbleValueTemplate' :: STemplate -> String -> Int
scrabbleValueTemplate' xs [] = 0
scrabbleValueTemplate' [] ys = 0
scrabbleValueTemplate' (x:xs) (y:ys) | x == 'D'  = 2*(scrabbleValue y) + m
                                     | x == 'T'  = 3*(scrabbleValue y) + m
                                     | otherwise =      scrabbleValue y + m
                      where m = scrabbleValueTemplate' xs ys

howMany :: Char -> String -> Int
howMany k [] = 0
howMany k (x:xs) | k == x    = 1 + howMany k xs
                 | otherwise = howMany k xs

scrabbleValueTemplate :: STemplate -> String -> Int

scrabbleValueTemplate xs ys | (howMany '3' xs) /= 0 && (howMany '2' xs) /= 0  =
                                  3^(howMany '3' xs)*2^(howMany '2' xs)*m
                            | (howMany '3' xs) /= 0 || (howMany '2' xs) /= 0  =
                                  3^(howMany '3' xs)*m + 2^(howMany '2' xs)*m
                            | otherwise = m
                  where m = scrabbleValueTemplate' xs ys
