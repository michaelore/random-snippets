--This thing is a hack...
module Wordsearch (wordSearch) where
import Data.List
import Data.Maybe
import System.Random

wordSearch :: (Int, Int) -> StdGen -> [String] -> [String]
wordSearch settings gen = format settings . pad gen . rawSearch settings gen

format :: (Int, Int) -> String -> [String]
format _ [] = []
format (width, height) xs = (take width xs) : (format (width, height) (drop width xs))

pad :: StdGen -> [Maybe Char] -> String
pad initGen = fst . foldr addChar ([], initGen)
  where addChar (Just x) (xs, gen) = ((x:xs), gen)
  	addChar Nothing (xs, gen) = ((((charList!!) $ fst $ randCharList gen):xs), snd $ next gen)
	randCharList = randomR (0, (length charList)-1)

rawSearch :: (Int, Int) -> StdGen -> [String] -> [Maybe Char]
rawSearch (width, height) gen = fst . foldr (addWord (width, height)) ((replicate (width*height) Nothing), gen)

addWord :: (Int, Int) -> String -> ([Maybe Char], StdGen) -> ([Maybe Char], StdGen)
addWord _ [] x = x
addWord (width, height) word (search, gen) = let (startPos, gen') = randSearch gen
        			       		 (dir, gen'') = randDir gen'
						 endPos = startPos+dir*((length word)-1)
						 randDir g = ([-width-1, -width, -width+1, -1, 1, width-1, width, width+1] !! (fst $ (randomR (0, 7) g)), (snd $ next g))
						 randSearch = randomR (0, (width*height-1))
						 in if ((width*height > endPos) && (endPos > -1) && ((startPos `mod` width) + ((length word)-1)*(dir `mod` width) == endPos `mod` width))
						 	then let it = foldr add (Just search) (zip (iterate (dir+) startPos) word)
								 in if (isNothing it)
								       then addWord (width, height) word (search, gen'')
								       else (fromMaybe [] it, gen'')
						 	else addWord (width, height) word (search, gen'')

add :: (Eq b) => (Int, b) -> (Maybe [Maybe b]) -> (Maybe [Maybe b])
add _ Nothing = Nothing
add (n, x) (Just list) = case list !! n of
			   Just y -> if (y == x)
			   		then Just list
					else Nothing
			   Nothing -> Just (replace list n (Just x))

replace :: [a] -> Int -> a -> [a]
replace xs n x = let (first, second) = splitAt (n+1) xs
		     in (init first) ++ [x] ++ second

charList :: String
charList = ['a'..'z']
