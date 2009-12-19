--A module for balancing chemical equations.
--Example: balance "H2 + O2 -> H2O"
--All polyatomic ions must be surrounded by parentheses.

module Balancer(balance) where

import Data.List
import Matrix
import Text.ParserCombinators.Parsec

balance :: (Monad m) => String -> m String
balance x = parseit x >>= processit >>= balanceit >>= formatit x

chemicalFail :: (Monad m) => m a
chemicalFail = fail "This equation does not make sense."

remDups :: (Eq a) => [a] -> [a]
remDups x = remDups' [] x
remDups' y [] = y
remDups' y (h:t) = if elem h y then
		     remDups' y t
		   else
		     remDups' (y ++ [h]) t

replace :: (Eq a) => a -> a -> [a] -> [a]
replace x y list = replace' [] list x y
replace' acc [] _ _ = acc
replace' acc (h:t) x y = if x == h
			   then replace' (acc ++ [y]) t x y
			   else replace' (acc ++ [h]) t x y

parseit :: (Monad m) => String -> m [[(String, Double)]]
parseit raw = case parse chemeq "chemical equation" raw of
		Left _ -> chemicalFail
		Right x -> return x 

chemeq = do left <- chemex
	    string " -> "
	    right <- chemex
	    return $ left ++ (map (map (\(x, y) -> (x, -y))) right)

chemex = do h <- chemcomp
	    option [] (try $ string " + ")
	    t <- option [] chemex
	    return (h:t)

chemcomp = do h <- chemel <|> chempoly
	      t <- option [] chemcomp
	      return (h:t)

chemel = do name <- upper
	    rest <- many lower
	    num <- many1 digit <|> return "1"
	    return (name:rest, read num :: Double)

chempoly = do char '('
	      poly <- many1 (upper <|> lower <|> digit)
	      char ')'
	      num <- many1 digit <|> return "1"
	      return ("(" ++ poly ++ ")", read num :: Double)

processit :: (Monad m) => [[(String, Double)]] -> m ([String], Matrix Double)
processit parsed = let elems = remDups $ concat $ map (map (\x -> fst x)) parsed
		       matrix = map (\elem -> map (countElems elem) parsed) elems
		   in if length matrix == 0 || ((length $ head matrix) == 0) then
                          chemicalFail
                      else if length elems == (length $ head matrix) then
                          return (elems, (replicate (length $ head matrix) 1):(tail matrix))
                      else if length elems + 1 == (length $ head matrix) then
                          return (elems, (replicate (length $ head matrix) 1):matrix)
                      else
                          chemicalFail
                   where countElems elem comp = sum $ map snd $ filter (\(x, _) -> x == elem) comp

balanceit :: (Monad m) => ([String], Matrix Double) -> m [Int]
balanceit (elems, matrix) = let r = head $ transpose $ (invert matrix) * (transpose [[1] ++ (replicate ((length matrix) - 1) 0)])
			    in makeint $ map (\x -> x / (minimum r)) r

makeint :: (Monad m) => [Double] -> m [Int]
makeint r = makeint' r 1
makeint' _ 1000 = chemicalFail
makeint' r n = if and $ map isInt result then
		 return $ map round result
	       else
		 makeint' r (n+1)
	       where result = map (*n) r

isInt :: Double -> Bool
isInt n = (snd $ properFraction n) == 0

formatit :: (Monad m) => String -> [Int] -> m String 
formatit str res = case parse (addresult res) "" str of
			Left _ -> chemicalFail
			Right x -> return x

addresult nums = do result <- sepBy (many1 $ oneOf $ ['a'..'z'] ++ ['A'..'Z'] ++ (map (head . show) [0..9]) ++ "()->+") space
	            let comps = filter (\x -> not $ x == "+") $ delete "->" $ result
		        strns = map (\n -> if n == 1 then "" else show n) nums
			newcomps = zipWith (++) strns comps
		    return $ tail $ foldl (\x y -> x ++ " " ++ y) [] $ foldr (uncurry replace) result $ zip comps newcomps
