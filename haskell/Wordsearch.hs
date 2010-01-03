{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Wordsearch where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader
import Data.List
import System.Environment
import System.Random

data Params = Params { getSize :: (Int, Int), getWords :: [String] }

newtype RandomTReader r a = RTS { runRTS :: RandT StdGen (Reader r) a }
    deriving (Monad, MonadRandom, MonadReader r)

evalRTS :: RandomTReader r a -> r -> StdGen -> a
evalRTS m p g = runReader (evalRandT (runRTS m) g) p

wordSearch :: (Int, Int) -> [String] -> StdGen -> [String]
wordSearch size words = evalRTS (emptySearch >>= addWords >>= mapM padChar >>= format) (Params size words)

emptySearch :: RandomTReader Params [Maybe Char]
emptySearch = do
  (width, height) <- asks getSize
  return $ replicate (width*height) Nothing

addWords :: [Maybe Char] -> RandomTReader Params [Maybe Char]
addWords search = asks getWords >>= foldM addWord search

addWord :: [Maybe Char] -> String -> RandomTReader Params [Maybe Char]
addWord search word = do
  (width, height) <- asks getSize
  startPos <- getRandomR (0, (width*height-1))
  let dirs = [-width-1, -width, -width+1, -1, 1, width-1, width, width+1]
  dir <- liftM (dirs!!) $ getRandomR (0, 7)
  let endPos = startPos+dir*((length word)-1)
  if ((width*height > endPos) && (endPos > -1) && ((startPos `mod` width) + ((length word)-1)*(dir `mod` width) == endPos `mod` width))
     then let maybeSearch = foldM addChar search (zip (iterate (dir+) startPos) word)
             in case maybeSearch of
                  Just newSearch -> return newSearch
                  Nothing -> addWord search word
     else
         addWord search word

addChar :: [Maybe Char] -> (Int, Char) -> Maybe [Maybe Char]
addChar search (n, x) = case search !! n of
                          Just y -> if (y == x)
                                    then Just search
                                    else Nothing
                          Nothing -> Just (replace search n (Just x))

replace :: [a] -> Int -> a -> [a]
replace xs n x = let (first, second) = splitAt (n+1) xs
		     in (init first) ++ [x] ++ second

padChar :: Maybe Char -> RandomTReader Params Char
padChar Nothing = liftM (charList!!) $ getRandomR (0, length charList-1)
padChar (Just x) = return x

charList :: String
charList = ['a'..'z']

format :: String -> RandomTReader Params [String]
format [] = return []
format xs = do
  (width, height) <- asks getSize
  liftM (take width xs :) (format $ drop width xs)
