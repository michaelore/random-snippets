-- Explanation and algorithm description can be found here:
-- http://www.eskimo.com/~rstarr/poormfa/explaintravesty.html
module Main where

import Control.Monad
import Data.Array
import Data.IORef
import Data.List
import Data.Maybe
import qualified Data.HashTable as H
import Random
import System.IO
import System.IO.Unsafe
import System.Environment

--Warning: uses unsafeInterleaveIO
travesty :: Int -> Maybe Int -> String -> IO String
travesty order maybeChar raw = do
    let str = filter (flip elem (range ('\0','\DEL'))) raw
    table <- H.new (==) H.hashString
    forM_ (take (length str) . map (take order) . tails . cycle . reverse $ str) $ \charAndComb -> do
        let (char:rcomb) = charAndComb
	let comb = reverse rcomb
        maybeVal <- H.lookup table comb
        case maybeVal of
            Just val -> do H.update table comb $ accum (+) val [(char, 1)]; return ()
            Nothing -> H.insert table comb $ accum (+) (array ('\0','\DEL') [(x,0) | x <- range ('\0','\DEL')]) [(char, 1)]
    let loop comb = do
        maybeVal <- H.lookup table comb
        choice <- case maybeVal of
            Just val -> weightedPick . assocs $ val
            Nothing -> error $ "The impossible happened! See line 32 of Travesty.hs\n"
	next <- unsafeInterleaveIO $ loop (tail comb ++ [choice])
	return $ choice:next
    let initialState = take (order-1) str
    output <- liftM (initialState++) $ loop initialState
    case maybeChar of
	Just char -> return $ take char output
	Nothing -> return output

weightedPick :: [(a, Int)] -> IO a
weightedPick choices = liftM (`weightedIndex` choices) . randomRIO $ (0 :: Int, weightedLength choices - 1)
    where weightedIndex _ [] = undefined
          weightedIndex 0 ((_, 0):xs) = weightedIndex 0 xs
          weightedIndex 0 ((x, _):_) = x
          weightedIndex a ((_, 0):xs) = weightedIndex a xs
          weightedIndex a ((_, 1):xs) = weightedIndex (a-1) xs
          weightedIndex a ((x, b):xs) = weightedIndex (a-1) ((x, b-1):xs)
          weightedLength xs = weightedLength' xs 0
          weightedLength' [] acc = acc
          weightedLength' ((_, 0):xs) acc = weightedLength' xs acc
          weightedLength' ((_, 1):xs) acc = weightedLength' xs (acc+1)
          weightedLength' ((x, n):xs) acc = weightedLength' ((x, n-1):xs) (acc+1)

main = do
    argv <- getArgs
    (order, maybeChar) <- case argv of
	(_:_:_:_) -> error $ "Too many arguments.\n" ++ helpMessage
	[order, char] -> return (read order, Just . read $ char)
	[order] -> return (read order, Nothing)
    getContents >>= travesty order maybeChar >>= putStrLn

helpMessage = "Type either: travesty order chars, or\n" ++
	      "             travesty order\n" ++
	      "Where order is the order of the desired travesty, and chars is the number of desired characters. If chars is omitted, the output is infinite. Either way, the output is produced lazily."
