module Main(main) where

import System.IO
import Data.Maybe
import Balancer

main :: IO ()
main = interact $ unlines . (map $ (fromMaybe "Error") . balance) . lines
