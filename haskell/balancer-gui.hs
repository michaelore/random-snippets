module Main(main) where

import System.IO
import Graphics.UI.WX
import Balancer

main :: IO ()
main = start gui

gui :: IO ()
gui = do
    f <- frame [text := "The Balancer"
               ,clientSize := sz 270 110]
    un <- entry f [text := ""
                  ,clientSize := sz 250 25
                  ,position := pt 10 10]
    bal <- entry f [text := ""
                   ,clientSize := sz 250 25
                   ,position := pt 10 40]
    b <- button f [text := "Balance!"
                  ,position := pt 10 70
                  ,on command := setBalancedEquation un bal]
    return ()

setBalancedEquation :: TextCtrl () -> TextCtrl () -> IO ()
setBalancedEquation i o = do
    equation <- get i text
    case balance equation of
        Nothing -> set o [text := "ERROR"]
        Just eq -> set o [text := eq]
