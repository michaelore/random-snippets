module Main where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Bindings

main = do
    (progname, _) <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "OpenGL Test"
    reshapeCallback $= Just reshape
    keyboardMouseCallback $= Just keyboardMouse
    time <- newIORef 0.0
    displayCallback $= (display time)
    idleCallback $= Just (idle time)
    mainLoop
