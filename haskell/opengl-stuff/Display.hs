module Display (display, idle) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import FourD

display time = do
    clear [ColorBuffer]
    loadIdentity
    t <- get time
    renderFP (FourTriangle (FourPoint 0 (-1) (-1) 0) (FourPoint 0 1 1 0) (FourPoint 1 (-1) 1 0)) W (sin t)
    renderFP (FourTriangle (FourPoint 0 (-1) (-1) 0) (FourPoint 0 1 1 0) (FourPoint (-1) 1 (-1) 0)) W (sin t)
    swapBuffers

{-
points :: Int -> [(GLfloat, GLfloat, GLfloat)]
points n' = let n = fromIntegral n' in map (\k -> let t = 2*pi*k/n in (sin(t), cos(t),0.0)) [1..n]

display t = do
    clear [ColorBuffer]
    loadIdentity
    a <- get t
    rotate a $ Vector3 1 1 (1::GLfloat)
    scale 0.7 0.7 (0.7::GLfloat)
    mapM_ (\(x,y,z) -> preservingMatrix $ do
        color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
        translate $ Vector3 x y z
        cube (0.1::GLfloat)
        ) $ points 50
    swapBuffers
-}

idle time = do
    t <- get time
    time $= t + 0.01
    postRedisplay Nothing
