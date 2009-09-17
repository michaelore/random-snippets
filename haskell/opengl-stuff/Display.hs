module Display (display, idle) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import FourD

display time = do
    t <- get time
    clear [ColorBuffer]
    loadIdentity
    scale 0.7 0.7 (0.7 :: GLfloat)
    renderFP (FourTetrahedron (FourPoint (-0.5) 1 1 1) (FourPoint (0.5) (-1) (-1) 1) (FourPoint (-0.5) (-1) 1 (-1)) (FourPoint (0.5) 1 (-1) (-1))) W 0
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
