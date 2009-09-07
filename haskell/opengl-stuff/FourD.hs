module FourD where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Control.Arrow

data Dim = W | X | Y | Z deriving (Eq, Show)

getDim W = getW
getDim X = getX
getDim Y = getY
getDim Z = getZ

excludeDim W (FourPoint _ x y z) = Vertex3 x y z
excludeDim X (FourPoint w _ y z) = Vertex3 w y z
excludeDim Y (FourPoint w x _ z) = Vertex3 w x z
excludeDim Z (FourPoint w x y _) = Vertex3 w x y

dims = [W, X, Y, Z]

class FourPrimitive a where
    renderFP :: a -> Dim -> GLfloat -> IO ()

data FourPoint = FourPoint {
    getW :: GLfloat,
    getX :: GLfloat,
    getY :: GLfloat,
    getZ :: GLfloat
}

instance FourPrimitive FourPoint where
    renderFP point dim n =
        if getDim dim point == n then
            renderPrimitive Points (vertex$excludeDim dim point) >> return ()
        else
            return ()

within :: (Ord a) => a -> a -> a -> Bool
within a b c = max b c > a && a > min b c

split :: (Arrow a) => a b c -> a (b, b) (c, c)
split x = x *** x

data FourLine = FourLine FourPoint FourPoint

instance FourPrimitive FourLine where
    renderFP (FourLine point1 point2) dim n =
        if getDim dim point1 == n && getDim dim point2 == n then
            renderPrimitive Lines (runKleisli (split (Kleisli $ vertex . excludeDim dim)) $ (point1, point2)) >> return ()
        else if not (split (getDim dim) >>> (uncurry $ within n) $ (point1, point2)) then
                return ()
            else renderPrimitive Points (vertex$Vertex3 (intersection !! 0) (intersection !! 1) (intersection !! 2)) >> return ()
            where intersection = [fun d | d <- dims, d /= dim]
                  fun d = let y2 = getDim d point2
                              y1 = getDim d point1
                              x2 = getDim dim point2
                              x1 = getDim dim point1
                          in (y2-y1)*(n-x1)/(x2-x1)+y1

data FourTriangle = FourTriangle FourPoint FourPoint FourPoint

instance FourPrimitive FourTriangle where
    renderFP (FourTriangle point1 point2)
