module FourD where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data Dim = W | X | Y | Z deriving (Eq, Show)

dims = [W, X, Y, Z]

class FourPrimitive a where
    renderFP :: a -> Dim -> GLfloat -> IO ()

data FourPoint = FourPoint {
    getW :: GLfloat,
    getX :: GLfloat,
    getY :: GLfloat,
    getZ :: GLfloat
}

getDim W = getW
getDim X = getX
getDim Y = getY
getDim Z = getZ

excludeDim W (FourPoint _ x y z) = Vertex3 x y z
excludeDim X (FourPoint w _ y z) = Vertex3 w y z
excludeDim Y (FourPoint w x _ z) = Vertex3 w x z
excludeDim Z (FourPoint w x y _) = Vertex3 w x y

instance FourPrimitive FourPoint where
    renderFP point dim n =
        if getDim dim point == n then
            renderPrimitive Points (vertex$excludeDim dim point) >> return ()
        else
            return ()

within :: (Ord a) => a -> a -> a -> Bool
within a b c = max b c > a && a > min b c

data FourLine = FourLine FourPoint FourPoint

instance FourPrimitive FourLine where
    renderFP (FourLine point1 point2) dim n =
        if getDim dim point1 == n && getDim dim point2 == n then
            renderPrimitive Lines (mapM_ (vertex . excludeDim dim) [point1, point2]) >> return ()
        else if not $ within n (getDim dim point1) (getDim dim point2) then
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
    renderFP (FourTriangle point1 point2 point3) dim n =
        case map (\x -> getDim dim x == n) [point1, point2, point3] of
            [True, True, True] -> renderPrimitive Triangles (mapM_ (vertex . excludeDim dim) [point1, point2, point3]) >> return ()
            [False, True, True] -> twoPoints point2 point3
            [True, False, True] -> twoPoints point1 point3
            [True, True, False] -> twoPoints point1 point2
            [True, False, False] -> onePoint point1
            [False, True, False] -> onePoint point2
            [False, False, True] -> onePoint point3
        where twoPoints = undefined
              onePoint = undefined
