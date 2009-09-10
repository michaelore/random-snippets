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
            renderPrimitive Points (vertex$excludeDim dim point)
        else
            return ()

intersects :: (Ord a) => a -> a -> a -> Bool
intersects a b c = max b c > a && a > min b c

intersection :: FourPoint -> FourPoint -> Dim -> GLfloat -> Vertex3 GLfloat
intersection point1 point2 dim n =
    let nums = [fun d | d <- dims, d /= dim]
    in Vertex3 (nums !! 0) (nums !! 1) (nums !! 2)
    where fun d = let y2 = getDim d point2
                      y1 = getDim d point1
                      x2 = getDim dim point2
                      x1 = getDim dim point1
                  in (y2-y1)*(n-x1)/(x2-x1)+y1

data FourLine = FourLine FourPoint FourPoint

instance FourPrimitive FourLine where
    renderFP (FourLine point1 point2) dim n =
        if getDim dim point1 == n && getDim dim point2 == n then
            renderPrimitive Lines (mapM_ (vertex . excludeDim dim) [point1, point2])
        else if not $ intersects n (getDim dim point1) (getDim dim point2) then
                return ()
            else renderPrimitive Points (vertex$intersection point1 point2 dim n)

data FourTriangle = FourTriangle FourPoint FourPoint FourPoint

instance FourPrimitive FourTriangle where
    renderFP (FourTriangle point1 point2 point3) dim n =
        let ps@[p1, p2, p3] = map (getDim dim) [point1, point2, point3]
        in case map (== n) ps of
            [True, True, True] -> renderPrimitive Triangles (mapM_ (vertex . excludeDim dim) [point1, point2, point3])
            [False, True, True] -> renderFP (FourLine point2 point3) dim n
            [True, False, True] -> renderFP (FourLine point1 point3) dim n
            [True, True, False] -> renderFP (FourLine point1 point2) dim n
            [True, False, False] -> if intersects n p2 p3 then
                                        renderPrimitive Lines (do vertex$excludeDim dim point1
                                                                  vertex$intersection point2 point3 dim n)
                                    else renderFP point1 dim n
            [False, True, False] -> if intersects n p1 p3 then
                                        renderPrimitive Lines (do vertex$excludeDim dim point2
                                                                  vertex$intersection point1 point3 dim n)
                                    else renderFP point2 dim n
            [False, False, True] -> if intersects n p1 p2 then
                                        renderPrimitive Lines (do vertex$excludeDim dim point3
                                                                  vertex$intersection point1 point2 dim n)
                                    else renderFP point3 dim n
            otherwise -> renderPrimitive Lines (do if intersects n p1 p2 then vertex$intersection point1 point2 dim n else return ()
                                                   if intersects n p1 p3 then vertex$intersection point1 point3 dim n else return ()
                                                   if intersects n p2 p3 then vertex$intersection point2 point3 dim n else return ())
