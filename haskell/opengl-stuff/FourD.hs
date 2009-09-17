module FourD where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.List

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
    renderFP point dim n = if getDim dim point == n then renderPrimitive Points $ vertex $ excludeDim dim point else return ()

intersects :: FourPoint -> FourPoint -> Dim -> GLfloat -> Bool
intersects point1 point2 dim n = max b c > n && n > min b c where [b, c] = map (getDim dim) [point1, point2]

intersection :: FourPoint -> FourPoint -> Dim -> GLfloat -> Vertex3 GLfloat
intersection point1 point2 dim n =
    let nums = [fun d | d <- dims, d /= dim]
    in Vertex3 (nums !! 0) (nums !! 1) (nums !! 2)
    where fun d = let y2 = getDim d point2
                      y1 = getDim d point1
                      x2 = getDim dim point2
                      x1 = getDim dim point1
                  in (y2-y1)*(n-x1)/(x2-x1)+y1

renderIfIntersects :: FourPoint -> FourPoint -> Dim -> GLfloat -> IO ()
renderIfIntersects point1 point2 dim n = if intersects point1 point2 dim n then vertex$intersection point1 point2 dim n else return ()

data FourLine = FourLine FourPoint FourPoint

instance FourPrimitive FourLine where
    renderFP (FourLine point1 point2) dim n =
        if getDim dim point1 == n && getDim dim point2 == n then
            renderPrimitive Lines (mapM_ (vertex . excludeDim dim) [point1, point2])
        else renderPrimitive Points $ renderIfIntersects point1 point2 dim n

data FourTriangle = FourTriangle FourPoint FourPoint FourPoint

instance FourPrimitive FourTriangle where
    renderFP (FourTriangle point1 point2 point3) dim n =
        let fun a b c = if intersects b c dim n then
                                renderPrimitive Lines (do vertex$excludeDim dim a
                                                          vertex$intersection b c dim n)
                              else
                                renderFP a dim n
        in case map (\x -> getDim dim x == n) [point1, point2, point3] of
            [True, True, True] -> renderPrimitive Triangles (mapM_ (vertex . excludeDim dim) [point1, point2, point3])
            [False, True, True] -> renderFP (FourLine point2 point3) dim n
            [True, False, True] -> renderFP (FourLine point1 point3) dim n
            [True, True, False] -> renderFP (FourLine point1 point2) dim n
            [True, False, False] -> fun point1 point2 point3
            [False, True, False] -> fun point2 point1 point3
            [False, False, True] -> fun point3 point1 point2
            otherwise -> renderPrimitive Lines $ do renderIfIntersects point1 point2 dim n
                                                    renderIfIntersects point1 point3 dim n
                                                    renderIfIntersects point2 point3 dim n

data FourTetrahedron = FourTetrahedron FourPoint FourPoint FourPoint FourPoint

instance FourPrimitive FourTetrahedron where
    renderFP (FourTetrahedron point1 point2 point3 point4) dim n =
        let fun1 a b c d = if intersects a b dim n then
                                renderPrimitive Triangles (do vertex$excludeDim dim c
                                                              vertex$excludeDim dim d
                                                              vertex$intersection a b dim n)
                           else
                                renderFP (FourLine c d) dim n
            fun2 a b c d = let intersections = zipWith (\x y -> intersects x y dim n) [b, c, d] [c, d ,b]
                           in if not $ or intersections then
                                renderFP a dim n
                              else
                                renderPrimitive Triangles (do vertex$excludeDim dim a
                                                              renderIfIntersects b c dim n
                                                              renderIfIntersects b d dim n
                                                              renderIfIntersects c d dim n)
        in case map (\x -> getDim dim x == n) [point1, point2, point3, point4] of
            [True, True, True, True] -> renderPrimitive TriangleStrip $ mapM_ (vertex . excludeDim dim) [point1, point2, point3, point4, point1, point2]
            [False, True, True, True] -> renderPrimitive Triangles $ mapM_ (vertex . excludeDim dim) [point2, point3, point4]
            [True, False, True, True] -> renderPrimitive Triangles $ mapM_ (vertex . excludeDim dim) [point1, point3, point4]
            [True, True, False, True] -> renderPrimitive Triangles $ mapM_ (vertex . excludeDim dim) [point1, point2, point4]
            [True, True, True, False] -> renderPrimitive Triangles $ mapM_ (vertex . excludeDim dim) [point1, point2, point3]
            [False, False, True, True] -> fun1 point1 point2 point3 point4
            [False, True, False, True] -> fun1 point1 point3 point2 point4
            [False, True, True, False] -> fun1 point1 point4 point2 point3
            [True, False, False, True] -> fun1 point2 point3 point1 point4
            [True, False, True, False] -> fun1 point2 point4 point1 point3
            [True, True, False, False] -> fun1 point3 point4 point1 point2
            [True, False, False, False] -> fun2 point1 point2 point3 point4
            [False, True, False, False] -> fun2 point2 point1 point3 point4
            [False, False, True, False] -> fun2 point3 point1 point2 point4
            [False, False, False, True] -> fun2 point4 point1 point2 point3
            otherwise -> renderPrimitive Triangles $ do renderIfIntersects point1 point2 dim n
                                                        renderIfIntersects point1 point3 dim n
                                                        renderIfIntersects point1 point4 dim n
                                                        renderIfIntersects point2 point3 dim n
                                                        renderIfIntersects point2 point4 dim n
                                                        renderIfIntersects point3 point4 dim n
