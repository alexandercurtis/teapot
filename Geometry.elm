module Geometry where




nth : List a -> Int -> Maybe a
nth xs n = List.head ( List.drop n xs )



π = pi -- Unicode FTW!


degToRad : Float -> Float
degToRad a = 2 * π * a / 360



type alias Point2D = {x: Float, y: Float}
newPoint2D [x,y] = Point2D x y


type alias Point3D = {x: Float, y: Float, z: Float}
newPoint3D [x,y,z] = Point3D x y z



type alias Poly = {illumination: Float, vertices: List Point3D, velocity: Point3D}

getVert : Poly -> Int -> Point3D
getVert poly i = Maybe.withDefault (Point3D 0 0 0) ( nth (poly.vertices) i )

sub : Point3D -> Point3D -> Point3D
sub a b =
  Point3D (a.x - b.x) (a.y - b.y) (a.z - b.z)


-- Returns a vector representing the edge between the specified vertices
getEdgeVec : Poly -> Int -> Int -> Point3D
getEdgeVec poly i1 i2 =
  let
    v1 = getVert poly i1
    v2 = getVert poly i2
  in
    sub v2 v1



rotateX : Float -> Point3D -> Point3D
rotateX θ point3D =
  let c = cos θ
      s = sin θ
  in
    Point3D point3D.x (c * point3D.y - s * point3D.z) (s * point3D.y + c * point3D.z)


rotateY : Float -> Point3D -> Point3D
rotateY θ point3D =
  let c = cos θ
      s = sin θ
  in
    Point3D (c * point3D.x + s * point3D.z) point3D.y (c * point3D.z - s * point3D.x)


rotateZ : Float -> Point3D -> Point3D
rotateZ θ point3D =
  let c = cos θ
      s = sin θ
  in
    Point3D (c * point3D.x - s * point3D.y) (s * point3D.x + c * point3D.y) point3D.z


translate : Float -> Float -> Point2D -> Point2D
translate deltaX deltaY point2D =
  Point2D (point2D.x + deltaX) (point2D.y + deltaY)

zoom : Float -> Point2D -> Point2D
zoom scale point2D =
  Point2D (scale * point2D.x) (scale * point2D.y)

project : Float -> Point3D -> Point2D
project perspective point3D =
  Point2D (point3D.x + perspective * point3D.z) (point3D.y + perspective * point3D.z)


meanZ : Poly -> Float
meanZ face =
  let numVerts = List.length face.vertices
      sumZs = List.sum (List.map .z face.vertices)
  in
    sumZs / toFloat numVerts



normal : Poly -> Point3D
normal face =
  let u = getEdgeVec face 0 1
      v = getEdgeVec face 0 2
      i = (u.y * v.z - u.z * v.y)
      j = (u.z * v.x - u.x * v.z)
      k = (u.x * v.y - u.y * v.x)
  in
      Point3D i j k



dot : Point3D -> Point3D -> Float
dot p1 p2 =
  p1.x * p2.x + p1.y * p2.y + p1.z * p2.z

mag : Point3D -> Float
mag p = sqrt ( p.x * p.x + p.y * p.y + p.z * p.z )

cosAngle : Point3D -> Point3D -> Float
cosAngle p1 p2 =
  (dot p1 p2) / ((mag p1) * (mag p2))


