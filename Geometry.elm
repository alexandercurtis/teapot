module Geometry where

π = pi -- Unicode FTW!

type alias Point2D = {x: Float, y: Float}
newPoint2D [x,y] = Point2D x y


type alias Point3D = {x: Float, y: Float, z: Float}
newPoint3D [x,y,z] = Point3D x y z


degToRad : Float -> Float
degToRad a = 2 * π * a / 360


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


meanZ : List Point3D -> Float
meanZ face =
  let numVerts = List.length face
      sumZs = List.sum (List.map .z face)
  in
    sumZs / toFloat numVerts



normal : List Point3D -> Point3D
normal verts =
  let u = (Maybe.withDefault (Point3D 0 0 0) (List.head verts))
      v = (Maybe.withDefault (Point3D 0 0 0) (List.head (Maybe.withDefault [(Point3D 0 0 0)] (List.tail verts))))
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


