import Geometry

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)


-- MODEL

import TeapotModel as Model

-- VIEW

translate : Geometry.Point2D -> Geometry.Point2D
translate point2D =
  Geometry.translate 20.0 20.0 point2D


zoom : Geometry.Point2D -> Geometry.Point2D
zoom point2D =
  Geometry.zoom 10.0 point2D


project : Geometry.Point3D -> Geometry.Point2D
project point3D =
  Geometry.project 0.2 point3D


transform : Float -> Float -> Geometry.Point3D -> Geometry.Point3D
transform angle1 angle2 point3D =
  point3D
    |> Geometry.rotateX (Geometry.degToRad angle1)
    |> Geometry.rotateY (Geometry.degToRad angle2)
    |> Geometry.rotateZ (Geometry.degToRad 10)

transformFace : Float -> Float -> List Geometry.Point3D -> List Geometry.Point3D
transformFace angle1 angle2 face =
  List.map (transform angle1 angle2) face


toScreen : Geometry.Point3D -> Geometry.Point2D
toScreen point3D =
  point3D
    |> project
    |> zoom
    |> translate


faceToScreen : (Float, List Geometry.Point3D) -> (Float, List Geometry.Point2D)
faceToScreen (light, face) =
  (light, List.map toScreen face)


pToStr : Geometry.Point2D -> String
pToStr p = (toString p.x)  ++ "," ++ (toString p.y)


pvecToStr : List Geometry.Point2D -> String
pvecToStr pvec = List.map pToStr pvec
              |> List.intersperse " "
              |> List.foldl (++) ""


polyToSvg : (Float, List Geometry.Point2D) -> Svg
polyToSvg (light, poly) =
  polygon [ fill (illuminationToColour light), points (pvecToStr poly) ] []


facesCamera : List Geometry.Point3D -> Bool
facesCamera face =
  let n = Geometry.normal face
  in
    True --    n.z > 0


illumination : List Geometry.Point3D -> Float
illumination face =
  Geometry.cosAngle (Geometry.normal face) Model.lightVector

illuminationToColour : Float -> String
illuminationToColour l =
   let r = toString( round (64 + 128 * l) )
       g = toString( round (32 + 64 * l) )
       b = toString( round (64 + 80 * l) )
   in
      "rgb(" ++ r ++ "," ++ g ++ "," ++ b ++ ")"

illuminate : List Geometry.Point3D -> (Float, List Geometry.Point3D)
illuminate face =
  (illumination face, face)



-- MAIN

teatime : (Int,Int) -> Html
teatime (mouseX,mouseY) =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 100 100" ]
    (Model.faces
--      |> List.map physics
      |> List.map (transformFace (toFloat mouseX) (toFloat mouseY))
      |> List.filter (\face -> facesCamera face)
      |> List.sortBy Geometry.meanZ
      |> List.map illuminate
      |> List.reverse
      |> List.map faceToScreen
      |> List.map polyToSvg)

main : Signal Html
main =
  Signal.map teatime Model.mousePos

