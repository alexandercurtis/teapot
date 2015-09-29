module Teapot where

import Geometry

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Time

import TeapotModel as Model


-- SIGNALS

type Action = NoOp | Tick Float | Boom Bool | DoBFC Bool | DoIllumination Bool | DoRotation Bool

clock : Signal Action
clock =
  Signal.map Tick (Time.fps 24)




-- PORTS

port dnp : Signal Bool

port bfc : Signal Bool

port ill : Signal Bool

port rot : Signal Bool

port fps : Signal Int
port fps =
  (Signal.map (\m -> truncate (1000.0 / m.delta)) model)

-- VIEW

translate : Geometry.Point2D -> Geometry.Point2D
translate point2D =
  Geometry.translate2D 20.0 16.0 point2D


zoom : Geometry.Point2D -> Geometry.Point2D
zoom point2D =
  Geometry.zoom 10.0 point2D


project : Geometry.Point3D -> Geometry.Point2D
project point3D =
  Geometry.project 0.2 point3D


transform : Geometry.Point3D -> Geometry.Point3D -> Geometry.Point3D
transform rotation point3D =
  point3D
    |> Geometry.rotateX (Geometry.degToRad rotation.x)
    |> Geometry.rotateY (Geometry.degToRad rotation.y)
    |> Geometry.rotateZ (Geometry.degToRad rotation.z)

transformFace : Geometry.Point3D -> Geometry.Poly -> Geometry.Poly
transformFace rotation face =
  { face | vertices <- (List.map (transform rotation) face.vertices) }


toScreen : Geometry.Point3D -> Geometry.Point2D
toScreen point3D =
  point3D
    |> project
    |> zoom
    |> translate


faceToScreen : Geometry.Poly -> (Float, List Geometry.Point2D)
faceToScreen face =
  (face.illumination, List.map toScreen face.vertices)


pToStr : Geometry.Point2D -> String
pToStr p = (toString p.x)  ++ "," ++ (toString p.y)


pvecToStr : List Geometry.Point2D -> String
pvecToStr pvec = List.map pToStr pvec
              |> List.intersperse " "
              |> List.foldl (++) ""


polyToSvg : (Float, List Geometry.Point2D) -> Svg
polyToSvg (light, poly) =
  polygon [ fill (illuminationToColour light), points (pvecToStr poly) ] []


facesCamera : Geometry.Poly -> Bool
facesCamera face =
  let n = Geometry.normal face
  in
    n.z > 0


light : Geometry.Poly -> Float
light face =
  Geometry.cosAngle (Geometry.normal face) Model.lightVector

illuminationToColour : Float -> String
illuminationToColour l =
   let r = toString( round (64 + 128 * l) )
       g = toString( round (32 + 64 * l) )
       b = toString( round (64 + 80 * l) )
   in
      "rgb(" ++ r ++ "," ++ g ++ "," ++ b ++ ")"

illuminate : Geometry.Poly -> Geometry.Poly
illuminate face =
  { face | illumination <- (light face) }

drawTeapot : Model.Model -> Html
drawTeapot model =
  svg [ version "1.1", x "0", y "0", viewBox "0 0 100 100" ]
    (model.faces
      |> List.map (transformFace model.rotation)
      |> (if model.bfcOn then (List.filter (\face -> facesCamera face)) else (identity))
      |> List.sortBy Geometry.meanZ
      |> (if model.illuminationOn then (List.map illuminate) else (identity))
      |> List.reverse
      |> List.map faceToScreen
      |> List.map polyToSvg)

-- UPDATE

updateVertex : Geometry.Point3D -> Geometry.Point3D -> Geometry.Point3D
updateVertex v velocity =
  Geometry.Point3D (v.x + velocity.x) (v.y + velocity.y) (v.z + velocity.z)

updateFace : Geometry.Poly -> Geometry.Poly
updateFace poly =
  { poly | vertices <- (List.map (updateVertex poly.velocity) poly.vertices) }

update : Action -> Model.Model -> Model.Model
update action model =
   case action of
    NoOp ->
      model
    Boom _ ->
      explode model
    DoBFC a ->
      { model | bfcOn <- a }
    DoIllumination a ->
      { model | illuminationOn <- a }
    DoRotation a ->
      { model | rotationOn <- a }
    Tick delta ->
      if model.rotationOn then
        { model | rotation <- Geometry.Point3D (model.rotation.x + 0) (model.rotation.y + 10) (model.rotation.z + 0)
                , faces <- (List.map updateFace model.faces)
                , delta <- delta}
      else
        model



model : Signal Model.Model
model =
  Signal.foldp update Model.initialModel actions

actions : Signal Action
actions =
  Signal.mergeMany [clock, (Signal.map Boom dnp), (Signal.map DoBFC bfc), (Signal.map DoIllumination ill), (Signal.map DoRotation rot)]



-- MAIN


explodePoly : Geometry.Poly -> Geometry.Poly
explodePoly poly =
  let
    newVelocity = (Maybe.withDefault (Geometry.Point3D 0 0 0) (List.head poly.vertices))
  in
    { poly | velocity <- (Geometry.scale 0.5 (Geometry.Point3D (newVelocity.x) (newVelocity.y + 0.5) (newVelocity.z))) }

explode : Model.Model -> Model.Model
explode model =
  { model | faces <- List.map explodePoly model.faces }

main : Signal Html
main =
  Signal.map drawTeapot model


