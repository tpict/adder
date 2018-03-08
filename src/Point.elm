module Point exposing (..)

import Debug exposing (log)


type Point
    = Point Int Int


add : Point -> Point -> Point
add (Point y1 x1) (Point y2 x2) =
    Point (y1 + y2) (x1 + x2)


sub : Point -> Point -> Point
sub (Point y1 x1) (Point y2 x2) =
    Point (y1 - y2) (x1 - x2)

dot : Point -> Point -> Int
dot (Point y1 x1) (Point y2 x2) =
  y1 * y2 + x1 * x2


getX : Point -> Int
getX (Point _ x) =
    x


getY : Point -> Int
getY (Point y _) =
    y


getAngle : Point -> Float
getAngle point =
  let
      dir = if getX point > 0 then -1 else 1
      angle = acos (toFloat (dot point up)) * dir
  in angle


up : Point
up =
    Point 1 0


down : Point
down =
    Point -1 0


left : Point
left =
    Point 0 -1


right : Point
right =
    Point 0 1


origin : Point
origin =
    Point 0 0


inRange : Point -> Int -> Int -> Bool
inRange (Point y x) my mx =
    y
        < my
        && y
        >= 0
        && x
        < mx
        && x
        >= 0


type DirectedPoint
    = DirectedPoint Point Point


getPosition : DirectedPoint -> Point
getPosition (DirectedPoint position _) =
    position


getDirection : DirectedPoint -> Point
getDirection (DirectedPoint _ direction) =
    direction
