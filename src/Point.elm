module Point exposing (..)


type Point
    = Point Int Int


add : Point -> Point -> Point
add (Point y1 x1) (Point y2 x2) =
    Point (y1 + y2) (x1 + x2)


up : Point
up =
    Point -1 0


down : Point
down =
    Point 1 0


left : Point
left =
    Point 0 -1


right : Point
right =
    Point 0 1


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
