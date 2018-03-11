module Point exposing (..)


type Point
    = Point Int Int


asTuple : Point -> ( Int, Int )
asTuple (Point x y) =
    ( x, y )


add : Point -> Point -> Point
add (Point x1 y1) (Point x2 y2) =
    Point (x1 + x2) (y1 + y2)


getX : Point -> Int
getX (Point x _) =
    x


getY : Point -> Int
getY (Point _ y) =
    y


getAngle : Point -> Point -> Float
getAngle (Point x1 y1) (Point x2 y2) =
    atan2 (toFloat (x1 * y2 - x2 * y1)) (toFloat (y1 * y2 + x1 * x2))


up : Point
up =
    Point 0 1


down : Point
down =
    Point 0 -1


left : Point
left =
    Point -1 0


right : Point
right =
    Point 1 0


origin : Point
origin =
    Point 0 0


inRange : Point -> Int -> Int -> Bool
inRange (Point x y) mx my =
    y
        < my
        && y
        >= 0
        && x
        < mx
        && x
        >= 0
