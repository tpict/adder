module Snake exposing (..)

import List exposing (member, reverse)
import Point exposing (..)
import Set exposing (fromList, size)


{-| A segment of a Snake.

    pos: The segment's position
    dir: The segment's direction, which changes with the succeeding segment
    pdir: The segment's initial direction

-}
type alias SnakePart =
    { pos : Point, dir : Point, pdir : Point }


{-| Move the SnakePart in the given direction. The direction must be a
magnitude 1 vector. The SnakePart's direction will be updated too.

    movePart { pos = Point 1 0, dir = Point 1 0, pdir = Point 1 0 } <| Point -1 0
        == { pos = Point 0 0, dir = Point -1 0, pdir = Point -1 0 }

-}
movePart : SnakePart -> Point -> SnakePart
movePart x d =
    { x | pos = add x.pos d, dir = d, pdir = d }


type alias Snake =
    ( SnakePart, List SnakePart, SnakePart )


{-| Get the game score for a snake. A snake with a single body part has a score
of 0.

    score ( head, [ body1, body2 ], tail ) == 1

-}
score : Snake -> Int
score snake =
    positions snake
        |> List.map (\p -> asTuple p)
        |> Set.fromList
        |> Set.size
        |> (+) -3


{-| Convert a Snake to a List of SnakeParts.

    asList (head, [body1, body2], tail) == [head, body1, body2, tail]

-}
asList : Snake -> List SnakePart
asList ( h, b, t ) =
    h :: b ++ [ t ]


{-| Whether or not the head of the given Snake is within the game's boundaries.

    headInRange ( Point 4 0, [body], tail ) 5 == True
    headInRange ( Point 5 0, [body], tail ) 5 == False
    headInRange ( Point 6 0, [body], tail ) 5 == False

-}
headInRange : Snake -> Int -> Bool
headInRange ( h, _, _ ) gameSize =
    inRange h.pos gameSize gameSize


{-| Convert a Snake to a List of Points that represents the segment positions.

    positions ( { pos = Point 0 0, .. }, [ { pos = Point 1 0, .. } ], { pos = Point 2 0, .. } )
        == [ Point 0 0, Point 1 0, Point 2 0 ]

-}
positions : Snake -> List Point
positions snake =
    asList snake |> List.map (\x -> x.pos)


{-| Whether or not a Snake has collided with the given Point.

    collidedWith ( { pos = Point 0 0, .. }, [ { pos = Point 1 0, .. } ], { pos = Point 2 0, .. } ) <| Point 1 0
        == True

    collidedWith ( { pos = Point 0 0, .. }, [ { pos = Point 1 0, .. } ], { pos = Point 2 0, .. } ) <| Point -1 0
        == False

-}
collidedWith : Snake -> Point -> Bool
collidedWith ( h, _, _ ) p =
    p == h.pos


{-| Whether or not the head of a Snake has collided with the rest of it.

    collidedWithSelf ( { pos = Point 1 0, .. }, [ { pos = Point 1 0, .. } ], { pos = Point 2 0, .. } )
        == True

    collidedWithSelf ( { pos = Point 0 0, .. }, [ { pos = Point 1 0, .. } ], { pos = Point 2 0, .. } )
        == False

-}
collidedWithSelf : Snake -> Bool
collidedWithSelf ( h, b, t ) =
    h.pos
        == t.pos
        || (List.map (\sp -> sp.pos) b |> member h.pos)


{-| Grow a Snake by 1 segment in the given direction.

    grow ( { pos = Point 0 0, .. }
         , [ { pos = Point 1 0, .. } ]
         , { pos = Point 2 0, .. }
         ) <| Point -1 0
        == ( { pos = Point -1 0, .. }
           , [ { pos = Point 0 0, .. }, { pos = Point 1 0, .. } ]
           , { pos = Point 2 0, .. }
           )

-}
grow : Snake -> Point -> Snake
grow ( h, b, t ) d =
    ( movePart h d, { h | dir = d } :: b, t )


{-| Trim the tail of a Snake off.

    trim ( { pos = Point 0 0, .. }
         , [ { pos = Point 0 0, .. }, { pos = Point 1 0, .. } ]
         , { pos = Point 2 0, .. }
         )
        == ( { pos = Point -1 0, .. }
           , [ { pos = Point 0 0, .. } ]
           , { pos = Point 1 0, .. }
           )

-}
trim : Snake -> Snake
trim ( h, b, t ) =
    case reverse b of
        [] ->
            ( h, [], t )

        nt :: nb ->
            ( h, reverse nb, nt )
