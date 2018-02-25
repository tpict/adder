module Snake exposing (..)

import Html exposing (Attribute, Html, program, div, input, text)
import Html.Attributes exposing (attribute, property, style)
import Html.Events exposing (on, keyCode, onInput)
import Json.Encode exposing (string)
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Process
import Time exposing (second)
import GameState exposing (..)
import Point exposing (..)


tickDur : Float
tickDur =
    second * 0.25


textHtml : String -> Html msg
textHtml t =
    div
        [ string t
            |> property "innerHTML"
        ]
        []


type alias Model =
    { state : GameState }


type Msg
    = NoOp
    | KeyDown Int
    | Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyDown key ->
            ( { model | state = handleInput key model.state }, Cmd.none )

        Move ->
            ( { model | state = move model.state }, loop )


tabindex : Attribute msg
tabindex =
    attribute "tabindex" "0"


view : Model -> Html Msg
view model =
    div [ monoStyle, tabindex, onKeyDown KeyDown ] [ textHtml <| toStr model.state ]


loop : Cmd Msg
loop =
    Process.sleep tickDur |> Task.perform (\_ -> Move)


main : Program Never Model Msg
main =
    program
        { init = ( { state = initState }, loop )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


monoStyle : Attribute Msg
monoStyle =
    style
        [ ( "font-family", "monospace" )
        , ( "font-size", "48px" )
        ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


handleInput : Int -> GameState -> GameState
handleInput c =
    case c of
        -- up arrow
        38 ->
            changeDir <| Just up

        -- w key
        87 ->
            changeDir <| Just up

        -- left arrow
        37 ->
            changeDir <| Just left

        -- a key
        65 ->
            changeDir <| Just left

        -- down arrow
        40 ->
            changeDir <| Just down

        -- s key
        83 ->
            changeDir <| Just down

        -- right arrow
        39 ->
            changeDir <| Just right

        -- d key
        68 ->
            changeDir <| Just right

        -- r key
        82 ->
            reset

        _ ->
            identity
