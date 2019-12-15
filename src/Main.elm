port module Main exposing (..)

import Browser
import Color exposing (Color)
import Debug
import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick, onMouseOver)
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as Decode
import Json.Encode as Encodev
import Random
import Rgb


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { currentColor : Color
    , width : Float
    , height : Float
    , x : Float
    , y : Float
    , lightness : Float
    }


type alias MouseEvent =
    { clientX : Float
    , clientY : Float
    , deltaY : Maybe Float
    }


type Msg
    = MouseMoved MouseEvent
    | MouseWheeled MouseEvent
    | InitColor Color
    | SelectColor


init : { width : Float, height : Float } -> ( Model, Cmd Msg )
init flags =
    ( { currentColor = Rgb.initColor
      , width = flags.width
      , height = flags.height
      , x = 0
      , y = 0
      , lightness = 50
      }
    , Random.generate InitColor Rgb.randomColor
    )


port updateColor : String -> Cmd msg


subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitColor c ->
            ( { model | currentColor = c }, Cmd.none )

        MouseMoved ev ->
            let
                newModel =
                    { model | x = ev.clientX, y = ev.clientY }
            in
            ( { newModel | currentColor = Rgb.calculateColor newModel }, Cmd.none )

        MouseWheeled ev ->
            case ev.deltaY of
                Just dy ->
                    let
                        l =
                            max 0 (min 100 (model.lightness + dy / 10))

                        newModel =
                            { model | lightness = l }
                    in
                    ( { newModel | currentColor = Rgb.calculateColor newModel }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SelectColor ->
            ( model, updateColor (Color.toHex model.currentColor) )


touchCoordinates touchEvent =
    let
        pos =
            List.head touchEvent.changedTouches
                |> Maybe.map .clientPos
                |> Maybe.withDefault ( 0, 0 )
    in
    { clientX = Tuple.first pos, clientY = Tuple.second pos, deltaY = Nothing }


mouseCoordinates ev =
    { clientX = Tuple.first ev.clientPos, clientY = Tuple.second ev.clientPos, deltaY = Nothing }


wheelCoordinates : Wheel.Event -> MouseEvent
wheelCoordinates ev =
    { clientX = 0, clientY = 0, deltaY = Just ev.deltaY }


view model =
    div
        [ id "picker"
        , Mouse.onMove (MouseMoved << mouseCoordinates)
        , Touch.onMove (MouseMoved << touchCoordinates)
        , Touch.onEnd (\_ -> SelectColor)
        , Wheel.onWheel (MouseWheeled << wheelCoordinates)
        , onClick SelectColor
        , style "backgroundColor" (Color.toRGBString model.currentColor)
        ]
        []
