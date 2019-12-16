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


type alias Position =
    { x : Float, y : Float }


type Msg
    = InitColor Color
    | UpdatePosition Position
    | UpdateLightness Float
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

        UpdatePosition pos ->
            let
                newModel =
                    { model | x = pos.x, y = pos.y }
            in
            ( { newModel | currentColor = Rgb.calculateColor newModel }, Cmd.none )

        UpdateLightness dy ->
            let
                l =
                    max 0 (min 100 (model.lightness + dy / 10))

                newModel =
                    { model | lightness = l }
            in
            ( { newModel | currentColor = Rgb.calculateColor newModel }, Cmd.none )

        SelectColor ->
            ( model, updateColor (Color.toHex model.currentColor) )


touchCoordinates touchEvent =
    let
        ( x, y ) =
            List.head touchEvent.changedTouches
                |> Maybe.map .clientPos
                |> Maybe.withDefault ( 0, 0 )
    in
    Position x y


mouseCoordinates ev =
    let
        ( x, y ) =
            ev.clientPos
    in
    Position x y


view model =
    div
        [ id "picker"
        , Mouse.onMove (UpdatePosition << mouseCoordinates)
        , Touch.onMove (UpdatePosition << touchCoordinates)
        , Touch.onEnd (\_ -> SelectColor)
        , Wheel.onWheel (\ev -> UpdateLightness ev.deltaY)
        , onClick SelectColor
        , style "backgroundColor" (Color.toRGBString model.currentColor)
        ]
        []
