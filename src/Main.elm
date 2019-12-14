port module Main exposing (..)

import Browser
import Color exposing (Color)
import Debug
import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick, onMouseOver)
import Json.Decode as Decode
import Json.Encode as Encodev
import Mouse exposing (MouseEvent, onMouse)
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


view model =
    div
        [ id "picker"
        , onMouse "move" MouseMoved
        , onMouse "wheel" MouseWheeled
        , onClick SelectColor
        , style "backgroundColor" (Color.toRGBString model.currentColor)
        ]
        []
