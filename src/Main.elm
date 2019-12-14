port module Main exposing (..)

import Browser
import Color exposing (Color)
import Debug
import Html exposing (..)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick, onMouseOver)
import Json.Decode as Decode
import Json.Encode as Encodev
import Random


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
    ( { currentColor = fromHSL 0 0 50
      , width = flags.width
      , height = flags.height
      , x = 0
      , y = 0
      , lightness = 50
      }
    , Random.generate InitColor randomColor
    )


port updateColor : String -> Cmd msg


subscriptions _ =
    Sub.none


onMouse : String -> (MouseEvent -> msg) -> Attribute msg
onMouse event msg =
    Html.Events.on ("mouse" ++ String.toLower event) (wrapMouseEvent msg)


mouseEventDecoder : Decode.Decoder MouseEvent
mouseEventDecoder =
    Decode.map3 MouseEvent
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
        (Decode.maybe (Decode.field "deltaY" Decode.float))


wrapMouseEvent : (MouseEvent -> msg) -> Decode.Decoder msg
wrapMouseEvent msg =
    mouseEventDecoder |> Decode.andThen (\x -> Decode.succeed (msg x))


randomColor =
    Random.map3
        fromHSL
        (Random.float 0 359)
        (Random.float 0 100)
        (Random.constant 50.0)


calculateColor model =
    fromHSL
        ((model.x / model.width) * 360)
        (100 - (model.y / model.height) * 100)
        model.lightness


fromHSL : Float -> Float -> Float -> Color
fromHSL h s l =
    Color.fromHSL ( h, s, l )


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
            ( { newModel | currentColor = calculateColor newModel }, Cmd.none )

        MouseWheeled ev ->
            case ev.deltaY of
                Just dy ->
                    let
                        l =
                            max 0 (min 100 (model.lightness + dy / 10))

                        newModel =
                            { model | lightness = l }
                    in
                    ( { newModel | currentColor = calculateColor newModel }, Cmd.none )

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
