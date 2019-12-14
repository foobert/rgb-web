module Mouse exposing (MouseEvent, onMouse)

import Html
import Html.Events
import Json.Decode as Decode


type alias MouseEvent =
    { clientX : Float
    , clientY : Float
    , deltaY : Maybe Float
    }


onMouse : String -> (MouseEvent -> msg) -> Html.Attribute msg
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
