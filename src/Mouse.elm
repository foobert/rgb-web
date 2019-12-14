module Mouse exposing (MouseEvent, onMouse)

import Html
import Html.Events
import Json.Decode exposing (Decoder, andThen, field, float, map3, maybe, succeed)


type alias MouseEvent =
    { clientX : Float
    , clientY : Float
    , deltaY : Maybe Float
    }


onMouse : String -> (MouseEvent -> msg) -> Html.Attribute msg
onMouse event msg =
    Html.Events.on ("mouse" ++ String.toLower event) (wrapMouseEvent msg)


mouseEventDecoder : Decoder MouseEvent
mouseEventDecoder =
    map3 MouseEvent
        (field "clientX" float)
        (field "clientY" float)
        (maybe (field "deltaY" float))


wrapMouseEvent : (MouseEvent -> msg) -> Decoder msg
wrapMouseEvent msg =
    mouseEventDecoder |> andThen (\x -> succeed (msg x))
