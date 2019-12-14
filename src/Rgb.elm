module Rgb exposing (calculateColor, fromHSL, initColor, randomColor)

import Color exposing (Color)
import Random


randomColor : Random.Generator Color
randomColor =
    Random.map3
        fromHSL
        (Random.float 0 359)
        (Random.float 0 100)
        (Random.constant 50.0)


calculateColor { x, y, width, height, lightness } =
    fromHSL
        ((x / width) * 360)
        (100 - (y / height) * 100)
        lightness


fromHSL : Float -> Float -> Float -> Color
fromHSL h s l =
    Color.fromHSL ( h, s, l )


initColor : Color
initColor =
    fromHSL 0 0 50
