module Svg.Attributes exposing (height, rx, ry, viewBox, width, x, x1, x2, y, y1, y2)

import Svg.Styled exposing (Attribute)
import Svg.Styled.Attributes


x : Float -> Attribute msg
x =
    String.fromFloat >> Svg.Styled.Attributes.x


y : Float -> Attribute msg
y =
    String.fromFloat >> Svg.Styled.Attributes.y


width : Float -> Attribute msg
width =
    String.fromFloat >> Svg.Styled.Attributes.width


height : Float -> Attribute msg
height =
    String.fromFloat >> Svg.Styled.Attributes.height


rx : Float -> Attribute msg
rx =
    String.fromFloat >> Svg.Styled.Attributes.rx


ry : Float -> Attribute msg
ry =
    String.fromFloat >> Svg.Styled.Attributes.ry


viewBox : Float -> Float -> Float -> Float -> Attribute msg
viewBox x_ y_ width_ height_ =
    [ x_, y_, width_, height_ ]
        |> List.map String.fromFloat
        |> String.join " "
        |> Svg.Styled.Attributes.viewBox



-- line : Float -> Float -> Float -> Float -> List (Attribute msg) -> List (Svg msg) -> Svg msg
-- line x1_ y1_ x2_ y2_ attributes elements =
--     Svg.Styled.line
--         ([ x1 x1_
--          , y1 y1_
--          , x2 x2_
--          , y2 y2_
--          ]
--             ++ attributes
--         )
--         elements
-- line2 : { x1 : Float, y1 : Float, x2 : Float, y2 : Float } -> List (Attribute msg) -> List (Svg msg) -> Svg msg
-- line2 coordinates attributes elements =
--     Svg.Styled.line
--         ([ x1 coordinates.x1
--          , y1 coordinates.y1
--          , x2 coordinates.x2
--          , y2 coordinates.y2
--          ]
--             ++ attributes
--         )
--         elements


x1 : Float -> Attribute msg
x1 =
    String.fromFloat >> Svg.Styled.Attributes.x1


y1 : Float -> Attribute msg
y1 =
    String.fromFloat >> Svg.Styled.Attributes.y1


x2 : Float -> Attribute msg
x2 =
    String.fromFloat >> Svg.Styled.Attributes.x2


y2 : Float -> Attribute msg
y2 =
    String.fromFloat >> Svg.Styled.Attributes.y2
