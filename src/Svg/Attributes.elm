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
