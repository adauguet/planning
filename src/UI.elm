module UI exposing
    ( fontAwesomeIcon
    , h2
    , inputAttributes
    , inputLabelAttributes
    , outlinedButtonAttributes
    , plainButtonAttributes
    , radioAttributes
    , shadow
    , shadowLg
    , shadowMd
    , shadowXl
    , smallButtonAttributes
    , smallOutlinedButtonAttributes
    , textButtonAttributes
    , windowAttributes
    )

import Element
    exposing
        ( Attribute
        , Color
        , Element
        , centerX
        , centerY
        , clipY
        , el
        , fill
        , height
        , html
        , maximum
        , mouseOver
        , moveDown
        , padding
        , paddingXY
        , rgba255
        , scrollbarY
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (i)
import Html.Attributes exposing (class)
import Tailwind


shadow : Attribute msg
shadow =
    shadows
        [ { offset = ( 0, 1 )
          , blur = 3
          , size = 0
          , color = rgba255 0 0 0 0.1
          }
        , { offset = ( 0, 1 )
          , blur = 2
          , size = 0
          , color = rgba255 0 0 0 0.06
          }
        ]


shadowMd : Attribute msg
shadowMd =
    shadows
        [ { offset = ( 0, 4 )
          , blur = 6
          , size = -1
          , color = rgba255 0 0 0 0.1
          }
        , { offset = ( 0, 2 )
          , blur = 4
          , size = -1
          , color = rgba255 0 0 0 0.06
          }
        ]


shadowLg : Attribute msg
shadowLg =
    shadows
        [ { offset = ( 0, 10 )
          , blur = 15
          , size = -3
          , color = rgba255 0 0 0 0.1
          }
        , { offset = ( 0, 4 )
          , blur = 6
          , size = -2
          , color = rgba255 0 0 0 0.05
          }
        ]


shadowXl : Attribute msg
shadowXl =
    shadows
        [ { offset = ( 0, 20 )
          , blur = 25
          , size = -5
          , color = rgba255 0 0 0 0.1
          }
        , { offset = ( 0, 10 )
          , blur = 10
          , size = -5
          , color = rgba255 0 0 0 0.04
          }
        ]


shadows : List Shadow -> Attribute msg
shadows list =
    let
        description =
            list
                |> List.map shadowToString
                |> String.join ", "
    in
    Element.htmlAttribute <| Html.Attributes.style "box-shadow" description


type alias Shadow =
    { offset : ( Int, Int )
    , size : Int
    , blur : Int
    , color : Color
    }


shadowToString : { offset : ( Int, Int ), size : Int, blur : Int, color : Color } -> String
shadowToString { offset, size, blur, color } =
    String.join " "
        [ intToPx <| Tuple.first offset
        , intToPx <| Tuple.second offset
        , intToPx blur
        , intToPx size
        , colorToRgbaString color
        ]


intToPx : Int -> String
intToPx int =
    case int of
        0 ->
            "0"

        i ->
            String.fromInt i ++ "px"


colorToRgbaString : Color -> String
colorToRgbaString color =
    let
        { red, green, blue, alpha } =
            Element.toRgb color

        rgb =
            [ red, green, blue ] |> List.map ((*) 255 >> String.fromFloat) |> List.intersperse ", " |> List.foldl (++) ""
    in
    "rgba(" ++ rgb ++ "," ++ String.fromFloat alpha ++ ")"


fontAwesomeIcon : String -> Element msg
fontAwesomeIcon description =
    el [] <| html (i [ class description ] [])


inputLabelAttributes : List (Attribute msg)
inputLabelAttributes =
    [ Font.color Tailwind.gray700
    , Font.size 12
    ]


plainButtonAttributes : List (Attribute msg)
plainButtonAttributes =
    [ paddingXY 16 12
    , Background.color Tailwind.blue700
    , Font.color Tailwind.white
    , Font.size 13
    , Border.rounded 4
    , shadowLg
    , Font.medium
    ]


smallButtonAttributes : List (Attribute msg)
smallButtonAttributes =
    [ Border.rounded 4
    , Border.width 1
    , Border.color Tailwind.blue700
    , shadowLg
    , Font.medium
    , paddingXY 12 8
    , Background.color Tailwind.blue700
    , Font.color Tailwind.white
    , Font.size 13
    ]


inputAttributes : List (Attribute msg)
inputAttributes =
    [ padding 8, Background.color Tailwind.gray300 ]


outlinedButtonAttributes : List (Attribute msg)
outlinedButtonAttributes =
    [ Border.rounded 4
    , shadowLg
    , Font.medium
    , paddingXY 16 12
    , Border.width 1
    , Border.color Tailwind.blue700
    , Background.color Tailwind.white
    , Font.color Tailwind.blue700
    ]


smallOutlinedButtonAttributes : List (Attribute msg)
smallOutlinedButtonAttributes =
    [ paddingXY 12 8
    , Border.rounded 4
    , Border.width 1
    , Border.color Tailwind.blue700
    , Background.color Tailwind.white
    , Font.medium
    , Font.color Tailwind.blue700
    , Font.size 13
    ]


textButtonAttributes : List (Attribute msg)
textButtonAttributes =
    [ paddingXY 8 8
    , Font.color Tailwind.blue700
    , Font.medium
    , Border.rounded 4
    , mouseOver [ Background.color Tailwind.gray200 ]
    ]


h2 : String -> Element msg
h2 title =
    el [ Font.size 18, Font.heavy ] <| text title


windowAttributes : List (Attribute msg)
windowAttributes =
    [ centerX
    , centerY
    , spacing 24
    , width (fill |> maximum 400)
    , padding 36
    , Border.width 1
    , Border.color Tailwind.gray400
    , Border.rounded 10
    ]


radioAttributes : List (Attribute msg)
radioAttributes =
    [ paddingXY 0 8
    , Background.color Tailwind.white
    , shadow
    , height (fill |> maximum 200)
    , clipY
    , scrollbarY
    , moveDown 4
    , width fill
    ]
