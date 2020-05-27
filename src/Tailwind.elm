module Tailwind exposing
    ( blue100
    , blue200
    , blue300
    , blue400
    , blue500
    , blue600
    , blue700
    , blue800
    , blue900
    , gray100
    , gray200
    , gray300
    , gray400
    , gray500
    , gray600
    , gray700
    , gray800
    , gray900
    , red100
    , red200
    , red300
    , red400
    , red500
    , red600
    , red700
    , red800
    , red900
    , rounded
    , roundedLarge
    , roundedSmall
    , shadow
    , shadow2Xl
    , shadowInner
    , shadowLg
    , shadowMd
    , shadowXl
    , white
    )

import Css exposing (Color, Style, borderRadius, hex, inset, px, rem, rgb, rgba, zero)


shadow : Style
shadow =
    Css.property "box-shadow" "0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)"


shadowMd : Style
shadowMd =
    Css.property "box-shadow" "0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)"


shadowLg : Style
shadowLg =
    Css.property "box-shadow" "0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05)"


shadowXl : Style
shadowXl =
    Css.property "box-shadow" "0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)"


shadow2Xl : Style
shadow2Xl =
    Css.boxShadow5 zero (px 25) (px 50) (px -12) (rgba 0 0 0 0.25)


shadowInner : Style
shadowInner =
    Css.boxShadow6 inset zero (px 2) (px 4) zero (rgba 0 0 0 0.06)


white : Color
white =
    rgb 255 255 255


blue100 : Color
blue100 =
    hex "#EBF8FF"


blue200 : Color
blue200 =
    hex "#BEE3F8"


blue300 : Color
blue300 =
    hex "#90CDF4"


blue400 : Color
blue400 =
    hex "#63B3ED"


blue500 : Color
blue500 =
    hex "#4299E1"


blue600 : Color
blue600 =
    hex "#3182CE"


blue700 : Color
blue700 =
    hex "#2B6CB0"


blue800 : Color
blue800 =
    hex "#2C5282"


blue900 : Color
blue900 =
    hex "#2A4365"


gray100 : Color
gray100 =
    hex "#F7FAFC"


gray200 : Color
gray200 =
    hex "#EDF2F7"


gray300 : Color
gray300 =
    hex "#E2E8F0"


gray400 : Color
gray400 =
    hex "#CBD5E0"


gray500 : Color
gray500 =
    hex "#A0AEC0"


gray600 : Color
gray600 =
    hex "#718096"


gray700 : Color
gray700 =
    hex "#4A5568"


gray800 : Color
gray800 =
    hex "#2D3748"


gray900 : Color
gray900 =
    hex "#1A202C"


roundedSmall : Style
roundedSmall =
    borderRadius (rem 0.125)


rounded : Style
rounded =
    borderRadius (rem 0.25)


roundedLarge : Style
roundedLarge =
    borderRadius (rem 0.5)


red100 : Color
red100 =
    hex "#FFF5F5"


red200 : Color
red200 =
    hex "#FED7D7"


red300 : Color
red300 =
    hex "#FEB2B2"


red400 : Color
red400 =
    hex "#FC8181"


red500 : Color
red500 =
    hex "#F56565"


red600 : Color
red600 =
    hex "#E53E3E"


red700 : Color
red700 =
    hex "#C53030"


red800 : Color
red800 =
    hex "#9B2C2C"


red900 : Color
red900 =
    hex "#742A2A"
