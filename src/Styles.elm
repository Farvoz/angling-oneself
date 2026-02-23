module Styles exposing (..)

import Css exposing (..)
import Css.Global exposing (global)
import Html.Styled
import ThemeTokens exposing (..)

-- Constants
cardSize : Rem
cardSize =
    rem 3

cardWidth : Style
cardWidth =
    width cardSize

cardHeight : Style
cardHeight =
    height cardSize

-- Layout
flexColumn : Style
flexColumn =
    batch [ displayFlex, flexDirection column ]

flexCenter : Style
flexCenter =
    batch [ displayFlex, alignItems center, justifyContent center ]

-- Card styles
cardStyle : Style
cardStyle = 
    batch 
        [ cardWidth
        , cardHeight
        , borderRadius borderRadiusDefault
        , position relative
        , flexCenter
        , fontWeight bold
        ]

cardSlotStyle : Style
cardSlotStyle =
    batch
        [ cardStyle
        , backgroundColor surfaceMuted
        ]


cardTerrainStyle : Style
cardTerrainStyle =
    batch 
        [ cardStyle
        , backgroundColor surfaceDefault
        , color textPrimary
        ]

cardFishOutlineStyle : Style
cardFishOutlineStyle =
    batch
        [ cardStyle
        , backgroundColor surfaceAccent
        , color textOnAccent
        ]

notchStyle : Style
notchStyle =
    batch
        [ position absolute
        , width (px 10)
        , height (px 10)
        , backgroundColor borderDefault
        , borderRadius (pct 50)
        , flexCenter
        , color textOnDark
        ]

-- Button style
btnStyle : Style
btnStyle =
    batch
        [ minHeight (px 44)
        , padding2 (rem 1) (rem 1)
        , cursor pointer
        , backgroundColor surfaceDefault
        , color textPrimary
        , borderStyle none
        , borderRadius borderRadiusDefault
        ]

-- Root layout
rootStyle : Style
rootStyle =
    batch
        [ height (pct 100)
        , overflow hidden
        , displayFlex
        , flexDirection column
        , justifyContent center
        , padding (rem 0.75)
        , fontFamilies [ "sans-serif" ]
        , property "box-sizing" "border-box"
        , backgroundColor surfacePage
        , color textPrimary
        ]

-- Global styles (html, body from index.html)
globalStyles : Html.Styled.Html msg
globalStyles =
    global
        [ Css.Global.html [ height (pct 100), overflow hidden, padding zero, margin zero ]
        , Css.Global.body [ height (pct 100), padding zero, margin zero, backgroundColor surfacePage, color textPrimary, property "-webkit-tap-highlight-color" "transparent" ]
        , Css.Global.selector "#elm" [ height (pct 100) ]
        ]
