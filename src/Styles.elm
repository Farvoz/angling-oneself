module Styles exposing (..)

import Css exposing (..)
import Css.Global exposing (global)
import Html.Styled
import ThemeTokens exposing (..)

-- Constants
cardSize : Px
cardSize =
    px 48

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
cardSlotStyle : Style
cardSlotStyle =
    batch
        [ cardWidth
        , cardHeight
        , border3 (px 2) dashed borderMuted
        , borderRadius (px 6)
        , flexCenter
        , backgroundColor surfaceMuted
        ]

cardTerrainStyle : Style
cardTerrainStyle =
    batch
        [ cardWidth
        , cardHeight
        , border3 (px 2) solid borderDefault
        , borderRadius (px 6)
        , position relative
        , flexCenter
        , backgroundColor surfaceDefault
        , color textPrimary
        , fontSize (rem 1.1)
        , fontWeight bold
        ]

cardBaitStyle : Style
cardBaitStyle =
    batch
        [ cardWidth
        , cardHeight
        , border3 (px 2) solid borderDefault
        , borderRadius (px 6)
        , flexCenter
        , backgroundColor surfaceAccent
        , color textOnAccent
        , fontSize (rem 0.75)
        ]

notchStyle : Style
notchStyle =
    batch
        [ position absolute
        , width (px 6)
        , height (px 6)
        , backgroundColor borderDefault
        , borderRadius (pct 50)
        , flexCenter
        , fontSize (rem 0.45)
        , color textOnDark
        ]

-- Button style
btnStyle : Style
btnStyle =
    batch
        [ minHeight (px 44)
        , fontSize (rem 1)
        , cursor pointer
        , backgroundColor surfaceMuted
        , color textPrimary
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
