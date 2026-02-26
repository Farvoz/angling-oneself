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
        , backgroundColor surface.muted
        ]


cardTerrainStyle : Style
cardTerrainStyle =
    batch
        [ cardStyle
        , backgroundColor surface.default
        , color textColor.primary
        ]


cardFishOutlineStyle : Style
cardFishOutlineStyle =
    batch
        [ cardStyle
        , backgroundColor surface.accent
        , color textColor.onAccent
        ]


cardTechniqueStyle : Style
cardTechniqueStyle =
    batch
        [ cardStyle
        , backgroundColor surface.muted
        , color textColor.primary
        , border3 (px 2) solid ThemeTokens.border.default
        ]


notchStyle : Style
notchStyle =
    batch
        [ position absolute
        , width (px 10)
        , height (px 10)
        , backgroundColor ThemeTokens.border.default
        , borderRadius (pct 50)
        , flexCenter
        , color textColor.onDark
        ]



-- Button style


btnStyle : Style
btnStyle =
    batch
        [ minHeight (px 44)
        , padding2 (rem 1) (rem 1)
        , cursor pointer
        , backgroundColor surface.default
        , color textColor.primary
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
        , backgroundColor surface.page
        , color textColor.primary
        ]



-- Global styles (html, body from index.html)


globalStyles : Html.Styled.Html msg
globalStyles =
    global
        [ Css.Global.html [ height (pct 100), overflow hidden, padding zero, margin zero ]
        , Css.Global.body [ height (pct 100), padding zero, margin zero, backgroundColor surface.page, color textColor.primary, property "-webkit-tap-highlight-color" "transparent" ]
        , Css.Global.selector "#elm" [ height (pct 100) ]
        ]
