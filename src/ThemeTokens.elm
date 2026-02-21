module ThemeTokens exposing (..)

import Css exposing (..)

-- Design tokens (color scheme)
-- Palette: water, river, swamp — небо/мелководье/камыш, глубина, берег, приманка
-- surfacePage #eef4f8 | surfaceDefault #d8e6e8 | surfaceMuted #b8c9c4 | surfaceAccent #d4a84b
-- textPrimary #1e3a3f | textSecondary #2d4a4a | textMuted #5a6e66 | textOnAccent #1e2a1a
-- borderDefault #2d5a52 | borderMuted #7a9a8e | success #1a6b4a | error #b84a4a | textOnDark #ffffff

surfacePage : Color
surfacePage =
    hex "eef4f8"

surfaceDefault : Color
surfaceDefault =
    hex "d8e6e8"

surfaceMuted : Color
surfaceMuted =
    hex "b8c9c4"

surfaceAccent : Color
surfaceAccent =
    hex "d4a84b"

textPrimary : Color
textPrimary =
    hex "1e3a3f"

textSecondary : Color
textSecondary =
    hex "2d4a4a"

textMuted : Color
textMuted =
    hex "5a6e66"

textOnAccent : Color
textOnAccent =
    hex "1e2a1a"

borderDefault : Color
borderDefault =
    hex "2d5a52"

borderMuted : Color
borderMuted =
    hex "7a9a8e"

semanticSuccess : Color
semanticSuccess =
    hex "1a6b4a"

semanticError : Color
semanticError =
    hex "b84a4a"

-- For notch on dark border: white for contrast
textOnDark : Color
textOnDark =
    hex "ffffff"
