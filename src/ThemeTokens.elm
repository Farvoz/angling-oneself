module ThemeTokens exposing (..)

import Css exposing (..)



-- Design tokens (color scheme)
-- Palette: water, river, swamp — небо/мелководье/камыш, глубина, берег, приманка
-- surfacePage #eef4f8 | surfaceDefault #d8e6e8 | surfaceMuted #b8c9c4 | surfaceAccent #2fa4ff
-- textPrimary #1e3a3f | textSecondary #2d4a4a | textMuted #5a6e66 | textOnAccent #1e2a1a
-- borderDefault #2d5a52 | borderMuted #7a9a8e | success #1a6b4a | error #b84a4a | textOnDark #ffffff


surface =
    { page = hex "eef4f8"
    , default = hex "d8e6e8"
    , muted = hex "b8c9c4"
    , accent = hex "2fa4ff"
    }


textColor =
    { primary = hex "1e3a3f"
    , secondary = hex "2d4a4a"
    , muted = hex "5a6e66"
    , onAccent = hex "1e2a1a"
    , onDark = hex "ffffff"
    }


border =
    { default = hex "2d5a52"
    , muted = hex "7a9a8e"
    }


semantic =
    { success = hex "1a6b4a"
    , error = hex "b84a4a"
    }



-- Размеры


borderRadiusDefault : Px
borderRadiusDefault =
    px 6
