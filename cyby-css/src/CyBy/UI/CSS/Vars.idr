module CyBy.UI.CSS.Vars

import public Text.CSS

%default total

||| Variables to be used in CSS styling.
public export
record Vars where
  [noHints]
  constructor V

  -- Colours (lower numbers mean brighter colours)
  gray10              : Color
  gray20              : Color
  gray50              : Color
  gray80              : Color
  gray90              : Color

  primary10           : Color
  primary20           : Color
  primary50           : Color
  primary80           : Color
  primary90           : Color

  secondary10         : Color
  secondary20         : Color
  secondary50         : Color
  secondary80         : Color
  secondary90         : Color

  boron               : Color
  bromine             : Color
  carbon              : Color
  chlorine            : Color
  fluorine            : Color
  nitrogen            : Color
  oxygen              : Color
  phosphorous         : Color
  sulfur              : Color

  --    containers --
  gap                 : Length       -- preferred gap in grids and flex boxes
  bardim              : Length       -- height of a horizontal toolbar
  titleHeight         : Width        -- height of a component title
  formLblWidth        : Width        -- width of a label in a form
  barSepwidth         : Width        -- width of a separator in a toolbar
  formSepwidth        : Width        -- width of field separator in a form

  --    Fontsizes --
  smallFont           : FontSize
  xsmallFont          : FontSize
  xxsmallFont         : FontSize

  --    widgets --
  cornerRad           : BorderRadius -- preferred border radius
  narrowBW            : BorderWidth  -- narrow border width
  fatBW               : BorderWidth  -- broader border for major elements
  padding             : Length
  largePadding        : Length

export
defaultVars : Vars
defaultVars =
  V {
    gray10              = hsl 240 10.perc 95.perc
  , gray20              = hsl 240 10.perc 90.perc
  , gray50              = hsl 240 10.perc 55.perc
  , gray80              = hsl 240 10.perc 25.perc
  , gray90              = hsl 240 10.perc 10.perc

  , primary10           = hsl 240 20.perc 95.perc
  , primary20           = hsl 240 30.perc 90.perc
  , primary50           = hsl 240 80.perc 40.perc
  , primary80           = hsl 240 60.perc 25.perc
  , primary90           = hsl 240 40.perc 10.perc

  , secondary10         = hsl 60 20.perc 95.perc
  , secondary20         = hsl 60 30.perc 90.perc
  , secondary50         = hsl 60 80.perc 40.perc
  , secondary80         = hsl 60 60.perc 25.perc
  , secondary90         = hsl 60 40.perc 10.perc

  , boron               = rgb 0xff 0xb5 0xb5
  , carbon              = dimgray
  , fluorine            = limegreen
  , sulfur              = rgb 0xE6 0xC6 0x40
  , oxygen              = rgb 0xFF 0x0D 0x0D
  , nitrogen            = rgb 0x30 0x50 0xF8
  , chlorine            = rgb 0x1F 0xF0 0x1F
  , bromine             = rgb 0xA6 0x29 0x29
  , phosphorous         = rgb 0xFF 0x80 0x00

  , gap                 = 0.4.em
  , bardim              = 2.0.em
  , titleHeight         = 1.5.em
  , formLblWidth        = 6.em
  , barSepwidth         = 3.px
  , formSepwidth        = 1.px

  , smallFont           = 0.89.em
  , xsmallFont          = 0.75.em
  , xxsmallFont         = 0.60.em

  , cornerRad           = 0.25.em
  , narrowBW            = 1.px
  , fatBW               = 2.px
  , padding             = 0.4.em
  , largePadding        = 0.6.em
  }

--------------------------------------------------------------------------------
-- Semantic Variables
--------------------------------------------------------------------------------

parameters {auto v : Vars}

  ||| Default background colour
  export %inline
  bg : Color
  bg = v.primary10

  ||| Default text colour
  export %inline
  fg : Color
  fg = v.primary90

  export
  widgetFG : Color
  widgetFG = v.primary80

  export
  widgetBG : Color
  widgetBG = v.primary20

  export
  activeFG : Color
  activeFG = v.primary50

  export
  activeBG : Color
  activeBG = v.primary10

  export
  disabledFG : Color
  disabledFG = v.gray80

  export
  disabledBG : Color
  disabledBG = v.gray20

  export
  bar : Color
  bar = v.secondary50
