module CyBy.UI.CSS.Vars

import public CyBy.UI.CSS.Tailwind

%default total

||| Variables to be used in CSS styling.
public export
record Vars where
  [noHints]
  constructor V

  -- Colours (lower numbers mean brighter colours)
  gray                : TailwindColor
  primary             : TailwindColor
  secondary           : TailwindColor
  errorColor          : Color
  warnColor           : Color
  infoColor           : Color
  debugColor          : Color
  traceColor          : Color

  --    Fontsizes --
  smallFont           : FontSize
  xsmallFont          : FontSize
  xxsmallFont         : FontSize

  --    Logging --
  levelWidth          : Width

  --    containers --
  gap                 : Length       -- preferred gap in grids and flex boxes
  bardim              : Length       -- height of a horizontal toolbar
  titleHeight         : Width        -- height of a component title
  formLblWidth        : Width        -- width of a label in a form
  barSepwidth         : Width        -- width of a separator in a toolbar
  formSepwidth        : Width        -- width of field separator in a form

  --    widgets --
  cornerRad           : BorderRadius -- preferred border radius
  narrowBW            : BorderWidth  -- narrow border width
  fatBW               : BorderWidth  -- broader border for major elements
  padding             : Length
  smallPadding        : Length
  largePadding        : Length

export
defaultVars : Vars
defaultVars =
  V {
    gray                = twNeutral
  , primary             = twEmerald
  , secondary           = twYellow
  , errorColor          = twRed.c700
  , warnColor           = twYellow.c700
  , infoColor           = twEmerald.c700
  , debugColor          = twSky.c700
  , traceColor          = twNeutral.c500

  , smallFont           = 0.89.em
  , xsmallFont          = 0.75.em
  , xxsmallFont         = 0.60.em

  , levelWidth          = 5.em

  , gap                 = 0.4.em
  , bardim              = 2.0.em
  , titleHeight         = 2.0.em
  , formLblWidth        = 6.em
  , barSepwidth         = 3.px
  , formSepwidth        = 1.px

  , cornerRad           = 0.25.em
  , narrowBW            = 1.px
  , fatBW               = 2.px
  , padding             = 0.4.em
  , smallPadding        = 0.2.em
  , largePadding        = 0.6.em
  }

--------------------------------------------------------------------------------
-- Semantic Variables
--------------------------------------------------------------------------------

parameters {auto v : Vars}

  ||| Default background colour
  export %inline
  bg : Color
  bg = v.gray.c200

  ||| Default text colour
  export %inline
  fg : Color
  fg = v.gray.c900

  export
  headerFG : Color
  headerFG = v.gray.c100

  export
  headerBG : Color
  headerBG = v.primary.c800

  export
  compBorder : Color
  compBorder = v.primary.c800

  export
  widgetFG : Color
  widgetFG = fg

  export
  widgetBG : Color
  widgetBG = v.gray.c100

  export
  activeFG : Color
  activeFG = v.gray.c100

  export
  activeBG : Color
  activeBG = v.secondary.c600

  export
  disabledFG : Color
  disabledFG = v.gray.c500

  export
  disabledBG : Color
  disabledBG = v.gray.c300

  export
  bar : Color
  bar = v.primary.c800
