module CyBy.UI.CSS.Vars

import public CyBy.UI.CSS.Tailwind

%default total

||| Variables to be used in CSS styling.
public export
record Vars where
  [noHints]
  constructor V

  --    Colours
  gray                : TailwindColor
  primary             : TailwindColor
  secondary           : TailwindColor
  errorColor          : Color
  warnColor           : Color
  infoColor           : Color
  debugColor          : Color
  traceColor          : Color

  --    Font Sizes
  smallFont           : FontSize
  xsmallFont          : FontSize
  xxsmallFont         : FontSize

  --    Logging
  levelWidth          : Width

  --    Components
  titleHeight         : Width        -- height of a component title
  titleSepWidth       : Width        -- width of a separator component title

  --    containers
  bardim              : Length       -- height of a horizontal toolbar
  formLblWidth        : Width        -- width of a label in a form
  barSepWidth         : Width        -- width of a separator in a toolbar
  barSepRadius        : BorderRadius -- radius of toolbar separator
  barSepMargin        : Length       -- cross-axis margin of a separator in a toolbar
  formSepWidth        : Width        -- width of field separator in a form

  --    widgets --
  widgetHeight        : Width        -- height of a widget
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

  , smallFont           = 0.875.em  -- 14/16
  , xsmallFont          = 0.75.em   -- 12/16
  , xxsmallFont         = 0.625.em  -- 10/16

  , levelWidth          = 5.em

  , titleHeight         = 2.5.em
  , titleSepWidth       = 3.px

  , bardim              = 1.8.em
  , formLblWidth        = 8.em
  , barSepWidth         = 4.px
  , barSepRadius        = 2.px
  , barSepMargin        = 0.4.em
  , formSepWidth        = 1.px

  , widgetHeight        = 1.8.em
  , smallPadding        = 0.2.em
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
  headerBG = v.primary.c900

  export
  compBorder : Color
  compBorder = v.primary.c900

  export
  widgetFG : Color
  widgetFG = v.primary.c900

  export
  widgetBG : Color
  widgetBG = v.gray.c200

  export
  widgetInvertFG : Color
  widgetInvertFG = v.gray.c100

  export
  widgetInvertBG : Color
  widgetInvertBG = v.gray.c800

  export
  hoverBG : Color
  hoverBG = v.gray.c100

  export
  hoverInvertBG : Color
  hoverInvertBG = v.gray.c900

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
  bar = v.gray.c500
