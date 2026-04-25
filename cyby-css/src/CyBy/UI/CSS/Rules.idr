module CyBy.UI.CSS.Rules

import Chem.Elem
import Derive.Prelude
import Text.CSS
import Text.CSS.Cursor
import Text.HTML.Ref
import Text.HTML.Tag
import CyBy.UI.CSS.Classes

%default total
%language ElabReflection

public export
0 Rules0 : Type
Rules0 = List (Rule 0)

public export
0 Rules : Type
Rules = List (Rule 1)

data Tag = Top | Bot | Left | Right | Draw | Dot

%runElab derive "Tag" [Show,Eq]

--------------------------------------------------------------------------------
-- Colours
--------------------------------------------------------------------------------

public export
record Colours where
  [noHints]
  constructor C
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

  boron               : Color
  bromine             : Color
  carbon              : Color
  chlorine            : Color
  fluorine            : Color
  nitrogen            : Color
  oxygen              : Color
  phosphorous         : Color
  sulfur              : Color

export %hint
defaultColours : Colours
defaultColours =
  C {
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

  , boron               = rgb 0xff 0xb5 0xb5
  , carbon              = dimgray
  , fluorine            = limegreen
  , sulfur              = rgb 0xE6 0xC6 0x40
  , oxygen              = rgb 0xFF 0x0D 0x0D
  , nitrogen            = rgb 0x30 0x50 0xF8
  , chlorine            = rgb 0x1F 0xF0 0x1F
  , bromine             = rgb 0xA6 0x29 0x29
  , phosphorous         = rgb 0xFF 0x80 0x00
  }

export %inline
(.bg) : Colours -> Color
(.bg) = primary10

export %inline
(.fg) : Colours -> Color
(.fg) = primary90

--------------------------------------------------------------------------------
-- Reusable
--------------------------------------------------------------------------------

export
gridGaps : {default 3 gap : Bits16} -> List Declaration
gridGaps = [rowGap gap.px, columnGap gap.px]

export
flexRow : {default 3 gap : Bits16} -> List Declaration
flexRow = [display Flex, flexDirection Row, rowGap gap.px]

export
flexColumn : {default 3 gap : Bits16} -> List Declaration
flexColumn = [display Flex, flexDirection Column, columnGap gap.px]

export
solidBorder : {default 2 width : Bits16} -> Color -> List Declaration
solidBorder c =
  [borderStyle (All Solid), borderWidth width.px, borderColor (All c)]

export
roundedBorder :
     {default 2 width : Bits16}
  -> {default 4 rad: Bits16}
  -> Color
  -> List Declaration
roundedBorder c = borderRadius rad.px :: solidBorder c

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

export
general : (c : Colours) => Rules
general =
  [ elem Html [height 100.perc]

  , elem Body
      [ display Flex
      , height 100.perc
      , width 100.perc
      , backgroundColor c.primary10
      , color c.primary90
      ]

  -- this makes sure that the text in a label is vertically centered
  , elem Label [display Flex , alignItems Center, padding (VH 1.px 4.px)]

  , class hidden [display None]

  , class quadratic [aspectRatio 1]

  , class smallText [fontSize Small]
  ]

||| Rules the main UI components
export
components : (c : Colours) => Rules
components =
  [ class sketcherDiv $
      area
        [MaxContent, 1.fr, MaxContent]
        [MaxContent, 1.fr, MaxContent]
        [ [Top, Top, Dot]
        , [Left, Draw, Right]
        , [Bot, Bot, Dot]
        ]
        :: padding (All 3.px)
        :: gridGaps

  , class toolbarTop    $ gridArea Rules.Top   :: flexRow
  , class toolbarBottom $ gridArea Rules.Bot   :: flexRow
  , class toolbarLeft   $ gridArea Rules.Left  :: flexColumn
  , class toolbarRight  $ gridArea Rules.Right :: flexColumn

  , class moleculeCanvas $ gridArea Draw :: roundedBorder c.bg
  , sel [class moleculeCanvas, Focus] [borderColor $ All c.primary50]
  , classes [moleculeCanvas,dragging] [cursor [Move]]
  , classes [moleculeCanvas,rotating]
      [cursor [URL_ "draw_icons/icon_rotation.svg", Cursor.Auto]]
  ]

||| Rules for interactive UI elements
export
widgets : (c : Colours) => Rules
widgets =
  [ class widget $
         backgroundColor c.primary50
      :: color c.primary10
      :: roundedBorder c.primary50

  , sel [Class widget, Active]
      [ backgroundColor c.primary10
      , color c.primary50
      ]

  , sel [Class widget, attribute "data-active"]
      [ backgroundColor c.primary10
      , color c.primary50
      ]

  , sel [Class widget, Hover]
      [ backgroundColor c.primary20
      , color c.primary50
      ]

  , sel [Class widget, Disabled]
      [ color c.gray80
      , backgroundColor c.gray20
      , borderColor (All c.gray20)
      ]

  , class (elemText B)  [fontWeight Bold, color c.boron]
  , class (elemText C)  [fontWeight Bold, color c.carbon]
  , class (elemText F)  [fontWeight Bold, color c.fluorine]
  , class (elemText S)  [fontWeight Bold, color c.sulfur]
  , class (elemText O)  [fontWeight Bold, color c.oxygen]
  , class (elemText N)  [fontWeight Bold, color c.nitrogen]
  , class (elemText P)  [fontWeight Bold, color c.phosphorous]
  , class (elemText Br) [fontWeight Bold, color c.bromine]
  , class (elemText Cl) [fontWeight Bold, color c.chlorine]
  ]

export
all : Rules
all = general ++ components ++ widgets

main : IO ()
main = traverse_ (putStrLn . interpolate) all
