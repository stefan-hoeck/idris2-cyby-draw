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

  , secondary10         = hsl 180 20.perc 95.perc
  , secondary20         = hsl 180 30.perc 90.perc
  , secondary50         = hsl 180 80.perc 40.perc
  , secondary80         = hsl 180 60.perc 25.perc
  , secondary90         = hsl 180 40.perc 10.perc

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
gridGaps : {default 5 gap : Bits16} -> List Declaration
gridGaps = [rowGap gap.px, columnGap gap.px]

export
flexRow : {default 5 gap : Bits16} -> List Declaration
flexRow = [display Flex, flexDirection Row, columnGap gap.px]

export
flexColumn : {default 5 gap : Bits16} -> List Declaration
flexColumn = [display Flex, flexDirection Column, rowGap gap.px]

export
solidBorder : (width : Bits16) -> Color -> List Declaration
solidBorder w c =
  [borderStyle (All Solid), borderWidth w.px, borderColor (All c)]

export
roundedBorder : (width, rad : Bits16) -> Color -> List Declaration
roundedBorder w r c = borderRadius r.px :: solidBorder w c

export
wregular : (c : Colours) => List Declaration
wregular =
  backgroundColor c.primary20
  :: color c.primary80
  :: roundedBorder 1 3 c.primary80

export
wactive : (c : Colours) => List Declaration
wactive =
  backgroundColor c.primary10
  :: color c.primary50
  :: outlineStyle Solid
  :: outlineWidth 1.px
  :: roundedBorder 1 3 c.primary50
  

export
whovered : (c : Colours) => List Declaration
whovered =
  backgroundColor c.primary20
  :: color c.primary50
  :: outlineStyle Solid
  :: outlineWidth 1.px
  :: roundedBorder 1 3 c.primary50

export
wdisabled : (c : Colours) => List Declaration
wdisabled =
  color c.gray80
  :: backgroundColor c.gray20
  :: outlineStyle None
  :: roundedBorder 1 3 c.gray20

export
hoveredSVG : Class -> Selector
hoveredSVG c = Complex [class widget, Hover] Descendant (class c)

export
activeSVG : Class -> Selector
activeSVG c = Complex Active Descendant (class c)

export
activeAttrSVG : Class -> Selector
activeAttrSVG c = Complex (attribute "data-active") Descendant (class c)

export
disabledSVG : Class -> Selector
disabledSVG c = Complex [class widget, Disabled] Descendant (class c)

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

export
general : (c : Colours) => Rules
general =
  [ elem Html [height 100.perc, width 100.perc]

  , elem Body
      [ display Flex
      , height 100.perc
      , width 100.perc
      , backgroundColor c.primary10
      , color c.primary90
      , padding (All 10.px)
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
        [25.px, 1.fr, 25.px]
        [25.px, 1.fr, MaxContent]
        [ [Top, Top, Dot]
        , [Left, Draw, Right]
        , [Bot, Bot, Dot]
        ]
        :: width 100.perc
        :: height 100.perc
        :: gridGaps

  , class toolbarTop    $ gridArea Rules.Top   :: flexRow
  , class toolbarBottom $ gridArea Rules.Bot   :: flexRow
  , class toolbarLeft   $ gridArea Rules.Left  :: flexColumn
  , class toolbarRight  $ gridArea Rules.Right :: flexColumn

  , class moleculeCanvas $
      width 100.perc
      :: height 100.perc
      :: minWidth 0.px
      :: minHeight 0.px
      :: gridArea Draw
      :: outlineStyle Solid
      :: outlineWidth 1.px
      :: outlineColor c.gray80
      :: roundedBorder 1 3 c.bg

  , sel [class moleculeCanvas, Focus]
      [backgroundColor white, outlineWidth 2.px, outlineColor c.primary50]
  , classes [moleculeCanvas,dragging] [cursor [Move]]
  , classes [moleculeCanvas,rotating]
      [cursor [URL_ "draw_icons/icon_rotation.svg", Cursor.Auto]]
  ]

export
icons : (c : Colours) => Rules
icons =
  [ class fillPath [fill $ Just c.primary80]
  , sel (activeSVG fillPath) [fill $ Just c.primary50]
  , sel (activeAttrSVG fillPath) [fill $ Just c.primary50]
  , sel (hoveredSVG fillPath) [fill $ Just c.primary50]
  , sel (disabledSVG fillPath) [fill $ Just c.gray80]
  , class molPath [stroke $ Just c.primary80]
  , sel (activeSVG molPath) [stroke $ Just c.primary50]
  , sel (activeAttrSVG molPath) [stroke $ Just c.primary50]
  , sel (hoveredSVG molPath) [stroke $ Just c.primary50]
  , sel (disabledSVG molPath) [stroke $ Just c.gray80]
  ]

||| Rules for interactive UI elements
export
widgets : (c : Colours) => Rules
widgets =
  [ class widget wregular
  , sel [Class widget, Hover] whovered
  , sel [Class widget, Active] wactive
  , sel [Class widget, attribute "data-active"] wactive
  , sel [Class widget, Disabled] wdisabled

  , class icon [height 25.px, padding (All 0.px)]

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
all = general ++ components ++ icons ++ widgets

main : IO ()
main = traverse_ (putStrLn . interpolate) all
