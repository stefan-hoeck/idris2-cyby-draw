module CyBy.UI.CSS.Rules

import Chem.Elem
import Derive.Prelude
import Text.CSS.Cursor
import Text.HTML.Ref
import Text.HTML.Tag
import CyBy.UI.CSS.Classes
import CyBy.UI.CSS.Vars

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
-- Reusable
--------------------------------------------------------------------------------

parameters {auto v : Vars}


  export
  gridGaps : List Declaration
  gridGaps = [rowGap v.gap, columnGap v.gap]

  export
  flexRow : List Declaration
  flexRow = [display Flex, flexDirection Row, columnGap v.gap]

  export
  flexColumn : List Declaration
  flexColumn = [display Flex, flexDirection Column, rowGap v.gap]

  export
  solidBorder : Color -> List Declaration
  solidBorder c =
    [ borderStyle (All Solid)
    , borderWidth  (All v.narrowBW)
    , borderColor (All c)
    ]

  export
  roundedBorder : Color -> List Declaration
  roundedBorder c = borderRadius v.cornerRad :: solidBorder c

  export
  wregular : List Declaration
  wregular =
    backgroundColor widgetBG
    :: color widgetFG
    :: outlineColor Current
    :: outlineStyle None
    :: roundedBorder Current

  export
  wactive : List Declaration
  wactive =
    [ backgroundColor activeBG
    , color activeFG
    , outlineStyle Solid
    , outlineWidth v.narrowBW
    ]

  export
  whovered : List Declaration
  whovered =
    [ backgroundColor widgetBG
    , color activeFG
    , outlineStyle Solid
    , outlineWidth v.narrowBW
    ]

  export
  wdisabled : List Declaration
  wdisabled =
    color disabledFG
    :: backgroundColor disabledBG
    :: outlineStyle None
    :: roundedBorder disabledBG

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

parameters {auto v : Vars}
  export
  general : Rules
  general =
    [ elem Html [height 100.perc, width 100.perc]
  
    , elem Body
        [ display Flex
        , height 100.perc
        , width 100.perc
        , backgroundColor bg
        , color fg
        , padding 1.em
        ]
  
    -- this makes sure that the text in a label is vertically centered
    , elem Label [display Flex , alignItems Center]
  
    , class hidden [display None]
  
    , class quadratic [aspectRatio 1]
  
    , class smallText [fontSize Small]
    ]

  ||| Rules the main UI components
  export
  components : Rules
  components =
    [ class sketcherDiv $
        area
          [cast v.bardim, 1.fr, cast v.bardim]
          [cast v.bardim, 3.fr, 1.fr]
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
        :: outlineWidth v.narrowBW
        :: outlineColor v.gray80
        :: roundedBorder bg
  
    , sel [class moleculeCanvas, attribute "data-active"]
        [backgroundColor white, outlineWidth v.fatBW, outlineColor activeFG]
    , classes [moleculeCanvas,dragging] [cursor [Move]]
    , classes [moleculeCanvas,rotating]
        [cursor [URL_ "draw_icons/icon_rotation.svg", Cursor.Auto]]
    ]
  
  export
  icons : Rules
  icons =
    [ class fillPath [fill $ Just Current]
    , class molPath [stroke $ Just Current]
    ]
  
  ||| Rules for interactive UI elements
  export
  widgets : Rules
  widgets =
    [ class widget $ alignSelf Stretch :: padding (VH 0.px v.paddingH) :: wregular
    , sel [Class widget, Hover] whovered
    , sel [Class widget, Active] wactive
    , sel [Class widget, attribute "data-active"] wactive
    , sel [Class widget, Disabled] wdisabled
  
    , classes [widget, icon] [padding 0.px]
  
    , class (elemText B)  [fontWeight Bold, color v.boron]
    , class (elemText C)  [fontWeight Bold, color v.carbon]
    , class (elemText F)  [fontWeight Bold, color v.fluorine]
    , class (elemText S)  [fontWeight Bold, color v.sulfur]
    , class (elemText O)  [fontWeight Bold, color v.oxygen]
    , class (elemText N)  [fontWeight Bold, color v.nitrogen]
    , class (elemText P)  [fontWeight Bold, color v.phosphorous]
    , class (elemText Br) [fontWeight Bold, color v.bromine]
    , class (elemText Cl) [fontWeight Bold, color v.chlorine]
    ]

export
all : Rules
all = general ++ components ++ icons ++ widgets

main : IO ()
main = traverse_ (putStrLn . interpolate) all
