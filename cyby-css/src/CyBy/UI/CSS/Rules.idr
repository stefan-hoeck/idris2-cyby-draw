module CyBy.UI.CSS.Rules

import Chem.Elem
import CyBy.UI.CSS.Classes
import CyBy.UI.CSS.Vars
import CyBy.UI.HTML
import Derive.Prelude
import IO.Async.Logging
import Text.CSS.Cursor
import Text.HTML.Ref
import Text.HTML.Tag

%default total
%language ElabReflection

data Tag = Util | Templates | Elems | Info | Draw | Dot

%runElab derive "Tag" [Show,Eq]

export
set : ({0 k : Type} -> {0 t : k} ->  Bool -> Attribute t) -> Selector 
set f =
  case f {t = ()} True of
    Bool name _ => Attr name Set
    _           => Attr "" Set

--------------------------------------------------------------------------------
-- Reusable
--------------------------------------------------------------------------------

parameters {auto v : Vars}

  export
  gridGaps : Declarations
  gridGaps = [rowGap v.gap, columnGap v.gap]

  export
  hpadded : Declaration
  hpadded = padding (VH 0.px v.padding)

  export
  padded : Declaration
  padded = padding (All v.padding)

  ||| Flex container with a default gap between components that
  ||| arranges components horizontally.
  export
  flexRow : Declarations
  flexRow = [display Flex, flexDirection Row, columnGap v.gap]

  ||| Flex container with a default gap between components that
  ||| arranges components vertically.
  export
  flexColumn : Declarations
  flexColumn = [display Flex, flexDirection Column, rowGap v.gap]

  ||| `solidBorder` with rounded corners using the default corner radius.
  export
  roundedBorder : Color -> Declarations
  roundedBorder c = roundedBorder v.narrowBW c v.cornerRad

  ||| Regular widget with default colors for font, background, and border.
  export
  wregular : Declarations
  wregular =
    backgroundColor widgetBG
    :: color widgetFG
    :: outlineColor Current
    :: outlineStyle None
    :: roundedBorder Current

  ||| Widget that has either the `data-active` attribute set, or
  ||| is in an `active` state (has the `:active` pseudoclass).
  export
  wactive : Declarations
  wactive =
    [ backgroundColor activeBG
    , color activeFG
    , outlineStyle Solid
    , outlineWidth v.narrowBW
    ]

  ||| Widget that is being hovered over (has the `:hover` pseudoclass).
  export
  whovered : Declarations
  whovered =
    [ backgroundColor widgetBG
    , color activeFG
    , outlineStyle Solid
    , outlineWidth v.narrowBW
    ]

  ||| Disabled widget (has the `:disabled` pseudoclass).
  export
  wdisabled : Declarations
  wdisabled =
    color disabledFG
    :: backgroundColor disabledBG
    :: outlineStyle None
    :: roundedBorder disabledBG

  ||| Outline and border of a cyby-draw component.
  export
  drawCompBorder : Declarations
  drawCompBorder =
    outlineStyle Solid
    :: outlineWidth v.narrowBW
    :: outlineColor v.gray80
    :: roundedBorder bg

  drawTitle : Declarations
  drawTitle =
       width 100.perc
    :: display Flex
    :: alignItems Center
    :: height v.titleHeight
    :: backgroundColor widgetFG
    :: color v.primary10
    :: hpadded
    :: noMargin
    :: fontSize 1.em
    :: roundedBorder widgetFG

  drawList : Declarations
  drawList = [flex1, overflowY Scroll, margin 0.px, padded] ++ flexColumn

  export
  vsep : Declarations
  vsep = [width 100.perc, height v.barSepwidth]

  export
  hsep : Declarations
  hsep = [height 100.perc, width v.barSepwidth]

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
        , padding (All v.largePadding)
        , containerType Size
        ]

    -- this makes sure that the text in a label is vertically centered
    , elem Label [display Flex , alignItems Center]

    , class sep [backgroundColor bar, width 100.perc, height v.formSepwidth]
    ]

  ||| Rules the main UI components
  export
  components : Rules
  components =
    [ class sketcher $
        area
          [cast v.bardim, 1.fr, cast v.bardim]
          [cast v.bardim, 4.fr, 1.fr]
          [ [Dot,   Util,      Util     ]
          , [Elems, Draw,      Info     ]
          , [Dot,   Templates, Templates]
          ]
        :: width 100.perc
        :: height 100.perc
        :: gridGaps

    -- the following rules make for a responsive design:
    -- by reducing the font size of the sketcher, the dimensions of
    -- all other components as well as paddings and corners are
    -- adjusted as well.
    , Container "width < 1440px" [class sketcher [fontSize v.smallFont]]
    , Container "width < 1024px" [class sketcher [fontSize v.xsmallFont]]
    , Container "width < 768px"  [class sketcher [fontSize v.xxsmallFont]]

    -- the drawing canvas
    , class moleculeCanvas $
           width 100.perc
        :: height 100.perc
        :: minWidth 0.px    -- necessary to resize this when parent is resized
        :: minHeight 0.px   -- necessary to resize this when parent is resized
        :: gridArea Draw
        :: drawCompBorder

    -- drawing canvas: special states
    , sel [class moleculeCanvas, set active]
        [backgroundColor white, outlineWidth v.fatBW, outlineColor activeFG]
    , classes [moleculeCanvas,dragging] [cursor [Move]]
    , classes [moleculeCanvas,rotating]
        [cursor [URL_ "data:image/png;base64,\{rotate}", Cursor.Auto]]

    -- CyBy Draw toolbars
    , class drawUtils $ gridArea Util :: flexRow
    , class drawTemplates $ gridArea Templates :: flexRow
    , class drawElems $ gridArea Elems :: flexColumn
    , class drawInfo $ [containerType Size, gridArea Rules.Info] ++ flexColumn
    , sel (class drawUtils > class sep) hsep
    , sel (class drawElems > class sep) vsep
    , sel (class drawTemplates > class sep) hsep

    -- CyBy Draw details
    , class drawDetails $
        [containerType Size, flex2] ++ flexColumn ++ drawCompBorder
    , sel (class drawDetails > elem H1) drawTitle
    , sel (class drawDetails > elem Ul) drawList

    -- logging
    , class drawLog $
           [fontSize v.smallFont, containerType Size, flex1]
        ++ flexColumn
        ++ drawCompBorder

    , sel (class drawLog > elem H1) drawTitle
    , sel (class drawLog > elem Ul) drawList

    , class (level Fatal) [color red]
    , class (level Error) [color red]
    , class (level Warn)  [color $ rgb 255 165 0]
    , class (level Info)  [color $ rgb 0 128 0]
    , class (level Debug) [color $ gray]
    , class (level Trace) [color $ gray]
    ]

  ||| Rules for form-like lists (label plus description/widget)
  export
  forms : Rules
  forms =
    [ class listEntry $ alignItems Stretch :: flexRow
    , sel (class listEntry > elem Label) [width v.formLblWidth]
    , Sel [class listEntryValue, class listEntry > class widget] [flex1]
    , Container "width < 300px"
        [ class listEntry $ alignItems Start :: flexColumn
        , sel (class listEntry > elem Label) [width 100.perc]
        , Sel [class listEntryValue, class listEntry > class widget]
            [flex "0 0 auto", margin (Left v.gap)]
        ]
    ]

  ||| Rules for interactive UI elements
  export
  widgets : Rules
  widgets =
    [ class widget $ alignSelf Stretch :: hpadded :: wregular
    , sel [Class widget, Hover] whovered
    , sel [Class widget, Active] wactive
    , sel [Class widget, set active] wactive
    , sel [Class widget, Disabled] wdisabled
  
    , classes [widget, icon] [noPadding, aspectRatio 1]
  
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
all = general ++ components ++ forms ++ widgets

main : IO ()
main = traverse_ (putStrLn . interpolate) all
