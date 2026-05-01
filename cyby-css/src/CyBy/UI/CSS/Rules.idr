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

public export
0 Rules0 : Type
Rules0 = List (Rule 0)

public export
0 Rules : Type
Rules = List (Rule 1)

data Tag = Top | Bot | Left | Details | Draw | Dot

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
  gridGaps : List Declaration
  gridGaps = [rowGap v.gap, columnGap v.gap]

  export
  hpadded : Declaration
  hpadded = padding (VH 0.px v.paddingH)

  export
  padded : Declaration
  padded = padding (All v.paddingH)

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
  drawCompBorder : List Declaration
  drawCompBorder =
    outlineStyle Solid
    :: outlineWidth v.narrowBW
    :: outlineColor v.gray80
    :: roundedBorder bg

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
        , containerType Size
        ]
  
    -- this makes sure that the text in a label is vertically centered
    , elem Label [display Flex , alignItems Center]
  
    , sel {n = 1} (set hidden) [display None]
  
    , class quadratic [aspectRatio 1]
  
    , class smallText [fontSize Small]

    , class hbarsep [backgroundColor bar, width 100.perc, height v.barSepwidth]

    , class vbarsep [backgroundColor bar, height 100.perc, width v.barSepwidth]

    , class formsep [backgroundColor bar, width 100.perc, height v.formSepwidth]
    ]

  ||| Rules the main UI components
  export
  components : Rules
  components =
    [ class sketcherDiv $
        area
          [cast v.bardim, 1.fr, cast v.bardim]
          [cast v.bardim, 4.fr, 1.fr]
          [ [Dot,  Top,  Top]
          , [Left, Draw, Details]
          , [Dot,  Bot,  Bot]
          ]
        :: width 100.perc
        :: height 100.perc
        :: gridGaps

    , Container "width < 1440px" [class sketcherDiv [fontSize v.smallFont]]
    , Container "width < 1024px" [class sketcherDiv [fontSize v.xsmallFont]]
    , Container "width < 768px"  [class sketcherDiv [fontSize v.xxsmallFont]]
  
    , class toolbarTop    $ gridArea Rules.Top   :: flexRow
    , class toolbarBottom $ gridArea Rules.Bot   :: flexRow
    , class toolbarLeft   $ gridArea Rules.Left  :: flexColumn
    , class toolbarRight  $
           [containerType Size, gridArea Rules.Details]
        ++ flexColumn
        ++ drawCompBorder

    , class drawDetails $
        [containerType Size, flex "2"] ++ flexColumn ++ drawCompBorder

    , class drawLog $
           [fontSize v.smallFont, containerType Size, flex "1"]
        ++ flexColumn
        ++ drawCompBorder

    , class compList $ flex "1" :: overflowY Scroll :: padded :: flexColumn

    , class compTitle $
           width 100.perc
        :: display Flex
        :: alignItems Center
        :: height v.titleHeight
        :: backgroundColor widgetFG
        :: color v.primary10
        :: hpadded
        :: roundedBorder widgetFG
  
    , class moleculeCanvas $
           width 100.perc
        :: height 100.perc
        :: minWidth 0.px
        :: minHeight 0.px
        :: gridArea Draw
        :: drawCompBorder
  
    , sel [class moleculeCanvas, set active]
        [backgroundColor white, outlineWidth v.fatBW, outlineColor activeFG]
    , classes [moleculeCanvas,dragging] [cursor [Move]]
    , classes [moleculeCanvas,rotating]
        [cursor [URL_ "data:image/png;base64,\{rotate}", Cursor.Auto]]

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
    [ class formRow $ alignItems Stretch :: flexRow
    , class formLabel [width v.formLblWidth]
    , class formValue [flex "1"]
    , Container "width < 300px"
        [class formRow $ alignItems Start :: flexColumn
        ,class formLabel [width 100.perc]
        ,class formValue [flex "0 0 auto", margin (Left v.gap)]
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
all = general ++ components ++ forms ++ widgets

main : IO ()
main = traverse_ (putStrLn . interpolate) all
