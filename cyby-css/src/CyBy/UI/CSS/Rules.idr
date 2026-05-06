module CyBy.UI.CSS.Rules

import Chem.Elem
import CyBy.UI.CSS.Classes
import CyBy.UI.CSS.Vars
import CyBy.UI.HTML
import Derive.Prelude
import IO.Async.Logging
import Text.CSS.Cursor
import Text.HTML.DomID
import Text.HTML.Ref
import Text.HTML.Tag

%default total
%language ElabReflection

data Tag = Util | Templates | Elems | Info | Draw | Dot

%runElab derive "Tag" [Show,Eq]

export
attr : Attribute () -> Selector 
attr (Str  n v) = Attr n $ Equals v
attr (Bool n _) = Attr n Set
attr _          = []

export %inline
domID : DomID -> Declarations -> Rule n
domID = id . value

export %inline
attribute : Attribute () -> Declarations -> Rule n
attribute = sel . attr

export
boolAttr : (Bool -> Attribute ()) -> Selector 
boolAttr f =
  case f True of
    Bool name _ => Attr name Set
    _           => Attr "" Set

export
formValues : List Selector
formValues =
  [ class listEntryValue
  , class listEntry > class widget
  , class listEntry > elem Input
  , class listEntry > elem Select
  ]

export
widgetSelectors : List Selector
widgetSelectors = [Elem Button, Elem Input, Elem Select, Class widget]

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
  solidBorder : Color -> Declarations
  solidBorder c = solidBorder v.narrowBW c

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
    [ backgroundColor activeBG
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
  sectionBorder : Declarations
  sectionBorder = roundedBorder v.fatBW compBorder v.cornerRad

  export
  sectionHeader : Declarations
  sectionHeader =
       width 100.perc
    :: alignItems Center
    :: height v.titleHeight
    :: backgroundColor headerBG
    :: color headerFG
    :: padding (VH v.smallPadding v.padding)
    :: solidBorder headerBG
    ++ flexRow

  export
  sectionList : Declarations
  sectionList = [flex1, overflowY Scroll, padded] ++ flexColumn

  export
  vsep : Declarations
  vsep = [width 100.perc, height v.barSepWidth]

  export
  hsep : Declarations
  hsep = [height 100.perc, width v.barSepWidth]

  levelRule : LogLevel -> Color -> Rule n
  levelRule l c = class (level l) [color c, width v.levelWidth]

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

parameters {auto v : Vars}

  export
  widgetRules : Selector -> Rules
  widgetRules s =
    [ sel s $ alignSelf Stretch :: hpadded :: wregular
    , sel [s, Hover] whovered
    , sel [s, Active] wactive
    , sel [s, boolAttr active] wactive
    , sel [s, Disabled] wdisabled
    ]

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

    , elem Header [noMargin]
    , elem Ul [noMargin, noPadding, decl "list-style" "none"]

    -- this makes sure that the text in a label is vertically centered
    , elem Label [display Flex , alignItems Center]

    , class sep [backgroundColor bar, width 100.perc, height v.formSepWidth]
    , class spacer [flex1]
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
        :: outlineStyle Solid
        :: outlineWidth v.narrowBW
        :: outlineColor compBorder
        :: roundedBorder compBorder

    -- drawing canvas: special states
    , sel [class moleculeCanvas, boolAttr active]
        [ backgroundColor widgetBG
        , outlineWidth v.fatBW
        , outlineColor activeBG
        , borderColor (All activeBG)
        ]
    , attribute (dragMode Dragging) [cursor [Move]]
    , attribute (dragMode Rotating)
        [cursor [URL_ "data:image/png;base64,\{rotate}", Cursor.Auto]]

    -- CyBy Draw toolbars
    , class drawUtils $ gridArea Util :: flexRow
    , class drawTemplates $ gridArea Templates :: flexRow
    , class drawElems $ gridArea Elems :: flexColumn
    , class drawInfo $ [containerType Size, gridArea Rules.Info] ++ flexColumn
    , sel (class drawUtils > class sep) hsep
    , sel (class drawElems > class sep) vsep
    , sel (class drawTemplates > class sep) hsep

    -- CyBy Sections (Cards)
    , elem Section $ flexColumn ++ sectionBorder
    , sel (elem Section > elem Header) sectionHeader
    , sel (elem Section > elem Ul) sectionList
    , class drawDetails [containerType Size, flex2]

    -- logging
    , class drawLog [fontSize v.smallFont, containerType Size, flex1]
    , levelRule Fatal v.errorColor
    , levelRule Error v.errorColor
    , levelRule Warn  v.warnColor
    , levelRule Info  v.infoColor
    , levelRule Debug v.debugColor
    , levelRule Trace v.traceColor
    ]

  ||| Rules for form-like lists (label plus description/widget)
  export
  forms : Rules
  forms =
    [ class listEntry $ alignItems Start :: flexRow
    , sel (class listEntry > elem Label) [width v.formLblWidth]
    , Sel formValues [flex1]
    , Container "width < 300px"
        [ class listEntry $ alignItems Start :: flexColumn
        , sel (class listEntry > elem Label) [width 100.perc]
        , Sel formValues [noflex, margin (Left v.gap)]
        ]
    ]

  ||| Rules for interactive UI elements
  export
  widgets : Rules
  widgets =
       (widgetSelectors >>= widgetRules)
    ++ [class icon [noPadding, aspectRatio 1]]

  export
  all : Rules
  all = general ++ components ++ forms ++ widgets

main : IO ()
main = traverse_ (putStrLn . interpolate) (all @{defaultVars})
