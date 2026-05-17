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
formValues : List Selector
formValues =
  [ elem Li > elem Div
  , elem Li > class widget
  , elem Li > elem Input
  , elem Li > elem Select
  ]

export
widgetSelectors : List Selector
widgetSelectors = [Elem Button, Elem Input, Elem Select, Class widget]

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

parameters {auto v : Vars}
  export
  gridGaps : Declarations
  gridGaps = [rowGap 0.5.em, columnGap 0.5.em]

  export
  hpadded : Declaration
  hpadded = padding (VH 0.px v.padding)

  export
  hbpadded : Declaration
  hbpadded = padding (THB 0.px v.padding v.padding)

  export
  padded : Declaration
  padded = padding (All v.padding)

  ||| Regular widget with default colors for font, background, and border.
  export
  widgetRegular : Declarations
  widgetRegular =
    [ backgroundColor widgetBG
    , color widgetFG
    , outlineStyle None
    , round4
    ] ++ border1 Current ++ exactHeight v.widgetHeight ++ centerRow

  ||| Widget that has either the `data-active` attribute set, or
  ||| is in an `active` state (has the `:active` pseudoclass).
  export
  wactive : Declarations
  wactive = [backgroundColor activeBG, color activeFG]

  ||| Widget that is being hovered over (has the `:hover` pseudoclass).
  export
  whovered : Declarations
  whovered = [backgroundColor hoverBG]

  ||| Widget that has currently visible focus
  ||| (has the `:focus-visible` pseudoclass).
  export
  wfocus : Declarations
  wfocus = [borderWidth $ All 2.px]

  ||| Widget that has currently visible focus
  ||| (has the `:focus-visible` pseudoclass).
  export
  winvalid : Declarations
  winvalid = [color v.errorColor]

  ||| Disabled widget (has the `:disabled` pseudoclass).
  export
  wdisabled : Declarations
  wdisabled =
    [color disabledFG, backgroundColor disabledBG, borderColor (All disabledBG)]

  ||| Outline and border of a cyby-draw component.
  export
  sectionBorder : Declarations
  sectionBorder = round8 :: borderHB1 headerBG

  export
  sectionHeader : Declarations
  sectionHeader =
       backgroundColor headerBG
    :: color headerFG
    -- horizontal padding is for readability, vertical padding
    -- is to make sure a separator bar does not cut the header in two
    :: padding (VH v.smallPadding v.padding)
    :: exactHeight v.titleHeight
    ++ centerSepRow

  export
  sectionList : Declarations
  sectionList =
    [flex1, overflowY Scroll, hbpadded] ++ stretchSepCol

  export
  vsep : Declarations
  vsep =
       margin (VH v.barSepMargin 0.px)
    :: width 100.perc
    :: borderRadius v.barSepRadius
    :: exactHeight v.barSepWidth

  export
  hsep : Declarations
  hsep =
       margin (VH 0.px v.barSepMargin)
    :: height 100.perc
    :: borderRadius v.barSepRadius
    :: exactWidth v.barSepWidth

  export
  iconDecl : Declarations
  iconDecl = [noPadding, aspectRatio 1]

  export
  roundIconDecl : Declarations
  roundIconDecl = round :: iconDecl

  levelRule : LogLevel -> Color -> Rule n
  levelRule l c =
    class (level l) [fontWeight Normal, color c, width v.levelWidth]

--------------------------------------------------------------------------------
-- General
--------------------------------------------------------------------------------

parameters {auto v : Vars}

  export
  widgetRules : Selector -> Rules
  widgetRules s =
    [ sel s $ hpadded :: widgetRegular
    , sel [s, Hover] whovered
    , sel [s, FocusVisible] wfocus
    , sel [s, boolAttr active] wactive
    , sel (elem Section > (elem Header > s)) [backgroundColor widgetInvertBG, color widgetInvertFG]
    , sel (elem Section > (elem Header > [s,Hover])) [backgroundColor hoverInvertBG]
    , sel [s, Disabled] wdisabled
    , sel [s, Invalid] winvalid
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
        , padded
        , containerType Size
        ]

    , elem Header [noMargin]
    , elem Ul [noMargin, noPadding, decl "list-style" "none"]

    -- this makes sure that the text in a label is vertically centered
    , elem Label [display Flex , alignItems Center]

    , class sep $ [backgroundColor bar, height v.formSepWidth]
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

        -- make sure the sketcher always fills the parent perfectly
        -- without resizing the parent in case the sketcher's size
        -- changes.
        :: position Absolute
        :: inset (All 0.px)

        -- scroll bars in case the parent is too small
        :: overflow Auto
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
           minWidth 0.px    -- necessary to resize this when parent is resized
        :: minHeight 0.px   -- necessary to resize this when parent is resized
        :: gridArea Draw
        :: outlineStyle None
        :: round4
        :: overflow Hidden
        :: border1 compBorder

    -- drawing canvas: special states
    , sel [class moleculeCanvas, boolAttr active]
        [ backgroundColor v.gray.c100
        , borderColor (All activeBG)
        ]
    , attribute dragMode Dragging [cursor [Move]]
    , attribute dragMode Rotating
        [cursor [URL_ "data:image/png;base64,\{rotate}", Cursor.Auto]]

    -- CyBy Draw toolbars
    , class drawUtils $ gridArea Util :: centerSepRow
    , class drawTemplates $ gridArea Templates :: centerSepRow
    , class drawElems $ gridArea Elems :: centerSepCol
    , class drawInfo $ [containerType Size, gridArea Rules.Info] ++ stretchSepCol
    , sel (class drawUtils > class sep) hsep
    , sel (class drawElems > class sep) vsep
    , sel (class drawTemplates > class sep) hsep

    -- CyBy Sections (Cards)
    , elem Section $ overflow Hidden :: stretchSepCol ++ sectionBorder
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
    [ elem Li startSepRow
    , sel (elem Li > elem Label) [width v.formLblWidth, fontWeight Bold]
    , Sel formValues [flex1]
    , Container "width < 300px"
        [ elem Li startSepCol
        , sel (elem Li > elem Label) [width 100.perc]
        , Sel formValues [noflex]
        ]
    ]

  ||| Rules for interactive UI elements
  export
  widgets : Rules
  widgets =
    (widgetSelectors >>= widgetRules) ++
    [ class icon iconDecl
    , class roundIcon roundIconDecl
    , class elem [fontWeight Bold, justifyContent Center]
    , class pseIcon [decl "letter-spacing" "-2px"]
    , sel [elem Button, Hover] [cursor [Pointer]]
    , sel [elem Button, Disabled] [cursor [NotAllowed]]
    , sel [class widget, Hover] [cursor [Pointer]]
    , sel [class widget, Disabled] [cursor [NotAllowed]]
    , sel [elem Select, Hover] [cursor [Pointer]]
    , sel [elem Select, Disabled] [cursor [NotAllowed]]
    , sel [elem Input, attr type File] [display None]
    ]

  export
  all : Rules
  all = general ++ components ++ forms ++ widgets

  draw : Rules
  draw = class sketcher [padded] :: all

main : IO ()
main = traverse_ (putStrLn . interpolate) (draw @{defaultVars})
