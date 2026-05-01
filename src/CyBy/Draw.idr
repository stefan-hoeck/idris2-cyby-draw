module CyBy.Draw

import CyBy.Draw.Internal.Color
import CyBy.Draw.Internal.Label
import CyBy.UI.CSS.Classes
import CyBy.UI.HTML
import Data.Finite

import Data.List
import Geom
import Geom.Gen2D.Debug
import Text.HTML.DomID
import Text.HTML.Select
import Text.SVG
import Text.Show.Pretty
import Web.Async
import Web.Internal.Types

import public CyBy.Draw.Draw
import public CyBy.Draw.Event
import public CyBy.Draw.Internal.Abbreviations
import public CyBy.Draw.Internal.Atom
import public CyBy.Draw.Internal.CoreDims
import public CyBy.Draw.Internal.Graph
import public CyBy.Draw.Internal.Ring
import public CyBy.Draw.Internal.Role
import public CyBy.Draw.Internal.Settings
import public CyBy.Draw.MoleculeCanvas
import public CyBy.Draw.PeriodicTableCanvas
import public Text.Molfile

%default total
%hide Text.SVG.Types.Path.t

--------------------------------------------------------------------------------
-- Extensions
--------------------------------------------------------------------------------

export
color : ColorScheme -> Elem -> SVGColor
color Black  = const black
color CyBy   = basicColors
color Groups = groupColors
color CPK    = cpkColor
color CDK    = cdkColor
color JMol   = jmolColor
color PyMol  = pymolColor

%inline
molToClipboard : HasIO io => CDGraph -> io ()
molToClipboard = toClipboard . writeMolfile . toMolfile

fromClipboard : Sink DrawEvent => Loggable JS DrawMsg => Act ()
fromClipboard =
  readFromClipboard >>= \s =>
    case readMolfileE s of
      Left x  => case smilesToMol s of
        Left  _ => logLoggable (ReadErr x)
        Right m => sink (Event.SetTempl $ initGraph m.graph)
      Right g => sink (Event.SetTempl g)

export
storeSVG : String -> Act ()
storeSVG s =
  use1 (blob s "image/svg+xml" >>= blobURL) $ \u => Prelude.do
    e  <- createElement "a"
    setAttribute e "href" (cast u)
    setAttribute e "download" "cyby_draw_img.svg"
    he <- jsCast {t = HTMLElement} "storeSVG:<a> conversion" e
    click he

--------------------------------------------------------------------------------
-- Extensions
--------------------------------------------------------------------------------

||| Minimal environment required to run the core of cyby-draw.
public export
record DrawEnv where
  [noHints]
  constructor DE
  pre          : String
  {auto sets   : DrawSettings}
  {auto events : Sink DrawEvent}

||| Extension interface, currently used for the word plugin.
||| If the import button should be used, a tuple with the
||| class and title has to be specified. If no import button
||| is used, this is indicated by a `Nothing`.
||| If the export button should be modified, it also had to
||| be specified.
public export
record Extension where
  [noHints]
  constructor E
  doExport : DrawSettings => DrawState -> Act ()

  ||| Creats additional buttons to be displayed in the top bar
  ||| These might require additional mutable state (for instance, the current
  ||| colour scheme) , so this is an effectful computation
  buttons  : DrawEnv -> DrawState -> Act HTMLNodes

  ||| Make adjustments to the additional top bar buttons 
  adjust   : DrawEnv -> DrawEvent -> DrawState -> Act ()

--------------------------------------------------------------------------------
--          Events
--------------------------------------------------------------------------------

down : MouseInfo -> Maybe DrawEvent
down mi = case mi.button of
  0 => Just LeftDown
  1 => Just MiddleDown
  _ => Nothing

up : MouseInfo -> Maybe DrawEvent
up mi = case mi.button of
  0 => Just LeftUp
  1 => Just MiddleUp
  _ => Nothing

move : MouseInfo -> Maybe DrawEvent
move x = Just $ Move x.offsetX x.offsetY

wheel : WheelInfo -> Maybe DrawEvent
wheel wi =
  if wi.deltaY < 0 then Just (ZoomIn True)
     else if wi.deltaY > 0 then Just (ZoomOut True)
     else Nothing

--------------------------------------------------------------------------------
--          IDs
--------------------------------------------------------------------------------

export
moleculeCanvas : String -> Ref Div
moleculeCanvas pre = Id "\{pre}-molecule-canvas"

export
sketcherDiv : String -> Ref Div
sketcherDiv pre = Id "\{pre}-sketcher-div"

export
molReader : String -> Ref Div
molReader pre = Id "\{pre}-mol-reader"

export
molInput : String -> Ref TextArea
molInput pre = Id "\{pre}-mol-input"

export
leftBarID : String -> Ref Div
leftBarID pre = Id "\{pre}-left-bar"

export
detailsID : String -> Ref Div
detailsID pre = Id "\{pre}-draw-details"

export
rightBarID : String -> Ref Div
rightBarID pre = Id "\{pre}-right-bar"

export
topBarID : String -> Ref Div
topBarID pre = Id "\{pre}-top-bar"

export
bottomBarID : String -> Ref Div
bottomBarID pre = Id "\{pre}-bottom-bar"

export
expButton : String -> Ref Tag.Button
expButton pre = Id "\{pre}-exp-button"

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------
  
abbrActive : DrawState -> Attribute t
abbrActive s =
  case s.mode of
    SetAbbr _ => active True
    _         => active False

drawing : MolBond -> DrawState -> Bool
drawing b s =
  (s.mode == Draw || s.mode == Drawing Nothing) &&
  (s.bond == b)

setting : Elem -> DrawState -> Bool
setting el s = s.mode == SetAtom (cast el)

%inline fromStereo : BondStereo -> MolBond
fromStereo = MkBond True Single

disable : Bool -> HTMLNode -> HTMLNode
disable b = withAttribute (disabled b)

minZoom : (s : DrawSettings) => AffineTransformation -> Bool
minZoom (AT tf _) = tf.scale <= s.minZoom

maxZoom : (s : DrawSettings) => AffineTransformation -> Bool
maxZoom (AT tf _) = tf.scale >= s.maxZoom

pse : Mode -> Bool
pse (PTable _)  = True
pse (SetAtom i) = all (i.elem /=) (the (List Elem) [C,O,N,F,P,S,Cl,Br])
pse _           = False

detail : String -> HTMLNode -> HTMLNode
detail ttl n = div [class formRow] [label [class formLabel] [Text ttl], n]

currAbbr : Mode -> Maybe String
currAbbr (SetAbbr a) = Just a.label
currAbbr _           = Nothing

parameters {auto de : Sink DrawEvent}

  elems : MolAtomAT -> HTMLNode
  elems a =
    selectFromListBy values (a.elem.elem ==) symbol ChgElem
      [ classes [widget,formValue], title "set element" ]

  charges : MolAtomAT -> HTMLNode
  charges a =
    selectFromListBy chs (a.charge ==) (show . value) ChgCharge
      [ classes [widget,formValue], title "set charge" ]
    where
      chs : List Charge
      chs = mapMaybe refineCharge [(-8) .. 8]

  massNrs : MolAtomAT -> HTMLNode
  massNrs a =
    selectFromListBy (masses a.elem.elem) (a.elem.mass ==) dispMass ChgMass
      [ classes [widget,formValue], title "set charge" ]
    where
      dispMass : Maybe MassNr -> String
      dispMass Nothing  = "Mix"
      dispMass (Just m) = show m.value
  
  icon :
       Classes
    -> DrawEvent
    -> (active : Bool)
    -> (title : String)
    -> HTMLNode
    -> HTMLNode
  icon cs ev a ttl child =
    button
      [classes (widget::icon::quadratic::cs),active a,onClick ev,title ttl]
      [child]

  abbrs : (ds : DrawSettings) => (pre : String) -> DrawState -> HTMLNode
  abbrs pre s =
    selectFromListBy
      (Nothing :: map Just ds.abbreviations)
      (\v => currAbbr s.mode == map label v)
      (maybe "--abbreviation--" label)
      (maybe Redraw SelAbbr)
      [class widget, abbrActive s]

  bondIcon : MolBond -> String -> DrawState -> HTMLNode -> HTMLNode
  bondIcon b title s = icon [] (SetBond b) (drawing b s) title

  topBar :
       {auto ds : DrawSettings}
    -> (pre     : String)
    -> (topadd  : HTMLNodes)
    -> DrawState
    -> HTMLNode
  topBar {ds} pre topadd s =
    div
      [ Id $ topBarID pre, class toolbarTop ] $
      [ icon [] SelectMode (s.mode == Select) "select" select
      , icon [] EraseMode (s.mode == Erase) "erase" erase
      , icon [] Clear False "clear" trash
      , vbarSep
      , disable (s.undos == []) $ icon [] Undo False "undo" undo
      , disable (s.redos == []) $ icon [] Redo False "redo" redo
      , vbarSep
      , icon [] Center False "center" center
      , disable (maxZoom s.transform) $ icon [] (ZoomIn False) False "zoom in" zoomIn
      , disable (minZoom s.transform) $ icon [] (ZoomOut False) False "zoom out" zoomOut
      , vbarSep
      , bondIcon (cast Single) "single bond" s single
      , bondIcon (fromStereo Up) "single bond up" s bondUp
      , bondIcon (fromStereo Down) "single bond down" s bondDown
      , bondIcon (fromStereo Either) "single bond up or down" s bondEither
      , bondIcon (cast Types.Dbl) "double bond" s double
      , bondIcon (cast Triple) "triple bond" s triple
      , vbarSep
      ] ++ topadd

  template : CDGraph -> String -> DrawState -> HTMLNode -> HTMLNode
  template g nm s = icon [] (SetTempl g) (s.mode == SetTempl g) "template \{nm}"

  elemIcon : DrawState -> String -> Elem -> HTMLNode
  elemIcon s t e = icon [elemText e] (SetElem e) (setting e s) t (Text $ symbol e)

  leftBar pre s =
    div
      [ Id $ leftBarID pre, class toolbarLeft ]
      [ elemIcon s "Boron" B
      , elemIcon s "Carbon" C
      , elemIcon s "Oxygen" O
      , elemIcon s "Nitrogen" N
      , elemIcon s "Fluorine" F
      , elemIcon s "Phosphorous" P
      , elemIcon s "Sulfur" S
      , elemIcon s "Chlorine" Cl
      , elemIcon s "Bromine" Br
      , hbarSep
      , icon [] StartPSE (pse s.mode) "PSE" "PSE"
      ]

  detailItems : (pre : String) -> DrawState -> HTMLNodes
  detailItems pre s =
    case selectedNodes s.imol False of
      [n] =>
        let atm     := atom $ lab s.imol n
            tpe     := atm.type.name
            [x,y,_] := atm.position
            cx      := dispCoordShort x
            cy      := dispCoordShort y
         in [ detail "Element"  $ elems atm, formSep
            , detail "Isotope"  $ massNrs atm, formSep
            , detail "Charge"   $ charges atm, formSep
            , detail "Type"     $ div [class formValue] [Text tpe], formSep
            , detail "x-Coord." $ div [class formValue] [Text cx], formSep
            , detail "y-Coord." $ div [class formValue] [Text cy]
            ]
      _   => case selectedEdges s.imol of
        [(x,y)] =>
          let px := point $ position $ atom $ lab s.imol x
              py := point $ position $ atom $ lab s.imol y
              d  := printDouble 3 $ distance px py
              a  := angleOrZero (px - py)
              a' := printDouble (S Z) $ toDegree $ if a >= Angle.pi then (a - Angle.pi) else a
           in [ detail "Length"   $ div [class formValue] [Text "\{d} Å"], formSep
              , detail "Angle"    $ div [class formValue] [Text "\{a'}°"]
              ]
        _       => []

  details : (pre : String) -> DrawState -> HTMLNode
  details pre s =
    div
      [ Id $ detailsID pre, class drawDetails ]
      [ div [class compTitle] ["Details"]
      , div [class compList] (detailItems pre s)
      ]

  bottomBar : DrawSettings => (pre : String) -> DrawState -> HTMLNode
  bottomBar pre s =
    div
      [ Id $ bottomBarID pre, class toolbarBottom ]
      [ template phenyl "benzene" s benzene
      , template (ring 6) "cyclohexane" s cyclohexane
      , template (ring 5) "cyclopentane" s cyclopentane
      , template (ring 3) "cyclopropane" s cyclopropane
      , template (ring 4) "cyclobutane" s cyclobutane
      , template (ring 7) "cycloheptane" s cycloheptane
      , template (ring 8) "cyclooctane" s cyclooctane
      , vbarSep
      , abbrs pre s
      ]

  export
  sketcher :
       {auto ds : DrawSettings}
    -> (pre     : String)
    -> (topadd  : HTMLNodes)
    -> DrawState
    -> HTMLNode
  sketcher pre topadd s =
    div
      [ class sketcherDiv, Id $ sketcherDiv pre ]
      [ topBar pre topadd s
      , leftBar pre s
      , div [Id $ rightBarID pre, class toolbarRight] [details pre s]
      , div
          [ class moleculeCanvas
          , Id $ moleculeCanvas pre
          , Event $ MouseMove move
          , Event $ MouseDown down
          , Event $ MouseUp up
          , Event_ True False $ Wheel wheel
          , Event_ True False $ KeyDown (Just . KeyDown . key)
          , Event_ True False $ KeyUp (Just . KeyUp . key)
          , onMouseEnter Draw.Event.Focus
          , onMouseLeave Draw.Event.Blur
          , onDblClick Expand
          , onResize (\r => Resize r.height r.width)
          , Str "tabindex" "1"
          , active s.isActive
          ]
          [Raw s.curSVG]
      , bottomBar pre s
      ]

export
cybyDrawBtn : Sink e => String -> e -> Attributes Tag.Button -> HTMLNode
cybyDrawBtn s e as = button (class widget :: onClick e :: as) [Text s]

export
expBtn : DrawEnv => String -> DrawState -> HTMLNode
expBtn @{DE pre} txt s =
  cybyDrawBtn txt SVG  [Id $ expButton pre, disabled $ emptyGraph s]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

parameters {auto ds : DrawSettings}
           {auto se : Sink DrawEvent}
           {auto lm : Loggable JS DrawMsg}
           {auto ex : Extension}
           (pre : String)

  canvasCls : List Class -> Act ()
  canvasCls = attr (moleculeCanvas pre) . classes . (moleculeCanvas ::)

  rotating : Act ()
  rotating = canvasCls [rotating]

  dragging : Act ()
  dragging = canvasCls [dragging]

  normal : Act ()
  normal = canvasCls []

  selectCursor : DrawState -> Act ()
  selectCursor s =
    case s.mode of
      Dragging _    => dragging
      Rotating _    => rotating
      RotTempl _ _  => rotating
      Translating _ => dragging
      _             => applyWhenSel s dragging rotating normal

  displayST : (force : Bool) -> DrawState -> Act ()
  displayST force s = Prelude.do
    when s.isActive $ focus (moleculeCanvas pre)
    when (force || s.curSVG /= s.prevSVG) $
      child (moleculeCanvas pre) (Raw s.curSVG)

  adjustBars : DrawState -> Act ()
  adjustBars s = Prelude.do
    topadd <- ex.buttons (DE pre) s
    replace (topBarID pre) (topBar pre topadd s)
    replace (bottomBarID pre) (bottomBar pre s)
    replace (leftBarID pre) (leftBar pre s)
    replace (detailsID pre) (details pre s)

  dispKeyDown : String -> DrawState -> Act ()
  dispKeyDown "Escape" s = adjustBars s
  dispKeyDown "c" s =
    when (s.modifier == Ctrl) $
      let g := selectedSubgraph True s.mol
       in when (g.order > 0) (molToClipboard g >> logLoggable Copied)
  dispKeyDown "x" s =
    when (s.modifier == Ctrl) $
      let g := selectedSubgraph False s.mol
       in when (g.order > 0) (molToClipboard g >> logLoggable Copied)
  dispKeyDown "v"    s =
    -- we need to read from the clipboard in a new fiber, because the result
    -- will be written to the sink of `DrawEvent`s, which we are currently
    -- processing
    when (s.modifier == Ctrl) (ignore $ start fromClipboard)
  dispKeyDown "Ctrl" s = selectCursor s
  dispKeyDown _      s = pure ()

  displayEv : DrawEvent -> DrawState -> Act ()
  displayEv Focus            s = focus (moleculeCanvas pre) >> attr (moleculeCanvas pre) (active True)
  displayEv Blur             s = blur (moleculeCanvas pre) >> attr (moleculeCanvas pre) (active False)
  displayEv (KeyDown k)      s = dispKeyDown k s
  displayEv (KeyUp _)        s = adjustBars s
  displayEv (SetElem _)      s = adjustBars s
  displayEv (SelAbbr _)      s = adjustBars s
  displayEv (SetBond _)      s = adjustBars s
  displayEv (SetTempl _)     s = adjustBars s
  displayEv (Load _)         s = adjustBars s
  displayEv SelectMode       s = adjustBars s
  displayEv EraseMode        s = adjustBars s
  displayEv (ChgElem _)      s = adjustBars s
  displayEv (Move _ _)       s = selectCursor s
  displayEv MiddleDown       s = selectCursor s
  displayEv MiddleUp         s = selectCursor s
  displayEv LeftUp           s = adjustBars s
  displayEv Undo             s = adjustBars s
  displayEv Redo             s = adjustBars s
  displayEv (ZoomIn _)       s = adjustBars s
  displayEv (ZoomOut _)      s = adjustBars s
  displayEv Clear            s = adjustBars s
  displayEv SVG              s = ex.doExport s
  displayEv Redraw           s = displayST True s
  displayEv _                s = pure ()


  export
  displaySketcher : DrawEvent -> DrawState -> Act ()
  displaySketcher e s =
    displayEv e s >> displayST False s >> ex.adjust (DE pre) e s

export
disableExport : DrawEnv => DrawState -> Act ()
disableExport @{DE pre} = disabled (expButton pre) . emptyGraph

||| Renders a molecule at the given canvas.
|||
||| The molecule will be scaled and centered to fit the canvas and
||| the given nodes will be highlighted.
export
displayMol :
     {auto ds : DrawSettings}
  -> SceneDims
  -> MolGraphAT
  -> Maybe (List Nat)
  -> HTMLNode
displayMol sd g m =
  let cdg    := initGraph g
      G o mg := maybe cdg (\ns => highlight ns cdg) m
   in Raw . curSVG $ initMol sd Fill False "" $ G o mg

||| An editor for molecules.
export
molEdit :
     {auto ex : Extension}
  -> {auto lg : Loggable JS DrawMsg}
  -> Act DrawSettings
  -> SceneDims
  -> Editor MolfileAT
molEdit getDS sd =
  E $ \m => Prelude.do
   ui     <- map interpolate uniqueID
   ds     <- getDS
   E es   <- event DrawEvent
   let st := fromMol sd Init (maybe (G 0 empty) graph m)
   topadd <- ex.buttons (DE ui) st
   let nd := sketcher ui topadd st
   pure $ Widget.W nd $
     es |> P.evalScans1 st (doact ui)
        |> (\x => cons st x)
        |> P.mapOutput (Valid . toMolfile . mol)

   where
     doact : Sink DrawEvent => String -> DrawState -> DrawEvent -> Act DrawState
     doact pre s e = Prelude.do
       ds <- getDS
       let s2 := update e s
       displaySketcher pre e s2 $> s2

||| The default `Extension`
export %hint
NoExt : Extension
NoExt =
  E
    { doExport = storeSVG . exportSVG
    , buttons  = \_,s => pure [expBtn "Save..." s]
    , adjust   = \_,_,s => disableExport s
    }
