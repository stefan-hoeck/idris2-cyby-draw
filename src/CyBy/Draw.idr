module CyBy.Draw

import Data.Finite
import Data.List
import Geom
import Text.CSS
import Text.HTML.DomID
import Text.HTML.Select
import Text.Show.Pretty
import Text.SVG
import Web.Async

import CyBy.Draw.Internal.Label
import Geom.Gen2D.Debug

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

%inline
molToClipboard : HasIO io => CDGraph -> io ()
molToClipboard = toClipboard . writeMolfile . toMolfile

fromClipboard : Sink DrawEvent => Sink DrawMsg => HasIO io => io ()
fromClipboard =
  readFromClipboard1 $ \s =>
    ioToF1 $ case readMolfileE s of
      Left x  => case smilesToMol s of
        Left  _ => sink (ReadErr x)
        Right m => sink (Event.SetTempl $ initGraph m.graph)
      Right g => sink (Event.SetTempl g)

--------------------------------------------------------------------------------
-- Extensions
--------------------------------------------------------------------------------

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
  doExport     : DrawSettings => DrawState -> JS [] ()
  buttons      : Sink DrawEvent => (pre : String) -> HTMLNodes

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
sketcherDivInner : String -> Ref Div
sketcherDivInner pre = Id "\{pre}-sketcher-div-inner"

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
rightBarID : String -> Ref Div
rightBarID pre = Id "\{pre}-right-bar"

export
topBarID : String -> Ref Div
topBarID pre = Id "\{pre}-top-bar"

export
bottomBarID : String -> Ref Div
bottomBarID pre = Id "\{pre}-bottom-bar"

export
abbrID : String -> Ref Tag.Select
abbrID pre = Id "\{pre}-abbreviations"

export
expButton : String -> Ref Tag.Button
expButton pre = Id "\{pre}-exp-button"

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------
  
hidden : {0 t : _} -> Attribute t
hidden = class "hidden"

abbrCls : DrawState -> List Class
abbrCls s =
  case s.mode of
    SetAbbr _ => ["cyby-draw-select","active"]
    _         => ["cyby-draw-select"]

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
detail title n =
  div
    [class "cyby-draw-detail"]
    [label [ class "cyby-draw-label" ] [ Text title ], n]

px : Double -> String
px v = show (cast {to = Bits32} v) ++ "px"

parameters {auto de : Sink DrawEvent}

  elems : MolAtomAT -> HTMLNode
  elems a =
    selectFromListBy values (a.elem.elem ==) symbol ChgElem
      [ class "cyby-draw-select", title "Set Element" ]

  charges : MolAtomAT -> HTMLNode
  charges a =
    selectFromListBy chs (a.charge ==) (show . value) ChgCharge
      [ class "cyby-draw-select", title "Set Charge" ]
    where
      chs : List Charge
      chs = mapMaybe refineCharge [(-8) .. 8]

  massNrs : MolAtomAT -> HTMLNode
  massNrs a =
    selectFromListBy (masses a.elem.elem) (a.elem.mass ==) dispMass ChgMass
      [ class "cyby-draw-select", title "Set Charge" ]
    where
      dispMass : Maybe MassNr -> String
      dispMass Nothing  = "Mix"
      dispMass (Just m) = show m.value
  
  icon' :
       List (Attribute Tag.Button)
    -> (cls : Class)
    -> DrawEvent
    -> (title : String)
    -> HTMLNode
  icon' as cls ev ttl =
    button (classes ["cyby-draw-icon", cls] :: onClick ev :: title ttl :: as) []

  %inline
  icon : (cls : Class) -> DrawEvent -> (title : String) -> HTMLNode
  icon = icon' []

  radioIcon : (cls : Class) -> DrawEvent -> (ttl : String) -> Bool -> HTMLNode
  radioIcon cls ev ttl b =
    input
      [ name "tool"
      , type Radio
      , classes ["cyby-draw-radio-icon", cls]
      , onClick ev, title ttl
      , checked b
      ]

  abbrs : (ds : DrawSettings) => (pre : String) -> DrawState -> HTMLNode
  abbrs pre s =
    selectFromListBy
      ds.abbreviations
      (\a => any ((a.label ==) . label) s.abbr)
      label
      SelAbbr
      [ Id $ abbrID pre
      , classes $ abbrCls s
      , title "Abbreviations"
      , Event (MouseDown $ \mi => toMaybe (mi.button == 0) EnableAbbr)
      ]

  bondIcon : Class -> MolBond -> String -> DrawState -> HTMLNode
  bondIcon c b title = radioIcon c (SetBond b) title . drawing b

  topBar :
       {auto ds : DrawSettings}
    -> {auto ex : Extension}
    -> (pre     : String)
    -> DrawState
    -> HTMLNode
  topBar {ds} pre s =
    div
      [ Id $ topBarID pre, class "cyby-draw-toolbar-top" ] $
      [ radioIcon "sel" SelectMode "select" (s.mode == Select)
      , radioIcon "erase" EraseMode "erase" (s.mode == Erase)
      , disable (order s.mol == 0) $ icon "clear" Clear "clear"
      , disable (s.undos == []) $ icon "undo" Undo "undo"
      , disable (s.redos == []) $ icon "redo" Redo "redo"
      , icon "center" Center "center"
      , disable (maxZoom s.transform) $ icon "zoom-in" (ZoomIn False) "zoom in"
      , disable (minZoom s.transform) $ icon "zoom-out" (ZoomOut False) "zoom out"
      , bondIcon "single-bond" (cast Single) "single bond" s
      , bondIcon "single-up" (fromStereo Up) "single bond up" s
      , bondIcon "single-down" (fromStereo Down) "single bond down" s
      , bondIcon "single-up-down" (fromStereo Either) "single bond up or down" s
      , bondIcon "double-bond" (cast Chem.Types.Dbl) "double bond" s
      , bondIcon "triple-bond" (cast Triple) "triple bond" s
      ] ++ ex.buttons pre

  template : (cls : Class) -> CDGraph -> String -> DrawState -> HTMLNode
  template cls g nm s =
    radioIcon cls (SetTempl g) "Template \{nm}" (s.mode == SetTempl g)

  leftBar : (pre : String) -> DrawState -> HTMLNode
  leftBar pre s =
    div
      [ Id $ leftBarID pre, class "cyby-draw-toolbar-left" ]
      [ radioIcon "set-c" (SetElem C) "Carbon" (setting C s)
      , radioIcon "set-o" (SetElem O) "Oxygen" (setting O s)
      , radioIcon "set-n" (SetElem N) "Nitrogen" (setting N s)
      , radioIcon "set-f" (SetElem F) "Fluorine" (setting F s)
      , radioIcon "set-p" (SetElem P) "Phosphorus" (setting P s)
      , radioIcon "set-s" (SetElem S) "Sulfur" (setting S s)
      , radioIcon "set-cl" (SetElem Cl) "Chlorine" (setting Cl s)
      , radioIcon "set-br" (SetElem Br) "Bromine" (setting Br s)
      , radioIcon "pse" StartPSE "PSE" (pse s.mode)
      ]


  rightBar : (pre : String) -> DrawState -> HTMLNode
  rightBar pre s =
    case selectedNodes s.imol False of
      [n] =>
        let atm     := atom $ lab s.imol n
            tpe     := atm.type.name
            [x,y,_] := atm.position
            cx      := dispCoordShort x
            cy      := dispCoordShort y
         in div
              [ Id $ rightBarID pre, class "cyby-draw-toolbar-right" ]
              [ detail "Element"  $ elems atm
              , detail "Isotope"  $ massNrs atm
              , detail "Charge"   $ charges atm
              , detail "Type"     $ div [ class "cyby-draw-atomtype"] [Text tpe]
              , detail "x-Coord." $ div [ class "cyby-draw-atomtype"] [Text cx]
              , detail "y-Coord." $ div [ class "cyby-draw-atomtype"] [Text cy]
              ]
      _   => case selectedEdges s.imol of
        [(x,y)] =>
          let px := point $ position $ atom $ lab s.imol x
              py := point $ position $ atom $ lab s.imol y
              d  := printDouble 3 $ distance px py
              a  := angleOrZero (px - py)
              a' := printDouble (S Z) $ toDegree $ if a >= Angle.pi then (a - Angle.pi) else a
           in div
                [ Id $ rightBarID pre, class "cyby-draw-toolbar-right" ]
                [ detail "Length"   $ div [ class "cyby-draw-atomtype"] [Text "\{d} Å"]
                , detail "Angle"    $ div [ class "cyby-draw-atomtype"] [Text "\{a'}°"]
                ]
        _       => div [ Id $ rightBarID pre, class "cyby-draw-toolbar-right" ] []

  bottomBar : (pre : String) -> DrawState -> HTMLNode
  bottomBar pre s =
    div
      [ Id $ bottomBarID pre, class "cyby-draw-toolbar-bottom-inner" ]
      [ template "benzene" phenyl "Benzene" s
      , template "cyclohexane" (ring 6) "Cyclohexane" s
      , template "cyclopentane" (ring 5) "Cyclopentane" s
      , template "cyclopropane" (ring 3) "Cyclopropane" s
      , template "cyclobutane" (ring 4) "Cyclobutane" s
      , template "cycloheptane" (ring 7) "Cycloheptane" s
      , template "cyclooctane" (ring 8) "Cyclooctane" s
      ]

  export
  sketcher :
       {auto ds : DrawSettings}
    -> {auto ex : Extension}
    -> (pre     : String)
    -> DrawState
    -> HTMLNode
  sketcher pre s =
    div
      [ class "cyby-draw-main-content"
      , Id $ sketcherDiv pre
      ]
      [ div
        [ class "cyby-draw-sketcher-div"
        , Id $ sketcherDivInner pre
        ]
        [ topBar pre s
        , leftBar pre s
        , rightBar pre s
        , div
            [ class "cyby-draw-molecule-canvas"
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
            , style
                [ width $ px $ cast s.dims.swidth
                , height $ px $ cast s.dims.sheight
                ]
            ]
            [Raw s.curSVG]
        , div
            [ class "cyby-draw-toolbar-bottom-outer" ]
            [ bottomBar pre s, abbrs pre s ]
        ]
      ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

molCanvasCls : Class
molCanvasCls = "cyby-draw-molecule-canvas"

parameters {auto ds : DrawSettings}
           {auto se : Sink DrawEvent}
           {auto sm : Sink DrawMsg}
           {auto ex : Extension}
           (pre : String)

  canvasCls : List Class -> Act ()
  canvasCls = attr (moleculeCanvas pre) . classes . (molCanvasCls ::)

  rotating : Act ()
  rotating = canvasCls ["rotating"]

  dragging : Act ()
  dragging = canvasCls ["dragging"]

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

  adjAbbrCls : DrawState -> Act ()
  adjAbbrCls s = attr (abbrID pre) . classes $ abbrCls s

  focusCurrentApp : Act ()
  focusCurrentApp = focus (moleculeCanvas pre)

  displayST : DrawState -> Act ()
  displayST s =
    when (s.curSVG /= s.prevSVG) $
      child (moleculeCanvas pre) (Raw s.curSVG) >>
      when s.hasFocus focusCurrentApp

  adjustBars : DrawState -> Act ()
  adjustBars s = do
    replace (topBarID pre) (topBar pre s)
    replace (bottomBarID pre) (bottomBar pre s)
    replace (leftBarID pre) (leftBar pre s)
    adjAbbrCls s

  adjustRightBar : DrawState -> Act ()
  adjustRightBar s = do
    replace (rightBarID pre) (rightBar pre s)
    adjAbbrCls s

  dispKeyDown : String -> DrawState -> Act ()
  dispKeyDown "Escape" s = replace (sketcherDiv pre) (sketcher pre s)
  dispKeyDown "c" s =
    when (s.modifier == Ctrl) $
      let g := selectedSubgraph True s.mol
       in when (g.order > 0) (molToClipboard g >> sink Copied)
  dispKeyDown "x" s =
    when (s.modifier == Ctrl) $
      let g := selectedSubgraph False s.mol
       in when (g.order > 0) (molToClipboard g >> sink Copied)
  dispKeyDown "v"    s = when (s.modifier == Ctrl) fromClipboard
  dispKeyDown "Ctrl" s = selectCursor s
  dispKeyDown _      s = pure ()

  displayEv : DrawEvent -> DrawState -> Act ()
  displayEv Focus            s = focusCurrentApp
  displayEv Blur             s = blur (moleculeCanvas pre)
  displayEv (KeyDown k)      s = dispKeyDown k s
  displayEv (KeyUp _)        s = adjustRightBar s
  displayEv (SetElem _)      s = adjustBars s
  displayEv (SelAbbr _)      s = adjustBars s
  displayEv  EnableAbbr      s = adjustBars s
  displayEv (SetBond _)      s = adjustBars s
  displayEv (SetTempl _)     s = adjustBars s
  displayEv (Load _)         s = adjustBars s
  displayEv SelectMode       s = adjustBars s
  displayEv EraseMode        s = adjustBars s
  displayEv (ChgElem _)      s = adjustRightBar s
  displayEv (Move _ _)       s = selectCursor s
  displayEv MiddleDown       s = selectCursor s
  displayEv MiddleUp         s = selectCursor s
  displayEv LeftUp           s = adjustBars s >> adjustRightBar s
  displayEv Undo             s = adjustBars s
  displayEv Redo             s = adjustBars s
  displayEv (ZoomIn _)       s = adjustBars s
  displayEv (ZoomOut _)      s = adjustBars s
  displayEv Clear            s = adjustBars s
  displayEv SVG              s = weakenErrors $ ex.doExport s
  displayEv _                s = pure ()

  disableExportIfNoGraph : DrawState -> Act ()
  disableExportIfNoGraph s =
    let G o _ := s.mol in disabled (expButton pre) (o == 0)

  export
  displaySketcher : DrawEvent -> DrawState -> Act ()
  displaySketcher e s = displayEv e s >> displayST s >> disableExportIfNoGraph s

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
  -> (DrawMsg -> Act ())
  -> Act DrawSettings
  -> SceneDims
  -> Editor MolfileAT
molEdit logmsg getDS sd =
  E $ \m => Prelude.do
   ui   <- map interpolate uniqueID
   ds   <- getDS
   E es <- event DrawEvent
   E ms <- event DrawMsg
   let st := fromMol sd Init (maybe (G 0 empty) graph m)
       nd := sketcher ui st
   pure $ Widget.W nd $
     merge
       [ es |> P.evalScans1 st (doact ui)
            |> (\x => cons st x)
            |> P.mapOutput (Valid . toMolfile . mol)
       , ms |> foreach logmsg 
       ]

   where
     doact :
          {auto ds : DrawSettings}
       -> {auto sm : Sink DrawMsg}
       -> {auto se : Sink DrawEvent}
       -> (pre     : String)
       -> DrawState
       -> DrawEvent
       -> Act DrawState
     doact pre s e = let s2 := update e s in displaySketcher pre e s2 $> s2

||| The default `Extension`
export %hint
NoExt : Extension
NoExt =
  E
    { doExport     = toClipboard . exportSVG
    , buttons      = \pre => [ icon' [Id $ expButton pre] "svg" SVG "svg" ]
    }
