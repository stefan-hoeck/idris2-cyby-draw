module CyBy.Draw

import Data.Finite
import Data.List
import Geom
import Text.HTML.Select
import Text.SVG
import Web.Html
import Web.MVC
import Web.MVC.Util
import Web.MVC.View

import CyBy.Draw.Internal.Label
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

%foreign "browser:lambda:(s,w) => navigator.clipboard.writeText(s)"
prim__writeToClipboard : String -> PrimIO ()

%foreign "browser:lambda:(f,w) => navigator.clipboard.readText().then(s => f(s)(w))"
prim__readFromClipboard : (String -> PrimIO ()) -> PrimIO ()

molToClipboard : CDGraph -> JSIO ()
molToClipboard g = primIO (prim__writeToClipboard . writeMolfile $ toMolfile g)

fromClipboard : Cmd DrawEvent
fromClipboard =
  C $ \h => primIO $ prim__readFromClipboard $ \s,w =>
    case readMolfileE s of
      Left s  => toPrim (runJS $ h (Msg $ ReadErr s)) w
      Right g => toPrim (runJS $ h (SetTempl g)) w

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

bool : String -> Bool
bool "true" = True
bool _      = False

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
moleculeCanvas pre = Id "\{pre}_molecule_canvas"

export
sketcherDiv : String -> Ref Div
sketcherDiv pre = Id "\{pre}_sketcher_div"

export
sketcherDivInner : String -> Ref Div
sketcherDivInner pre = Id "\{pre}_sketcher_div_inner"

export
infoList : String -> Ref Div
infoList pre = Id "\{pre}_info_list"

export
molReader : String -> Ref Div
molReader pre = Id "\{pre}_mol_reader"

export
molInput : String -> Ref TextArea
molInput pre = Id "\{pre}_mol_input"

export
leftBarID : String -> Ref Div
leftBarID pre = Id "\{pre}_left_bar"

export
rightBarID : String -> Ref Div
rightBarID pre = Id "\{pre}_right_bar"

export
topBarID : String -> Ref Div
topBarID pre = Id "\{pre}_top_bar"

export
bottomBarID : String -> Ref Div
bottomBarID pre = Id "\{pre}_bottom_bar"

export
abbrID : String -> Ref Tag.Select
abbrID pre = Id "\{pre}_abbreviations"

--------------------------------------------------------------------------------
--          View
--------------------------------------------------------------------------------

elems : MolAtomAT -> Node DrawEvent
elems a =
  selectFromListBy values (a.elem.elem ==) symbol ChgElem
    [ class "cyby_draw_select", title "Set Element" ]

charges : MolAtomAT -> Node DrawEvent
charges a =
  selectFromListBy chs (a.charge ==) (show . value) ChgCharge
    [ class "cyby_draw_select", title "Set Charge" ]
  where
    chs : List Charge
    chs = mapMaybe refineCharge [(-8) .. 8]

massNrs : MolAtomAT -> Node DrawEvent
massNrs a =
  selectFromListBy (masses a.elem.elem) (a.elem.mass ==) dispMass ChgMass
    [ class "cyby_draw_select", title "Set Charge" ]
  where
    dispMass : Maybe MassNr -> String
    dispMass Nothing  = "Mix"
    dispMass (Just m) = show m.value

hidden : {0 t : _} -> Attribute t e
hidden = class "hidden"

icon : (cls : String) -> DrawEvent -> (title : String) -> Node DrawEvent
icon cls ev ttl =
  button [classes ["cyby_draw_icon", cls], onClick ev, title ttl] []

radioIcon :
     (cls : String)
  -> DrawEvent
  -> (title : String)
  -> Bool
  -> Node DrawEvent
radioIcon cls ev ttl b =
  input
    [ name "tool"
    , type Radio
    , classes ["cyby_draw_radio_icon", cls]
    , onClick ev, title ttl
    , checked b
    ]
    []

abbrCls : DrawState -> List String
abbrCls s =
  case s.mode of
    SetAbbr _ => ["cyby_draw_select","active"]
    _         => ["cyby_draw_select"]
    

abbrs : (ds : DrawSettings) => (pre : String) -> DrawState -> Node DrawEvent
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

drawing : MolBond -> DrawState -> Bool
drawing b s =
  (s.mode == Draw || s.mode == Drawing Nothing) &&
  (s.bond == b)

setting : Elem -> DrawState -> Bool
setting el s = s.mode == SetAtom (cast el)

bondIcon : String -> MolBond -> String -> DrawState -> Node DrawEvent
bondIcon c b title = radioIcon c (SetBond b) title . drawing b

%inline fromStereo : BondStereo -> MolBond
fromStereo = MkBond True Single

disable : Bool -> Node e -> Node e
disable b = withAttribute (disabled b)

minZoom : (s : DrawSettings) => AffineTransformation -> Bool
minZoom (AT tf _) = tf.scale <= s.minZoom

maxZoom : (s : DrawSettings) => AffineTransformation -> Bool
maxZoom (AT tf _) = tf.scale >= s.maxZoom

topBar : DrawSettings => (pre : String) -> DrawState -> Node DrawEvent
topBar pre s =
  div
    [ Id $ topBarID pre, class "cyby_draw_toolbar_top" ]
    [ radioIcon "sel" SelectMode "select" (s.mode == Select)
    , radioIcon "erase" EraseMode "erase" (s.mode == Erase)
    , disable (order s.mol == 0) $ icon "clear" Clear "clear"
    , disable (s.undos == []) $ icon "undo" Undo "undo"
    , disable (s.redos == []) $ icon "redo" Redo "redo"
    , icon "center" Center "center"
    , disable (maxZoom s.transform) $ icon "zoomIn" (ZoomIn False) "zoom in"
    , disable (minZoom s.transform) $ icon "zoomOut" (ZoomOut False) "zoom out"
    , bondIcon "snglB" (cast Single) "single bond" s
    , bondIcon "snglBUp" (fromStereo Up) "single bond up" s
    , bondIcon "snglBDown" (fromStereo Down) "single bond down" s
    , bondIcon "snglBUpDown" (fromStereo UpOrDown) "single bond up or down" s
    , bondIcon "dblB" (cast Dbl) "double bond" s
    , bondIcon "trplB" (cast Triple) "triple bond" s
    ]

template : (cls : String) -> CDGraph -> String -> DrawState -> Node DrawEvent
template cls g nm s =
  radioIcon cls (SetTempl g) "Template \{nm}" (s.mode == SetTempl g)

pse : Mode -> Bool
pse (PTable _)  = True
pse (SetAtom i) = all (i.elem /=) (the (List Elem) [C,O,N,F,P,S,Cl,Br])
pse _           = False

leftBar : (pre : String) -> DrawState -> Node DrawEvent
leftBar pre s =
  div
    [ Id $ leftBarID pre, class "cyby_draw_toolbar_left" ]
    [ radioIcon "setC" (SetElem C) "Carbon" (setting C s)
    , radioIcon "setO" (SetElem O) "Oxygen" (setting O s)
    , radioIcon "setN" (SetElem N) "Nitrogen" (setting N s)
    , radioIcon "setF" (SetElem F) "Fluorine" (setting F s)
    , radioIcon "setP" (SetElem P) "Phosphorus" (setting P s)
    , radioIcon "setS" (SetElem S) "Sulfur" (setting S s)
    , radioIcon "setCl" (SetElem Cl) "Chlorine" (setting Cl s)
    , radioIcon "setBr" (SetElem Br) "Bromine" (setting Br s)
    , radioIcon "pse" StartPSE "PSE" (pse s.mode)
    ]

detail : String -> Node e -> Node e
detail title n =
  div
    [class "cyby_draw_detail"]
    [label [ class "cyby_draw_label" ] [ Text title ], n]


rightBar : (pre : String) -> DrawState -> Node DrawEvent
rightBar pre s =
  case selectedNodes s.imol False of
    [n] =>
      let atm := atom $ lab s.imol n
          tpe := atm.type.name
       in div
            [ Id $ rightBarID pre, class "cyby_draw_toolbar_right" ]
            [ detail "Element" $ elems atm
            , detail "Isotope" $ massNrs atm
            , detail "Charge"  $ charges atm
            , detail "Type"    $ div [ class "cyby_draw_atomtype"] [Text tpe]
            ]
    _   => div [ Id $ rightBarID pre, class "cyby_draw_toolbar_right" ] []

bottomBar : (pre : String) -> DrawState -> Node DrawEvent
bottomBar pre s =
  div
    [ Id $ bottomBarID pre, class "cyby_draw_toolbar_bottom_inner" ]
    [ template "benzene" phenyl "Benzene" s
    , template "cyclohexane" (ring 6) "Cyclohexane" s
    , template "cyclopentane" (ring 5) "Cyclopentane" s
    , template "cyclopropane" (ring 3) "Cyclopropane" s
    , template "cyclobutane" (ring 4) "Cyclobutane" s
    , template "cycloheptane" (ring 7) "Cycloheptane" s
    , template "cyclooctane" (ring 8) "Cyclooctane" s
    ]

px : Double -> String
px v = show (cast {to = Bits32} v) ++ "px"

export
sketcher : DrawSettings => (pre : String) -> DrawState -> Node DrawEvent
sketcher pre s =
  div
    [ class "cyby_draw_main_content"
    , Id $ sketcherDiv pre
    ]
    [ div
      [ class "cyby_draw_sketcher_div"
      , Id $ sketcherDivInner pre
      ]
      [ topBar pre s
      , leftBar pre s
      , rightBar pre s
      , div
          [ class "cyby_draw_molecule_canvas"
          , Id $ moleculeCanvas pre
          , Event $ MouseMove move
          , Event $ MouseDown down
          , Event $ MouseUp up
          , Event_ True False $ Wheel wheel
          , Event_ True False $ KeyDown (Just . KeyDown . key)
          , Event_ True False $ KeyUp (Just . KeyUp . key)
          , onMouseEnter Focus
          , onMouseLeave Blur
          , onDblClick Expand
          , onResize (\r => EndResizeHW r.height r.width)
          , Str "tabindex" "1"
          , style "width:\{px s.dims.swidth};height:\{px s.dims.sheight}"
          ]
          [Raw s.curSVG]
      , div
          [ class "cyby_draw_toolbar_bottom_outer" ]
          [ bottomBar pre s, abbrs pre s ]
      ]
      , div [ Id $ infoList pre, class "cyby_draw_info_list" ] []
    ]

--------------------------------------------------------------------------------
--          Controller
--------------------------------------------------------------------------------

molCanvasCls : String
molCanvasCls = "cyby_draw_molecule_canvas"

parameters {auto ds : DrawSettings}
           (pre : String)

  canvasCls : List String -> Cmd e
  canvasCls = attr (moleculeCanvas pre) . classes . (molCanvasCls ::)

  rotating : Cmd e
  rotating = canvasCls ["rotating"]

  dragging : Cmd e
  dragging = canvasCls ["dragging"]

  normal : Cmd e
  normal = canvasCls []

  selectCursor : DrawState -> Cmd DrawEvent
  selectCursor s =
    case s.mode of
      Dragging _    => dragging
      Rotating _    => rotating
      RotTempl _ _  => rotating
      Translating _ => dragging
      _             => applyWhenSel s dragging rotating normal

  adjAbbrCls : DrawState -> Cmd DrawEvent
  adjAbbrCls s = attr (abbrID pre) . classes $ abbrCls s
  
  focusCurrentApp : Cmd DrawEvent
  focusCurrentApp = focus (moleculeCanvas pre)

  displayST : DrawState -> Cmd DrawEvent
  displayST s =
    cmdIf (s.curSVG /= s.prevSVG) $
      child (moleculeCanvas pre) (Raw s.curSVG)
  
  adjustBars : DrawState -> Cmd DrawEvent
  adjustBars s =
    replace (topBarID pre) (topBar pre s) <+>
    replace (bottomBarID pre) (bottomBar pre s) <+>
    replace (leftBarID pre) (leftBar pre s) <+>
    adjAbbrCls s
  
  adjustRightBar : DrawState -> Cmd DrawEvent
  adjustRightBar s =
    replace (rightBarID pre) (rightBar pre s) <+>
    adjAbbrCls s

  onResize : (Double -> Double -> DrawEvent) -> Cmd DrawEvent
  onResize f =
    C $ \h => do
      r      <- boundingRect (moleculeCanvas pre)
      h $ f r.height r.width

  dispKeyDown : String -> DrawState -> Cmd DrawEvent
  dispKeyDown "Escape" s = replace (sketcherDiv pre) (sketcher pre s)
  dispKeyDown "c" s =
    cmdIf (s.modifier == Ctrl) $
      let g := selectedSubgraph True s.mol
       in cmdIf (g.order > 0) $
            cmd_ (molToClipboard g) <+> pure (Msg Copied)
  dispKeyDown "x" s =
    cmdIf (s.modifier == Ctrl) $
      let g := selectedSubgraph False s.mol
       in cmdIf (g.order > 0) $
            cmd_ (molToClipboard g) <+>
            pure (KeyDown "Delete") <+>
            pure (Msg Copied)
  dispKeyDown "v" s = cmdIf (s.modifier == Ctrl) fromClipboard
  dispKeyDown "Ctrl" s = selectCursor s
  dispKeyDown _      s = neutral

  displayEv : DrawEvent -> DrawState -> Cmd DrawEvent
  displayEv Focus            s = focusCurrentApp
  displayEv Blur             s = blur (moleculeCanvas pre)
  displayEv (KeyDown k)      s = dispKeyDown k s
  displayEv (KeyUp _)        s = adjustRightBar s
  displayEv (SetElem _)      s = adjustBars s
  displayEv (SelAbbr _)      s = adjustBars s
  displayEv  EnableAbbr      s = adjustBars s
  displayEv (SetBond _)      s = adjustBars s
  displayEv (SetTempl _)     s = adjustBars s
  displayEv SelectMode       s = adjustBars s
  displayEv EraseMode        s = adjustBars s
  displayEv (ChgElem _)      s = adjustRightBar s
  displayEv (Move _ _)       s = selectCursor s
  displayEv MiddleDown       s = selectCursor s
  displayEv MiddleUp         s = selectCursor s
  displayEv LeftUp           s = adjustBars s <+> adjustRightBar s
  displayEv Undo             s = adjustBars s
  displayEv Redo             s = adjustBars s
  displayEv (ZoomIn _)       s = adjustBars s
  displayEv (ZoomOut _)      s = adjustBars s
  displayEv Clear            s = adjustBars s
  displayEv _                s = neutral

  export
  displaySketcher : DrawEvent -> DrawState -> Cmd DrawEvent
  displaySketcher e s = displayEv e s <+> displayST s

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
  -> Node e
displayMol sd g m =
  let cdg    := initGraph g
      G o mg := maybe cdg (\ns => highlight ns cdg) m
   in Raw . curSVG $ initMol sd Fill $ G o mg
