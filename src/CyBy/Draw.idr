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
import public CyBy.Draw.I18n
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
%hide Data.Linear.(.)
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

moleculeCanvas : String -> Ref Div
moleculeCanvas pre = Id "\{pre}-molecule-canvas"

sketcherDiv : String -> Ref Div
sketcherDiv pre = Id "\{pre}-sketcher-div"

elemsID : String -> Ref Div
elemsID pre = Id "\{pre}-elems"

export
infoID : String -> Ref Div
infoID pre = Id "\{pre}-draw-info"

detailsID : String -> Ref Div
detailsID pre = Id "\{pre}-draw-details"

utilsID : String -> Ref Div
utilsID pre = Id "\{pre}-utils"

templatesID : String -> Ref Div
templatesID pre = Id "\{pre}-templates"

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
detail ttl n = div [class listEntry] [label [] [Text ttl], n]

currAbbr : Mode -> Maybe String
currAbbr (SetAbbr a) = Just a.label
currAbbr _           = Nothing

parameters {auto de : Sink DrawEvent}

  elements : MolAtomAT -> HTMLNode
  elements a =
    selectFromListBy values (a.elem.elem ==) symbol ChgElem [title "set element"]

  charges : MolAtomAT -> HTMLNode
  charges a =
    selectFromListBy chs (a.charge ==) (show . value) ChgCharge
      [title "set charge"]
    where
      chs : List Charge
      chs = mapMaybe refineCharge [(-8) .. 8]

  massNrs : MolAtomAT -> HTMLNode
  massNrs a =
    selectFromListBy (masses a.elem.elem) (a.elem.mass ==) dispMass ChgMass
      [title "set mass number"]
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
      [classes (icon::cs),active a,onClick ev,title ttl]
      [child]

  abbrs : (ds : DrawSettings) => (pre : String) -> DrawState -> HTMLNode
  abbrs pre s =
    selectFromListBy
      (Nothing :: map Just ds.abbreviations)
      (\v => currAbbr s.mode == map label v)
      (maybe "--abbreviation--" label)
      (maybe Redraw SelAbbr)
      [abbrActive s]

  bondIcon : MolBond -> String -> DrawState -> HTMLNode -> HTMLNode
  bondIcon b title s = icon [] (SetBond b) (drawing b s) title

  utils :
       {auto ds : DrawSettings}
    -> (pre     : String)
    -> (topadd  : HTMLNodes)
    -> DrawState
    -> HTMLNode
  utils {ds} pre topadd s =
    div
      [ Id $ utilsID pre, class drawUtils ] $
      [ icon [] SelectMode (s.mode == Select) "select" select
      , icon [] EraseMode (s.mode == Erase) "erase" erase
      , icon [] Clear False "clear" trash
      , nodeSep
      , disable (s.undos == []) $ icon [] Undo False "undo" undo
      , disable (s.redos == []) $ icon [] Redo False "redo" redo
      , nodeSep
      , icon [] Center False "center" center
      , disable (maxZoom s.transform) $ icon [] (ZoomIn False) False "zoom in" zoomIn
      , disable (minZoom s.transform) $ icon [] (ZoomOut False) False "zoom out" zoomOut
      , nodeSep
      , bondIcon (cast Single) "single bond" s single
      , bondIcon (fromStereo Up) "single bond up" s bondUp
      , bondIcon (fromStereo Down) "single bond down" s bondDown
      , bondIcon (fromStereo Either) "single bond up or down" s bondEither
      , bondIcon (cast Types.Dbl) "double bond" s double
      , bondIcon (cast Triple) "triple bond" s triple
      , nodeSep
      ] ++ topadd

  template : CDGraph -> String -> DrawState -> HTMLNode -> HTMLNode
  template g nm s = icon [] (SetTempl g) (s.mode == SetTempl g) "template \{nm}"

  elemIcon : DrawState -> String -> Elem -> HTMLNode
  elemIcon s t e = icon [elemText e] (SetElem e) (setting e s) t (Text $ symbol e)

  elems pre s =
    div
      [ Id $ elemsID pre, class drawElems ]
      [ elemIcon s "Boron" B
      , elemIcon s "Carbon" C
      , elemIcon s "Oxygen" O
      , elemIcon s "Nitrogen" N
      , elemIcon s "Fluorine" F
      , elemIcon s "Phosphorous" P
      , elemIcon s "Sulfur" S
      , elemIcon s "Chlorine" Cl
      , elemIcon s "Bromine" Br
      , nodeSep
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
         in [ detail "Element"  $ elements atm
            , detail "Isotope"  $ massNrs atm
            , detail "Charge"   $ charges atm
            , detail "Type"     $ div [class listEntryValue] [Text tpe]
            , detail "x-Coord." $ div [class listEntryValue] [Text cx]
            , detail "y-Coord." $ div [class listEntryValue] [Text cy]
            ]
      _   => case selectedEdges s.imol of
        [(x,y)] =>
          let px := point $ position $ atom $ lab s.imol x
              py := point $ position $ atom $ lab s.imol y
              d  := printDouble 3 $ distance px py
              a  := angleOrZero (px - py)
              a' := printDouble (S Z) $ toDegree $ if a >= Angle.pi then (a - Angle.pi) else a
           in [ detail "Length"   $ div [class listEntryValue] [Text "\{d} Å"]
              , detail "Angle"    $ div [class listEntryValue] [Text "\{a'}°"]
              ]
        _       => []

  details : (pre : String) -> DrawState -> HTMLNode
  details pre s =
    div
      [ Id $ detailsID pre, class drawDetails ]
      [ h1 [] ["Details"]
      , ul [] (separate $ detailItems pre s)
      ]

  templates : DrawSettings => (pre : String) -> DrawState -> HTMLNode
  templates pre s =
    div
      [ Id $ templatesID pre, class drawTemplates ]
      [ template phenyl "benzene" s benzene
      , template (ring 6) "cyclohexane" s cyclohexane
      , template (ring 5) "cyclopentane" s cyclopentane
      , template (ring 3) "cyclopropane" s cyclopropane
      , template (ring 4) "cyclobutane" s cyclobutane
      , template (ring 7) "cycloheptane" s cycloheptane
      , template (ring 8) "cyclooctane" s cyclooctane
      , nodeSep
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
      [ class sketcher, Id $ sketcherDiv pre ]
      [ utils pre topadd s
      , elems pre s
      , div [class drawInfo, Id $ infoID pre] [details pre s]
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
      , templates pre s
      ]

export
cybyDrawBtn : Sink e => String -> e -> Attributes Tag.Button -> HTMLNode
cybyDrawBtn s e as = button (onClick e :: as) [Text s]

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

  selectCursor : DrawState -> Act ()
  selectCursor s =
    attr (moleculeCanvas pre) $ dragMode $ case s.mode of
      Dragging _    => Dragging
      Rotating _    => Rotating
      RotTempl _ _  => Rotating
      Translating _ => Dragging
      _             => applyWhenSel s Dragging Rotating None

  displayST : (force : Bool) -> DrawState -> Act ()
  displayST force s = Prelude.do
    when s.isActive $ focus (moleculeCanvas pre)
    when (force || s.curSVG /= s.prevSVG) $
      child (moleculeCanvas pre) (Raw s.curSVG)

  adjustBars : DrawState -> Act ()
  adjustBars s = Prelude.do
    topadd <- ex.buttons (DE pre) s
    replace (utilsID pre) (utils pre topadd s)
    replace (templatesID pre) (templates pre s)
    replace (elemsID pre) (elems pre s)
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

parameters {auto ex : Extension}
           {auto lc : DrawLocal}
           (getDS   : Act DrawSettings)

  doact : Sink DrawEvent => String -> DrawState -> DrawEvent -> Act DrawState
  doact pre s e = Prelude.do
    logLoggable e
    ds <- getDS
    let s2 := update e s
    displaySketcher pre e s2 $> s2

  ||| An editor for molecules.
  export
  molWidget : String -> SceneDims -> Maybe MolfileAT -> Act (Widget DrawState)
  molWidget pre sd m = Prelude.do
    ds     <- getDS
    E es   <- event DrawEvent
    let st := fromMol sd Init (maybe (G 0 empty) graph m)
    topadd <- ex.buttons (DE pre) st
    let nd := sketcher pre topadd st
    pure $ Widget.W nd $
      P.evalScans1 st (doact pre) es |> (\x => cons st x)

  ||| An editor for molecules.
  export
  molEdit : SceneDims -> Editor MolfileAT
  molEdit sd =
    E $ \m => Prelude.do
      ui <- map interpolate uniqueID
      map (Valid . toMolfile . mol) <$> molWidget ui sd m

||| The default `Extension`
export %hint
NoExt : Extension
NoExt =
  E
    { doExport = storeSVG . exportSVG
    , buttons  = \_,s => pure [expBtn "Save..." s]
    , adjust   = \_,_,s => disableExport s
    }
