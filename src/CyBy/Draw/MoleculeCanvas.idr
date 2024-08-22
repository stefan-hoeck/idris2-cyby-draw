module CyBy.Draw.MoleculeCanvas

import Chem.Util
import CyBy.Draw.Draw
import CyBy.Draw.Event
import CyBy.Draw.Internal.Abbreviations
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.CoreDims
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Label
import CyBy.Draw.Internal.Role
import CyBy.Draw.Internal.Settings
import CyBy.Draw.PeriodicTableCanvas
import Derive.Prelude
import Geom
import Text.Molfile
import Text.SVG

%default total
%language ElabReflection
%hide Language.Reflection.TTImp.Mode

--------------------------------------------------------------------------------
--          Drawing Mode
--------------------------------------------------------------------------------

public export
data Mode : Type where
  Select      : Mode
  Erase       : Mode
  Draw        : Mode
  SetAtom     : Isotope -> Mode
  SetAbbr     : Abbreviation -> Mode
  SetTempl    : CDGraph -> Mode
  RotTempl    : (start : Point Mol) -> CDGraph -> Mode
  Selecting   : (start : Point Id) -> Mode
  Erasing     : (start : Point Id) -> Mode
  Dragging    : (start : Point Mol) -> Mode
  Rotating    : (start : Point Mol) -> Mode
  Translating : (prev : Mode) -> Mode
  Drawing     : Maybe Abbreviation -> Mode
  PTable      : (hovered : Maybe Elem) -> Mode

%runElab derive "CyBy.Draw.MoleculeCanvas.Mode" [Show,Eq]

endTranslate : Mode -> Mode
endTranslate (Translating m) = m
endTranslate m               = m

--------------------------------------------------------------------------------
--          Hovering Rules
--------------------------------------------------------------------------------

sameAtom : Isotope -> CDAtom -> Bool
sameAtom i a = elem a.atom == i && not (inAnyGroup a) && a.atom.charge == 0

hoverAtom : Mode -> CDAtom -> Bool
hoverAtom Select       _ = True
hoverAtom Erase        _ = True
hoverAtom Draw         y = not (inAnyGroup y)
hoverAtom (SetAtom x)  y = not (sameAtom x y)
hoverAtom (SetAbbr x)  y = map lbl (group y) /= Just x.label
hoverAtom (SetTempl x) y = not (inAnyGroup y)
hoverAtom _            _ = False

hoverDrawing : MolBond -> MolBond -> Bool -> Bool
hoverDrawing (MkBond _ Single NoBondStereo) _ a = not a
hoverDrawing (MkBond _ Single UpOrDown)     b _ = b.stereo /= UpOrDown
hoverDrawing (MkBond _ Single _)            _ _ = True
hoverDrawing (MkBond _ x      _)            b a = not a && b.type /= x

hoverBond : Mode -> MolBond -> CDBond -> (inAbbreviation : Bool) -> Bool
hoverBond Select       _  _ _ = True
hoverBond Erase        _  _ _ = True
hoverBond Draw         mb b a = hoverDrawing mb b.molBond a
hoverBond (SetTempl x) _  _ b = not b
hoverBond _            _  _ _ = False

--------------------------------------------------------------------------------
--          Drawing State
--------------------------------------------------------------------------------

public export
record DrawState where
  [noHints]
  constructor ST
  dims       : SceneDims
  transform  : AffineTransformation
  curPos     : Point transform
  mol        : CDGraph
  undos      : List (CDGraph)
  redos      : List (CDGraph)
  mode       : Mode
  modifier   : Modifier
  bond       : MolBond
  abbr       : Maybe Abbreviation
  hasFocus   : Bool
  ptable     : Maybe Elem

  ||| Current SVG scene rendered to a string
  ||| We keep track of this and the previous one to easily
  ||| decide when to redraw the scene.
  curSVG     : String

  ||| Previous SVG scene rendered to a string
  prevSVG    : String

export %inline
toMol : DrawState -> MolfileAT
toMol ds = toMolfile ds.mol

export %inline
toMolStr : DrawState -> String
toMolStr = writeMolfile . toMol

export %inline
(.imol) : (s : DrawState) -> CDIGraph s.mol.order
s.imol = s.mol.graph

--------------------------------------------------------------------------------
-- State initialization
--------------------------------------------------------------------------------

||| Mode used to scale a molecule.
|||
||| This is used when centering a molecule, for instance, when we begin
||| drawing, or when we just display a molecule and want to zoom in for it
||| to fill the whole scene.
public export
data ScaleMode : Type where
  ||| Scale mode used when initializing a scene for drawing
  Init  : ScaleMode

  ||| Scale mode used when centering the molecule (by pressing the "reset" button)
  Reset : ScaleMode

  ||| Scale mode used when displaying a molecule so that it fills the whole
  ||| scene
  Fill  : ScaleMode

setTransform : AffineTransformation -> DrawState -> DrawState
setTransform tr = {transform := tr, curPos $= convert}

scaleTrans :
     {auto ds : DrawSettings}
  -> Point Id
  -> Scale
  -> AffineTransformation
  -> AffineTransformation
scaleTrans p sc t =
  let v := p - origin
   in translate v <+> scaling (validScale t sc) <+> translate (negate v) <+> t

parameters {auto ds : DrawSettings}

  export
  scaleAt : Point Id -> Scale -> DrawState -> DrawState
  scaleAt p sc s = setTransform (scaleTrans p sc s.transform) s

  ||| Scales the molecule at the current mouse position.
  export
  scaleAtPos : Scale -> DrawState -> DrawState
  scaleAtPos sc s = let P x y := s.curPos in scaleAt (P x y) sc s

  ||| Scales the molecule at the center of the scene
  export
  scaleAtCenter : Scale -> DrawState -> DrawState
  scaleAtCenter sc s = scaleAt (sceneCenter s.dims) sc s

  scaleFromBounds : (scene,mol : Bounds2D Id) -> Scale
  scaleFromBounds c m     =
    min (factor (width c) (width m)) (factor (height c) (height m))
    where
      factor : Double -> Double -> Scale
      factor x y = scale $ (x / (y + 4.0 * cast ds.core.fontSize))

  iniTrans : SceneDims -> ScaleMode -> CDGraph -> AffineTransformation
  iniTrans sd sm (G _ g) =
    let (bs,sc) := scaleToBounds
     in scaleTrans (sceneCenter sd) sc $
          translate (sceneCenter sd - convert (center bs))
    where
      scaleToBounds : (Bounds2D Id, Scale)
      scaleToBounds =
        case sm of
          Init  => (neutral,1.0)
          Reset =>
           let bs := foldMap bounds (Draw.labels g)
            in (bs, min 1.0 (scaleFromBounds (sceneBounds sd) bs))
          Fill =>
           let bs = foldMap bounds (Draw.labels g)
            in (bs, scaleFromBounds (sceneBounds sd) bs)

  initAbbr : Maybe Abbreviation
  initAbbr = case ds.abbreviations of {a :: _ => Just a; [] => Nothing}

export
(.posId) : DrawState -> Point Id
s.posId = convert s.curPos

export
(.posMol) : DrawState -> Point Mol
s.posMol = convert s.curPos

--------------------------------------------------------------------------------
--          Current Molecule
--------------------------------------------------------------------------------

-- Computes the molecule to be drawn based on the current
-- state and mode. This is used both for displaying the current
-- molecule in its editing state as well as for replacing the current
-- molecule with its updated version when an editing step ends
-- Using this for drawing allows us to not store additional information
-- in the drawing mode.
nextMol : DrawSettings => DrawState -> CDGraph
nextMol s =
  case s.hasFocus of
    False => s.mol
    True  => case s.mode of
      Select           => s.mol
      Erase            => s.mol
      Draw             => s.mol
      PTable _         => s.mol
      SetAtom x        => addAtom s.imol x s.curPos
      SetAbbr _        => s.mol
      Erasing p        => select p s.posId s.mol
      Translating _    => s.mol
      Selecting p      => select p s.posId s.mol
      Dragging p       => moveSelected p s.posMol s.mol
      Rotating p       => rotateSelected (s.modifier == Shift) p s.posMol s.mol
      SetTempl t       => addTemplate s.posMol t s.mol
      RotTempl p t     => addTemplate p (rotateTempl False p s.posMol t) s.mol
      Drawing Nothing  => addBond (s.modifier == Shift) s.posMol s.bond s.imol
      Drawing (Just $ A l _ g) => setAbbreviation (s.modifier == Shift) l s.posId g s.mol

--------------------------------------------------------------------------------
--          Editing Molecule
--------------------------------------------------------------------------------

-- overwrites the current molecule, adding it to the `undo` stack
updateMol : (CDGraph -> CDGraph) -> DrawState -> DrawState
updateMol f s =
  let G o g := f s.mol
      cm    := clear s.mol
   in if clear (G o g) == cm then s else
        { mol := G o $ adjAtomTypes g
        , undos $= (cm ::), redos := []
        } s

-- adjusts the atoms fulfilling the given predicate with the given function
-- this will delete any abbreviation associated with these atoms.
modAtomWhere :
     (CDAtom -> Bool)
  -> (MolAtomAT -> MolAtomAT)
  -> DrawState
  -> DrawState
modAtomWhere p f =
  updateMol $ \(G o g) =>
    let ns := filter (p . lab g) (nodes g)
     in delNodes (groupNodes g ns) $ mapIf p {atom $= adj} g
  where
    adj : MolAtomAT -> MolAtomAT
    adj = f . {label := Nothing}

-- adjusts the currently hovered atom with the given function
-- this will delete any abbreviation associated with the hovered atom.
modAtom : (MolAtomAT -> MolAtomAT) -> DrawState -> DrawState
modAtom = modAtomWhere (is Hover)

%inline
setMol : CDGraph -> DrawState -> DrawState
setMol = updateMol . const

-- delete the currently selected atoms
%inline
delete : DrawState -> DrawState
delete = updateMol (clear . deleteSelected)

--------------------------------------------------------------------------------
--          Current Selection
--------------------------------------------------------------------------------

-- draws a rectangle around the currently selected atoms (if any),
-- depending on whether the mouse is currently within the dragging zone
drawSelection : (se : DrawSettings) => DrawState -> List SVGNode
drawSelection s = case s.mode of
  Erasing p    => [fillRect se.selectFG p s.posId]
  RotTempl p _ => rotateTemplScene p s.posMol
  Selecting p  => [fillRect se.selectFG p s.posId]
  _           =>
    -- tests if any atoms are selected and if that's the case, whether
    -- the mouse is currently in the dragging zone.
    case selectionCorners (nextMol s) of
      Nothing      => []
      Just (p1,p2) =>
        let SZ d1 d2 r1 r2 := selectZones (convert p1) (convert p2)
         in if inRectangle s.posId d1 d2
               then [outlineRect se.hoverBG d1 d2]
               else [outlineRectD se.hoverBG r1 r2]

-----------------------------------------------------------------------------
-- Update State
-----------------------------------------------------------------------------

reset : DrawSettings => DrawState -> DrawState
reset s = {transform := iniTrans s.dims Reset s.mol, curPos := origin} s

undo : DrawState -> DrawState
undo s = case s.undos of
  []     => s
  (h::t) => {redos $= (clear s.mol ::), mol := h, undos := t} s

redo : DrawState -> DrawState
redo s = case s.redos of
  []     => s
  (h::t) => {undos $= (clear s.mol ::), mol := h, redos := t} s

changeSelMode : DrawState -> DrawState
changeSelMode s =
  let mode := if s.modifier == Shift then Many else One
   in case hoveredItem s.imol of
        None => {mode := Selecting s.posId, mol $= selectHovered Ignore Ignore} s
        N _  => {mode := Dragging s.posMol, mol $= selectHovered Ignore mode} s
        E _  => {mode := Dragging s.posMol, mol $= selectHovered mode Ignore} s

parameters {auto ds : DrawSettings}
  -- Elaborates the current mode and elevates the fitting argument to a return
  -- value
  export
  applyWhenSel :
       {0 a : Type}
    -> DrawState
    -> (dragging : Lazy a)
    -> (rotating : Lazy a)
    -> (nothing : Lazy a)
    -> a
  applyWhenSel s d r n =
    let pid := s.posId
    in case hoveredItem s.imol of
         None => case selectionCorners s.mol of
           Nothing      => n
           Just (p1,p2) =>
             let SZ d1 d2 r1 r2 := selectZones (convert p1) (convert p2)
              in case inRectangle pid d1 d2 of
                   False => case inRectangle pid r1 r2 of
                     False => n
                     True  => r
                   True  => d
         _ => n

  -- When we move the mouse, we must adjust the current mouse position
  -- in the application state. If the middle mouse button is pressed,
  -- we also translate the drawing area, otherwise we adjust the hovering
  -- state of atoms.
  move : (s : DrawState) -> Point s.transform -> DrawState
  move s p =
    case s.mode of
      Translating _ =>
        let V x y := p - s.curPos
            v     := vid x y
         in setTransform (translate v <+> s.transform) s
      PTable m      => {mode := PTable (hoveredElem s.dims p)} s
      m             =>
        let G o g := s.mol
            gh    := hover (hoverBond m s.bond) (hoverAtom m) (convert p) g
         in { mol := G o gh, curPos := p} s

  -- Pressing the left button typically begins an editing step
  -- If this step depends on the start and end position of the mouse,
  -- we enter a new mode and finish editing on the `leftUp` event.
  -- Otherwise (for instance, when setting the label of an atom at
  -- the mouse position),
  -- the modification happens immediately.
  leftDown : DrawState -> DrawState
  leftDown s =
    let pid := s.posId
     in case s.mode of
          Select      =>
            applyWhenSel
              s
              (updateMode s (Dragging s.posMol))
              (updateMode s (Rotating s.posMol))
              (changeSelMode s)

          Erase       =>
            case hoveredItem s.imol of
              None => {mode := Erasing s.posId} s
              N _  => delete $ {mol $= selectHovered Ignore One} s
              E _  => delete $ {mol $= selectHovered One Ignore} s

          Draw        =>
            case hoveredItem s.imol of
              None => setMol (G _ $ insElemAt s.imol C pid HoverNew) s 
              N x  =>
                if isJust (groupNr s.imol (fst x))
                   then s
                   else {mode := Drawing Nothing, mol $= ifHover Origin} s
              E (E x y $ CB r b)  => 
                let b2 := newBond s.bond.type s.bond.stereo b
                 in setMol (G _ $ insEdge (E x y $ CB r b2) s.imol) s

          SetAtom   i => setMol (cleanup $ nextMol s) s
          SetAbbr a   => {mode := Drawing (Just a), mol $= ifHover Origin} s
          SetTempl  t => setMol (cleanup $ nextMol s) $ {mode := SetTempl t} s
          _ => s
    where updateMode : DrawState -> Mode -> DrawState
          updateMode s m = {mode := m} s

  -- When the left mouse button is lifted, this ends an ongoing editing
  -- or selection process. We typically overwrite the current molecule with
  -- the freshly edited one, adjust the drawing roles of atoms and bonds, and
  -- determine the currently hovered atom anew.
  leftUp : DrawState -> DrawState
  leftUp s =
    case s.mode of
      Selecting _ => {mode := Select, mol := cleanup (nextMol s)} s
      Erasing   _ => delete $ {mode := Erase, mol := cleanup (nextMol s)} s
      Dragging  _ => setMol (cleanup $ nextMol s) $ {mode := Select} s
      Rotating  _ => setMol (cleanup $ nextMol s) $ {mode := Select} s
      Drawing (Just a) => setMol (cleanup $ nextMol s) $ {mode := SetAbbr a} s
      Drawing Nothing  => setMol (cleanup $ nextMol s) $ {mode := Draw} s
      PTable (Just el) => {mode := SetAtom (cast el)} s
      PTable Nothing   => s
      _           => {mol $= cleanup} s

  %inline
  zoomOut, zoomIn : (atPos : Bool) -> DrawState -> DrawState
  zoomOut True  = scaleAtPos 0.8
  zoomOut False = scaleAtCenter 0.8

  zoomIn True  = scaleAtPos 1.25
  zoomIn False = scaleAtCenter 1.25

ifCtrl : (f,g : Lazy (DrawState -> DrawState)) -> DrawState -> DrawState
ifCtrl f g s = if s.modifier == Ctrl then f s else g s

setElemStr : String -> DrawState -> DrawState
setElemStr s = modAtom {elem $= updateIsotope s, charge := 0}

startTemplRot : DrawState -> Mode -> Mode
startTemplRot s (SetTempl g) = RotTempl s.posMol g
startTemplRot s m           = m

stopTemplRot : DrawSettings => DrawState -> Mode -> Mode
stopTemplRot s (RotTempl p g) = SetTempl (rotateTempl False p s.posMol g)
stopTemplRot s m              = m

onKeyDown, onKeyUp : DrawSettings => String -> DrawState -> DrawState
onKeyDown "Escape"  s = {mode := Select, mol $= clear} s
onKeyDown "Delete"  s = delete s
onKeyDown "Shift"   s = {modifier := Shift} s
onKeyDown "Control" s = {modifier := Ctrl, mode $= startTemplRot s} s
onKeyDown "Meta"    s = {modifier := Ctrl, mode $= startTemplRot s} s
onKeyDown "ArrowUp"   s = modAtom {elem $= incIso} s
onKeyDown "ArrowDown" s = modAtom {elem $= decIso} s
onKeyDown "+"       s = ifCtrl (zoomIn True) (modAtom {charge $= incCharge}) s
onKeyDown "-"       s = ifCtrl (zoomOut True) (modAtom {charge $= decCharge}) s
onKeyDown "c"       s = ifCtrl id (setElemStr "C") s
onKeyDown "x"       s = ifCtrl id (setElemStr "X") s
onKeyDown "z"       s = ifCtrl undo (setElemStr "Z") s
onKeyDown "y"       s = ifCtrl redo (setElemStr "Y") s
onKeyDown x         s = setElemStr (toUpper x) s

onKeyUp "Shift"   s = {modifier $= reset Shift} s
onKeyUp "Control" s = {modifier $= reset Ctrl, mode $= stopTemplRot s} s
onKeyUp "Meta"    s = {modifier $= reset Ctrl, mode $= stopTemplRot s} s
onKeyUp _         s = s

enableAbbr : DrawState -> DrawState
enableAbbr s =
  case s.abbr of
    Nothing => s
    Just a => {mode := SetAbbr a, mol $= clear} s

setMassNr : Maybe MassNr -> MolAtomAT -> MolAtomAT
setMassNr m a = let MkI e _ := a.elem in {elem := MkI e m} a

erase : DrawState -> DrawState
erase s =
  case selectedItems s.imol of
    None => {mode := Erase} s
    _    => delete s

endResize : (h,w : Double) -> DrawState -> DrawState
endResize h w s =
  let sd := if h > 2 && w > 2 then SD {sheight = h - 2, swidth = w - 2} else s.dims
   in {dims := sd} s

upd : DrawSettings => DrawEvent -> DrawState -> DrawState
upd (ZoomIn b)    s = zoomIn b s
upd (ZoomOut b)   s = zoomOut b s
upd LeftDown      s = leftDown s
upd LeftUp        s = leftUp s
upd (Move x y)    s = move s (P x y)
upd MiddleDown    s = {mode $= Translating} s
upd MiddleUp      s = {mode $= endTranslate} s
upd Undo          s = undo s
upd Redo          s = redo s
upd (SetElem e)   s = {mode := SetAtom (cast e), mol $= clear} s
upd (ChgElem v)   s = modAtomWhere (is Selected) {elem := cast v, charge := 0} s
upd (ChgCharge v) s = modAtomWhere (is Selected) {charge := v} s
upd (ChgMass v)   s = modAtomWhere (is Selected) (setMassNr v) s
upd (SetTempl e)  s = {mode := SetTempl e, mol $= clear} s
upd (SetBond b)   s = {bond := b, mode := Draw, mol $= clear} s
upd SelectMode    s = {mode := Select} s
upd (KeyDown x)   s = onKeyDown x s
upd (KeyUp x)     s = onKeyUp x s
upd EraseMode     s = erase s
upd Focus         s = {hasFocus := True} s
upd Blur          s = {hasFocus := False} s
upd Clear         s = setMol (G 0 empty) s
upd Expand        s = updateMol expand s
upd Center        s = reset s
upd EnableAbbr    s = enableAbbr s
upd (SelAbbr a)   s = {mode := SetAbbr a, abbr := Just a, mol $= clear} s
upd (Msg _)       s = s
upd EndResize     s = s
upd (EndResizeHW h w) s = endResize h w s
upd StartPSE      s = {mode := PTable Nothing} s

||| Convert an `AffineTransformation` to a transformation to be
||| used in an SVG element.
export
toTransform : AffineTransformation -> Transform
toTransform (AT (LT s r) (V x y)) =
  let co  := s.value * cos r.value
      si  := s.value * sin r.value
   in Matrix co si (negate si) co x y

scene : DrawSettings => DrawState -> SVGNode
scene s =
  case s.mode of
    PTable me => displayPSE s.dims me
    _         =>
      let m := nextMol s
       in g
            [transform $ toTransform s.transform]
            (drawMolecule m ++ drawSelection s)

display : DrawSettings => DrawState -> SVGNode
display s =
  svg
    [ xmlns_2000
    , width 100.perc
    , height 100.perc
    , viewBox 0.u 0.u s.dims.swidth.u s.dims.sheight.u
    ] [scene s]

export
update : DrawSettings => DrawEvent -> DrawState -> DrawState
update e s =
  let s2 := upd e s
   in {prevSVG := s.curSVG, curSVG := render (display s2)} s2

--------------------------------------------------------------------------------
-- Initialization
--------------------------------------------------------------------------------

parameters {auto ds : DrawSettings}

  initST : SceneDims -> ScaleMode -> CDGraph -> DrawState
  initST sd sm g =
    ST
      { dims       = sd
      , curPos     = P (sd.swidth / 2.0) (sd.sheight / 2.0)
      , transform  = iniTrans sd sm g
      , mol        = g
      , undos      = []
      , redos      = []
      , mode       = Draw
      , modifier   = NoMod
      , bond       = MkBond False Single NoBondStereo
      , abbr       = initAbbr
      , hasFocus   = False
      , ptable     = Nothing
      , curSVG     = ""
      , prevSVG    = ""
      }

  ||| Initializes the drawing state for the given mol graph.
  |||
  ||| The `SceneDims` are used for centering the molecule, as well
  ||| as for scaling it to fill the scene in case the given bool is
  ||| set to `True`.
  export
  initMol : SceneDims -> ScaleMode -> CDGraph -> DrawState
  initMol sd sm g =
    let s := initST sd sm g
     in {curSVG := render (display s)} s
  
  export %inline
  init : SceneDims -> ScaleMode -> String -> DrawState
  init sd sm = initMol sd sm . readMolfile
  
  export %inline
  fromMol : SceneDims -> ScaleMode -> MolGraphAT -> DrawState
  fromMol sd sm = initMol sd sm . initGraph
