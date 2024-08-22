module CyBy.Draw.Internal.Graph

import Chem.Util
import CyBy.Draw.Internal.CoreDims
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.Role
import Data.Graph.Indexed.Subgraph
import Derive.Prelude
import Geom
import Text.Molfile

%language ElabReflection
%default total

||| In several places, we decide what to do on a UI event
||| based on whether any nodes or edges are selected or being
||| hovered over.
public export
data NOE : (a,b : Type) -> Type where
  None  : NOE a b      -- the empty case
  N     : a -> NOE a b -- the "nodes" case
  E     : b -> NOE a b -- the "edges" case

||| Creates an `NOE` from two `Maybe`s.
export
noe : Maybe a -> Lazy (Maybe b) -> NOE a b
noe mn me = maybe (maybe None E me) N mn

||| Like `noe`, but edges take precedence.
export
eon : Lazy (Maybe a) -> Maybe b -> NOE a b
eon mn me = maybe (maybe None N mn) E me

||| Creates an `NOE` from two `Lists`s.
|||
||| This returns `None` in case both lists are empty.
export
eons : List a -> Lazy (List b) -> NOE (List a) (List b)
eons _  es@(_::_) = E es
eons ns []        = if null ns then None else N ns

||| Bond type used in cyby-draw.
|||
||| This is an mol-file bond paired with a role used for drawing.
public export
record CDBond where
  constructor CB
  role    : Role
  molBond : MolBond

%runElab derive "CDBond" [Show,Eq]

export %inline
Cast CDBond Role where cast = role

export %inline
ModRole CDBond where modRole f = {role $= f}

||| Graph type used for drawing molecules.
public export
0 CDGraph : Type
CDGraph = Graph CDBond CDAtom

||| Order-indexed graph type used for drawing molecules.
public export
0 CDIGraph : Nat -> Type
CDIGraph k = IGraph k CDBond CDAtom

export %inline
toMolGraph : Graph CDBond CDAtom -> MolGraphAT
toMolGraph = bimap CDBond.molBond CDAtom.atom

export %inline
toMolfile : Graph CDBond CDAtom -> MolfileAT
toMolfile = MkMolfile "" "created by cyby-draw 1.0" "" . toMolGraph

||| Initialize a mol-file graph (with perceived atom types) to be used
||| in one of the drawing canvases. This includes normalizing the
||| molecule to a bond-length of 1.25 Angstrom.
export
initGraph : MolGraphAT -> CDGraph
initGraph (G o g) = G o $ bimap (CB None) (CA None) (normalizeMol g)

||| Reads and initializes a `CDGraph` from a mol-file string.
export
readMolfileE : String -> Either String CDGraph
readMolfileE mol =
  case readMol {es = [MolParseErr]} mol of
    Left (Here e)             => Left "\{e}"
    Right (MkMolfile _ _ _ g) => Right $ initGraph (perceiveMolAtomTypes g)

||| Like `readMolfileE` but returns the empty graph in case of a read error.
export
readMolfile : String -> CDGraph
readMolfile = either (const $ G 0 empty) id . readMolfileE

||| Re-calculates the atom types of a mol graph.
export
adjAtomTypes : {k : _} -> CDIGraph k -> CDIGraph k 
adjAtomTypes =
  mapWithAdj (\(A (CA r a) ns) => CA r $ calcMolAtomType (map molBond ns) a)

--------------------------------------------------------------------------------
-- Bond CyCling
--------------------------------------------------------------------------------

adjStereo : BondStereo -> MolBond -> MolBond
adjStereo Up   (MkBond fs _ Up)   = MkBond (not fs) Single Up
adjStereo Down (MkBond fs _ Down) = MkBond (not fs) Single Down
adjStereo bs   b                  = MkBond True Single bs

nextType : BondOrder -> BondOrder
nextType Single = Dbl
nextType Dbl    = Triple
nextType Triple = Single

export
newBond : BondOrder -> BondStereo -> MolBond -> MolBond
newBond Single NoBondStereo b =
  if b.stereo == NoBondStereo then cast $ nextType b.type else cast Single
newBond Single s            b = adjStereo s b
newBond Dbl    _            _ = cast Dbl
newBond Triple _            _ = cast Triple

--------------------------------------------------------------------------------
-- Highlighting
--------------------------------------------------------------------------------

hlAdj : IArray o Bool -> Fin o -> Adj o CDBond CDAtom -> Adj o CDBond CDAtom
hlAdj arr x (A l ns) =
  case arr `at` x of
    False => A l ns
    True  => A (set Highlight l) (mapKV (setIf Highlight . at arr) ns)

||| Highlight the nodes corresponding to the given natural numbers.
export
highlight : List Nat -> CDGraph -> CDGraph
highlight ns (G o $ IG gr) =
  let arr := fromPairs o False (map (,True) ns)
   in G o $ IG (mapWithIndex (hlAdj arr) gr)

--------------------------------------------------------------------------------
-- Visibility and Abbreviations
--------------------------------------------------------------------------------

||| Returns the number of the abbreviation group of an atom (if any).
export %inline
groupNr : CDIGraph k -> Fin k -> Maybe Nat
groupNr g = map nr . label . atom . lab g

||| True, if the given node is part of an abbreviation.
export
inAbbreviation : CDIGraph k -> Fin k -> Bool
inAbbreviation g = isJust . groupNr g

||| Returns the largest group number in the given graph.
export %inline
maxGroupNr : {k : _} -> CDIGraph k -> Nat
maxGroupNr = foldr (\a,n => maybe n (max n . nr) a.atom.label) 0

||| True, if any neighbour of the given node is part of the given
||| abbreviation (given as its ID).
export
anyNotInGroup : CDIGraph k -> Fin k -> Nat -> Bool
anyNotInGroup g x n = any (not . inGroup n) (neighbourLabels g x)

||| Custom label to be displayed for a node (if any).
export
customLabel : CDIGraph k -> Fin k -> Maybe String
customLabel  g x = do
  G n lbl <- label . atom $ lab g x
  guard $ lbl /= "" && anyNotInGroup g x n
  pure lbl

||| An atom is visible, if a) it is not part of an abbreviation, or b),
||| at least one of its neighbours is not part of an abbreviation.
export
visible : CDIGraph k -> Fin k -> Bool
visible g x =
  case Atom.label . atom $ lab g x of
    Nothing      => True
    Just (G n _) => anyNotInGroup g x n

||| We show an atom's label if a) it is a non-carbon, b) it is an isolate
||| carbon (no explicit neighbours), or c) `s.showC` is set to `True`
export
labelVisible : (showC : Bool) -> CDIGraph k -> Fin k -> Bool
labelVisible showC g x =
  let A (CA _ a) ns  := adj g x
      MkI e m := a.elem
   in    showC
      || null ns
      || a.type.name == "C.allene"
      || e /= C 
      || isJust m
      || a.charge /= 0
      || isJust (customLabel g x)

||| Returns a list of those nodes of a molecule that will be visible
||| in the drawing, that is, nodes that are not hidden because
||| they are part of an abbreviation.
export
visibleNodes : {k : _} -> CDIGraph k -> List (Fin k)
visibleNodes g = filter (visible g) (nodes g)

nonAbbreviatedNodes : {k : _} -> CDIGraph k -> List (Fin k)
nonAbbreviatedNodes g = filter (not . inAnyGroup . lab g) (nodes g)

||| Returns the list of visible neighbours of an atome, that is,
||| neighbours that are not hidded because they are part of an
||| abbreviation.
export
visibleNeighbours : CDIGraph k -> Fin k -> List (Fin k)
visibleNeighbours g x = filter (visible g) (neighbours g x)

||| Returns a list of those nodes of a molecule that are hidden
||| because they are part of an abbreviation.
export
hiddenNodes : {k : _} -> CDIGraph k -> List (Fin k)
hiddenNodes g = filter (not . visible g) (nodes g)

||| Returns a list of those edges of a molecule that will be visible
||| in the drawing, that is, edges that are not hidden because
||| they are part of an abbreviation.
export
visibleEdges : {k : _} -> CDIGraph k -> List (Edge k CDBond)
visibleEdges g = filter (\(E x y _) => visible g x && visible g y) (edges g)

||| Given a list `ns` of nodes in a molecule, returns a list containing
||| also the other nodes belonging to the same abbreviations (if any)
||| as the nodes in `ns`.
||| transitively via abbreviations.
export
groupNodes : {k : _} -> CDIGraph k -> (ns : List (Fin k)) -> List (Fin k)
groupNodes g ns =
  let gs@(_::_) := ns >>= toList . groupNr g | Nil => Nil
   in filter (any (`elem` gs) . groupNr g) (hiddenNodes g)

||| Given a list `ns` of nodes in a molecule, returns a list containing
||| also the other nodes belonging to the same abbreviations (if any)
||| as the nodes in `ns`.
||| transitively via abbreviations.
export
plusGroupNodes : {k : _} -> CDIGraph k -> (ns : List (Fin k)) -> List (Fin k)
plusGroupNodes g ns = ns ++ groupNodes g ns

--------------------------------------------------------------------------------
-- Geometry
--------------------------------------------------------------------------------

||| Returns the position of the given node in a mol graph.
export %inline
pointAt : CDIGraph k -> Fin k -> Point Id
pointAt g = pointId . lab g

||| Computes the angles of all visible bonds connecting the given node.
export
bondAngles : CDIGraph k -> Fin k -> List Angle
bondAngles g x =
  let p  := pointId (lab g x)
      ns := lab g <$> visibleNeighbours g x
   in mapMaybe (\k => angle $ pointId k - p) ns

parameters {k : Nat}
           {auto cd : CoreDims}

  ||| Finds the visible node closest to the given point, but only
  ||| if it is closer than the defined atom radius and it fulfills
  ||| the given predicate.
  export
  closestNodeWhere : (Fin k -> Bool) -> Point Id -> CDIGraph k -> Maybe (Fin k)
  closestNodeWhere pred p g = do
    x <- minBy (distance p . pointId . lab g) . filter pred $ nodes g
    let q := pointAt g x
    guard $ near p q cd.radiusAtom
    pure x

  ||| Finds the visible node closest to the given point, but only
  ||| if it is closer than the defined atom radius.
  export %inline
  closestNode : Point Id -> CDIGraph k -> Maybe (Fin k)
  closestNode p g = closestNodeWhere (visible g) p g

  ||| Finds the visible edge closest to the given point, but only
  ||| if it is closer than the defined atom radius.
  export
  closestEdge : Point Id -> CDIGraph k -> Maybe (Edge k CDBond)
  closestEdge p g = do 
    ed <- minBy distEdge $ visibleEdges g
    guard $ distEdge ed <= cd.radiusAtom
    pure ed

    where
      distEdge : Edge k n -> Double
      distEdge (E x y _) = distanceToLineSegment p (pointAt g x) (pointAt g y)

  ||| Returns the item (node or edge) closest to the current mouse position.
  export
  closestItem : Point Id -> CDIGraph k -> NOE (Fin k) (Edge k CDBond)
  closestItem p g = noe (closestNode p g) (closestEdge p g)

  ||| Generously approximates the bounds of an atom in the drawing.
  export
  approxBounds : CDIGraph k -> Fin k -> Bounds2D Id
  approxBounds g x =
    case visible g x of
      False => neutral
      True  => case group $ lab g x of
        Nothing      =>
          let r := 2 * cd.radiusAtom
              P x y := pointAt g x
           in BS (range (x-r) (x+r)) (range (y-r) (y+r))
        Just (G _ l) =>
          let w := cast {to = Double} (length l * cd.fontSize)
              h := cast {to = Double} cd.fontSize
              P x y := pointAt g x
           in BS (range (x-w) (x+w)) (range (y-h) (y+h))

--------------------------------------------------------------------------------
--          Hovering
--------------------------------------------------------------------------------

||| Removes all roles from atoms and bonds in the given graph.
export %inline
clear : CDGraph -> CDGraph
clear = bimap clear clear

||| Unset all roles with the exception of `Hover` and `Selected`
export %inline
cleanup : CDGraph -> CDGraph
cleanup = bimap (keep Persistent) (keep Persistent)

%inline
unHover : {k : _} -> CDIGraph k -> CDIGraph k
unHover = bimap (unset Hover) (unset Hover)

hoverE : Fin k -> Fin k -> Fin k -> Adj k CDBond CDAtom -> Adj k CDBond CDAtom
hoverE x y z (A a ns) =
  if x == z || y == z
     then A a $ mapKV (\w => setIf Hover (w == x || w == y)) ns
     else A a ns

||| Adjusts the `Hovering` flag of all atoms and edges in the molecule.
||| The visible atom closest to the given point is set to
||| `Hover` if it is not further away than `radiusAtom`.
|||
||| Otherwise, the visible edge closest to the given point is set to
||| `Hovering` if it is not further away than `radiusAtom`.
|||
||| If the atom, over which the mouse hovers is part of an abbreviation,
||| all other atoms in the abbreviations will be set to `Hovering` as well.
|||
||| The `hatom` predicate is used to figure out if we can currently hover over
||| a given atom.
|||
||| The `hbond` predicate is used to figure out if we can currently hover over
||| a given bond (its bool argument should be `True`, if the bond is connected
||| to at least one atom in an abbreviation group)
export
hover :
     {k : _}
  -> {auto cd : CoreDims}
  -> (hbond : CDBond -> Bool-> Bool)
  -> (hatom : CDAtom -> Bool)
  -> Point Id
  -> CDIGraph k
  -> CDIGraph k
hover hbond hatom p g0 =
  let g := unHover g0
   in case closestItem p g of
        N n         =>
          if hatom (lab g n)
             then mapWithCtxt (\x,(A a _) => setIf Hover (x == n) a) g
             else g
        E (E x y b) =>
          if hbond b (inAnyGroup (lab g x) || inAnyGroup (lab g y))
             then mapCtxt (hoverE x y) g
             else g
        None        => g

||| Adds the given role to the currently hovered atoms
export %inline
ifHover : Role -> CDGraph -> CDGraph
ifHover r = map (\x => setIf r (is Hover x) x)

||| Returns the currently hovered edges or atoms atoms
export %inline
hoveredItem : {k : _} -> CDIGraph k -> NOE (Fin k, CDAtom) (Edge k CDBond)
hoveredItem g =
  eon (find (is Hover . snd) (labNodes g)) (find (is Hover . label) (edges g))

||| Selects the currently hovered atoms and bonds.
|||
||| The `SelectMode` flags indicate, if currently selected items should
||| be kept or not, or if no item should be selected at all. The first
||| value is used for edge selection and the second for node selection.
export %inline
selectHovered : SelectMode -> SelectMode -> CDGraph -> CDGraph
selectHovered em nm = bimap (selectIfHovered em) (selectIfHovered nm)

--------------------------------------------------------------------------------
--          Selecting Nodes
--------------------------------------------------------------------------------

public export
record SelectZones where
  constructor SZ
  dragUL : Point Id -- upper left corner for dragging
  dragLR : Point Id -- lower right corner for dragging
  rotUL  : Point Id -- upper left corner for rotating
  rotLR  : Point Id -- lower right corner for rotating

||| Selects all nodes that are a) currently being hovered over, or visible
||| and in the given rectangle.
export
select : (start,end : Point Id) -> CDGraph -> CDGraph
select s e (G o g) = G o $ mapWithCtxt sel g
  where
    sel : Fin o -> Adj o CDBond CDAtom -> CDAtom
    sel n (A a _) =
      let p := pointId a
       in setIf Selected (is Hover a || (visible g n && inRectangle p s e)) a

||| Returns `True` if the given node is currently selected.
|||
||| In case the `includeEdges` flag is set to `True`, this will also
||| return `True` if one of the edges connecting the node is currently
||| selected.
export
isSelected : CDIGraph k -> (includeEdges : Bool) -> Fin k -> Bool
isSelected g include n =
  let A a bs := adj g n
   in is Selected a || (include && any (is Selected) bs)

||| The list of currently selected nodes.
export
selectedNodes : {k : _} -> CDIGraph k -> (includeEdges : Bool) -> List (Fin k)
selectedNodes g include = filter (isSelected g include) (nodes g)

||| The list of currently selected edges.
export
selectedEdges : {k : _} -> CDIGraph k -> List (Fin k, Fin k)
selectedEdges =
  mapMaybe (\(E x y b) => if is Selected b then Just (x,y) else Nothing) . edges

export
selectedItems : {k : _} -> CDIGraph k -> NOE (List $ Fin k) (List (Fin k, Fin k))
selectedItems g = eons (selectedNodes g False) (selectedEdges g)

nodeBounds : CDIGraph k -> Fin k -> Bounds2D Mol
nodeBounds g = bounds . lab g

edgeBounds : CDIGraph k -> (Fin k,Fin k) -> Bounds2D Mol
edgeBounds g (x,y) = bounds (lab g x) <+> bounds (lab g y)

||| Computes the top left and bottom right corner of the bounding box
||| containing the currently selected atoms (if any)
export
selectionCorners : CDGraph -> Maybe (Point Mol, Point Mol)
selectionCorners (G o g) =
  case selectedItems g of
    N ns@(_ :: _ :: _) => corners $ foldMap (nodeBounds g) ns
    E ps               => corners $ foldMap (edgeBounds g) ps
    _                  => Nothing

||| Checks, if there is enough space to grab the box in the canvas.
export
selectZones : (s : CoreDims) => (p1,p2 : Point Id) -> SelectZones
selectZones p1 p2 =
  let b  := s.selectBufferSize
      bs := bounds p1 <+> bounds p2

      -- translation vector for the two corners of the inner buffer
      -- this is zero if the bounding box formed by `p1` and `p2` is
      -- already large enough, otherwise both dimensions are expanded as
      -- needed
      vd := scale 0.5 $ V (max 0 (b - width bs)) (max 0 (b - height bs))
      vr := vid b b
      d1 := translate (negate vd) p1
      d2 := translate vd p2
   in SZ d1 d2 (translate (negate vr) d1) (translate vr d2)

--------------------------------------------------------------------------------
--          Editing Molecules
--------------------------------------------------------------------------------

||| Creates an uncharged atom at the given position and with the given role.
export
isotopeAt : Isotope -> Point Id -> Role -> CDAtom
isotopeAt i p r =
  let pos := toCoords (convert p) [0,0,0]
   in CA r (MkAtom i 0 pos NoRadical 0 unknown () Nothing)

||| Creates an uncharged atom at the given position and with the given role.
export %inline
elemAt : Elem -> Point Id -> Role -> CDAtom
elemAt = isotopeAt . cast

||| Inserts an uncharged atom at the given position and with the given
||| role.
export %inline
insIsotopeAt :
     {k : _}
  -> CDIGraph k
  -> Isotope
  -> Point Id
  -> Role
  -> CDIGraph (S k)
insIsotopeAt g i p r = insNode g (isotopeAt i p r)

||| Inserts an uncharged atom at the given position and with the given
||| role.
export %inline
insElemAt : {k : _} -> CDIGraph k -> Elem -> Point Id -> Role -> CDIGraph (S k)
insElemAt g = insIsotopeAt g . cast

||| Computes the preferred angle for a new bond based on the bond type
||| and angles to already existing bonds.
export
preferredAngle : (hasTriple : Bool) -> List Angle -> Angle
preferredAngle _     []  = (negate 1.0 / 6.0) * pi
preferredAngle True  [x] = x + pi
preferredAngle False [x] =
  if (x >= zero && x <= halfPi) || (x >= pi && x <= threeHalfPi)
     then x + twoThirdPi
     else x - twoThirdPi
preferredAngle _     xs  = largestBisector xs

||| Preferred position for a new atom bound to an existing one based on the
||| largest bisector of angles between existing bonds
export
bestPos : CDIGraph k -> MolBond -> Fin k -> Point Id -> Point Id
bestPos g b n p =
  let hasTrpl  := any ((Triple ==) . type) (b::map molBond (edgeLabels g n))
      newAngle := preferredAngle hasTrpl (bondAngles g n)
   in translate (polar BondLengthInPixels newAngle) p

||| Equally spaced sequence of `s.angleSteps` angles from 0 until 2pi.
export
stepAngles : (s : CoreDims) => List Angle
stepAngles =
  let step = angle (TwoPi / cast s.angleSteps)
   in map (\x => cast x * step) [0.. pred s.angleSteps]

||| Suggested position for a new atom based on the current mouse position.
||| The boolean flag indicates if "Shift" is currently down, in which case
||| we just return the current point.
export
suggestedPos : CoreDims => Bool -> (atom, current : Point Id) -> Point Id
suggestedPos True pa pc = pc
suggestedPos False pa pc =
  let Just mouseAngle := angle (pc - pa) | Nothing => pc
      Just bondAngle  := closestAngle mouseAngle stepAngles | Nothing => pc
   in translate (polar BondLengthInPixels bondAngle) pa

||| Draws a new bond from the given node to the suggested position.
||| If another atom is already close to the current mouse position
||| or the suggested position, connect the two atoms instead.
export
bondTo :
     {k : _}
  -> {auto s : CoreDims}
  -> CDBond
  -> Fin k
  -> (current, suggested : Point Id)
  -> CDIGraph k
  -> Either (CDIGraph k) (CDIGraph $ S k)
bondTo b n pc ps g =
  maybe
    (Right $ insEdge (edge n b) (insElemAt g C ps New))
    (\e => Left $ insEdge e g)
    (closeEdge pc <|> closeEdge ps)
  where
    closeEdge : Point Id -> Maybe (Edge k CDBond)
    closeEdge p = closestNode p g >>= \k => mkEdge n k b

||| From two graphs, returns pairs of visible nodes closest
||| to each other (but no farther apart than `s.radiusAtom`).
export
nodesToMerge :
     {k,m : _}
  -> {auto s : CoreDims}
  -> CDIGraph k
  -> CDIGraph m
  -> List (Fin k, Fin m)
nodesToMerge g t =
  mapMaybe
    (\x => (x,) <$> closestNode (pointAt g x) t)
    (nonAbbreviatedNodes g)

-- Offset between origin atom and template atoms as a vector in `Mol` space.
offset : CDIGraph k -> CDIGraph m -> List (Fin k, Fin m) -> Vector (transform Mol)
offset _  _  []             = vzero
offset g1 g2 ((n1,n2) :: _) = point (lab g1 n1) - point (lab g2 n2)

-- create new bonds between the merging template atoms and the corresponding
-- neighbours of the original molecule
newEdges :
     {k,m : _}
  -> CDIGraph m
  -> List (Fin k, Fin m)
  -> List (Fin k, Fin m, CDBond)
newEdges t ps = do
  (a1,a2) <- ps
  (\(x,l) => (a1,x,l)) <$> neighboursAsPairs t a2

incNode : {m : _} -> (k : Nat) -> Fin m -> Maybe (Fin $ k + m)
incNode k x = tryNatToFin (k + finToNat x)

||| After moving or rotating the selected nodes in a graph,
||| check for pairs of close nodes and merge them.
export
mergeCloseNodes : CoreDims => {k:_} -> CDIGraph k -> CDGraph
mergeCloseNodes g =
  let ns        := filter (not . inAbbreviation g) (selectedNodes g True)
      lMergeN   := mapMaybe closestPair ns
      offset    := negate $ offset g g lMergeN
      lnewBonds := mapMaybe (\(x,y,l) => mkEdge x y l) (newEdges g lMergeN)
      mol'      := insEdges lnewBonds $ mapIf doAdjust (translate offset) g
   in delNodes (map snd lMergeN) mol'

  where
    canSelfMerge : Fin k -> Bool
    canSelfMerge x =
      let a := lab g x in not (isSelected g True x || inAnyGroup a)

    doAdjust : CDAtom -> Bool
    doAdjust a = is Selected a

    closestPair : Fin k -> Maybe (Fin k, Fin k)
    closestPair x = (x,) <$> closestNodeWhere canSelfMerge (pointAt g x) g

export
mergeGraphs' : CoreDims => {k,m:_} -> CDIGraph k -> CDIGraph m -> CDGraph
mergeGraphs' g t =
  let lMergeN     := nodesToMerge g t
      offset      := offset g t lMergeN
      lnewBonds   := newEdges t lMergeN
      mol'        := mergeGraphsWithEdges g (translate offset t) lnewBonds
   in delNodes (mapMaybe (incNode k . snd) lMergeN) mol'

-- This connects a template to a graph by connecting the template's
-- zero node via a single bond with the given node of the current molecule.
-- The template is rotated and translated in such a way that we get
-- preferrable bond angles both at the current graph and the template.
mergeGraphsOnAtom : CoreDims => {k,m : _} -> Fin k -> CDIGraph k -> CDIGraph m -> CDGraph
mergeGraphsOnAtom {m = 0}   _ g _ = G _ g
mergeGraphsOnAtom {m = S _} n g t =
  case bondAngles g n of
    [an] =>
      let a0     := preferredAngle False (bondAngles t 0)
          tr     := rotate (an - a0) t
          offset := point (lab g n) - point (lab tr 0)
       in mergeGraphs' g (translate offset tr)
    as   =>
      let an     := preferredAngle False as
          a0     := preferredAngle False (bondAngles t 0)
          tr     := rotate (an - a0 + pi) t
          offset := point (lab g n) - point (lab tr 0)
          v      := polar BondLengthInAngstrom an + offset
          bond   := CB New $ cast Single
       in G _ $ mergeGraphsWithEdges g (translate v tr) [(n,0,bond)]

-- This connects a template to a graph by replacing the template's
-- smallest edge given edge of the current molecule.
-- The template is rotated and translated in such a way that the two
-- edges are properly aligned.
--
-- There are two ways to align the bonds of template and
-- molecule, so we try both and keep the one with its
-- center closer to the mouse position. This allows us to
-- flip between the two placements by moving the mouse
-- from one side of a bond to the other.
mergeGraphsOnBond :
     {k,m : _}
  -> {auto cd : CoreDims}
  -> Point Mol
  -> Edge k CDBond
  -> CDIGraph k
  -> CDIGraph m
  -> CDGraph
mergeGraphsOnBond p (E n1 n2 _) g t =
  case edges t of
    []      => G k g
    E n3 n4 _ :: _ =>
      let Just ag := angle (pointAt g n1 - pointAt g n2) | Nothing => G k g
          Just at := angle (pointAt t n3 - pointAt t n4) | Nothing => G k g
          tr1     := rotate (ag - at) t
          tt1     := translate (point (lab g n1) - point (lab tr1 n3)) tr1
          tr2     := rotate (ag - at + pi) t
          tt2     := translate (point (lab g n1) - point (lab tr2 n4)) tr2
       in if distance p (center tt1) <= distance p (center tt2)
             then mergeGraphs' g tt1
             else mergeGraphs' g tt2

||| Add the template to the existing graph depending on clicking on an atom,
||| a bond or elsewhere on the canvas.
export
mergeGraphs : CoreDims => Point Id -> (g, t : CDGraph) -> CDGraph
mergeGraphs c (G o1 g) (G o2 t) =
  case hoveredItem g of
    N k  => mergeGraphsOnAtom (fst k) g t
    E e  => mergeGraphsOnBond (convert c) e g t
    None => mergeGraphs' g t

||| Attaches an atom to a mol graph.
|||
||| Depending on the current mouse position and whether "Shift" is
||| pressed or not, the drawing tool suggest a different bond length
||| and angle for the new bond.
export
addBondE :
     {k : _}
  -> {t : _}
  -> {auto cd   : CoreDims}
  -> (shiftDown : Bool)
  -> (mousePos  : Point t)
  -> (newBond   : MolBond)
  -> CDIGraph k
  -> Either (CDIGraph k) (CDIGraph $ S k)
addBondE @{cd} shiftDown p mb g =
  let b          := CB New mb -- new `CDBond`
      Just (n,a) := find (is Origin . snd) (labNodes g) | Nothing => Left g
      pc         := pointId p -- current mouse position
      pa         := pointId a -- position of atom we attach new bond to
   in if near pa pc cd.radiusAtom
         -- if mouse is close to origin atom
         -- use largest bisector as new bond angle and draw a bond
         -- of preferred length
         then bondTo b n pc (bestPos g mb n pa) g
  
         -- else use an angle close to the one of the vector connecting
         -- the origin atom and the mouse pointer, unless "Shift" is down,
         -- in which case we use the mouse position without modification
         else bondTo b n pc (suggestedPos shiftDown pa pc) g

||| Attaches an atom to a mol graph.
|||
||| Depending on the current mouse position and whether "Shift" is
||| pressed or not, the drawing tool suggest a different bond length
||| and angle for the new bond.
export
addBond :
     {k : _}
  -> {t : _}
  -> {auto cd   : CoreDims}
  -> (shiftDown : Bool)
  -> (mousePos  : Point t)
  -> (newBond   : MolBond)
  -> CDIGraph k
  -> CDGraph
addBond sd p mb g = either (G k) (G $ S k) (addBondE sd p mb g)

||| Adds an uncharged atom of the given isotope at the given position.
|||
||| This either replaces the currently hovered atom, or it inserts a new
||| isolate atom.
export
addAtom :
     {auto cd : CoreDims}
  -> {t : _}
  -> {k : _}
  -> CDIGraph k
  -> Isotope
  -> Point t
  -> CDGraph
addAtom g i p =
  case hoveredItem g of
    N (x,_) => 
      let g2 := setNode x (isotopeAt i (pointAt g x) New) g
       in delNodes (groupNodes g [x]) g2
    _       => case closestNode (convert p) g of
      Nothing => G _ $ insIsotopeAt g i (convert p) New

      -- in the `Just` case, we have a node close to the mouse pointer
      -- that is not set to "hovered". This means, replacing that node is
      -- a non-op. We don't want to insert another atom on top of it, so
      -- we return the graph unmodified.
      Just _  => G _ g

export
setAbbreviationAt :
     {auto cd : CoreDims}
  -> {k,m        : Nat}
  -> (lbl        : String)
  -> (node,neigh : Fin k)
  -> (abbr       : CDIGraph (S m))
  -> (mol        : CDIGraph k)
  -> CDGraph
setAbbreviationAt lbl n1 n2 a g =
  let nr      := S (maxGroupNr g)
      pn      := point (lab g n1) -- position of label
      pc      := point (lab g n2) -- node to which label is connected
      Just an := angle (pn - pc) | Nothing => G k g
      a0      := preferredAngle False (bondAngles a 0)
      ar      := rotate (an-a0+pi) (setGroup (G nr lbl) <$> a)
      at      := translate (pn - point (lab ar 0)) ar
      bond    := CB New $ maybe (cast Single) molBond (elab g n1 n2)
      gm      := mergeGraphsWithEdges g at [(n2,0,bond)]
   in delNodes (plusGroupNodes gm [weakenN _ n1]) gm

||| Replaces the atom or abbreviation at the given position with
||| an abbreviation.
export
setAbbreviation :
     {auto cd : CoreDims}
  -> (shiftDown : Bool)
  -> (lbl       : String)
  -> (mouse     : Point Id)
  -> (abbr      : CDGraph)
  -> (mol       : CDGraph)
  -> CDGraph
setAbbreviation _  lbl mouse (G 0 _) g = g
setAbbreviation sd lbl mouse (G (S m) a) (G k g) =
  case find (is Origin . snd) (labNodes g) of
    Nothing     => G k g
    Just (n1,_) => case visibleNeighbours g n1 of
      [n2] => setAbbreviationAt lbl n1 n2 a g
      _    => case addBondE sd mouse (cast Single) g of
        Left _   => G k g
        Right g2 => setAbbreviationAt lbl last (weaken n1) a g2

||| Expands the currently hovered-over abbreviation (if any)
export
expand : CDGraph -> CDGraph
expand (G o g) =
  let N (n1,CA _ a) := hoveredItem g | _ => G o g
      Just (G n _)  := label a       | _ => G o g
   in G o $ map (clearGroup n) g

%inline new : CDBond -> CDBond
new = {role := New}

%inline translateTemplateAtom : Vector (transform Mol) -> CDAtom -> CDAtom
translateTemplateAtom v (CA _ a) = CA None $ translate v a

||| Merges a template graph with the current mol graph based on the
||| current mouse position.
export
addTemplate : CoreDims => {s : _} -> Point s -> (t, mol : CDGraph) -> CDGraph
addTemplate p t mol =
  let v := convert p - center t
   in mergeGraphs (convert p) mol (bimap new (translateTemplateAtom v) t)

||| Remove the abbreviation labels from orphaned abbreviation atoms.
||| The list of nodes have had an edge remove and now belong to potentially
||| orphaned abbreviation groups.
export
clearOrphanGroups : {k : _} -> List (Fin k) -> CDIGraph k -> CDIGraph k 
clearOrphanGroups ns g =
  let gs@(_::_) := ns >>= toList . groupNr g | Nil => g
   in map (\x => foldl (flip clearGroup) x gs) g

||| Deletes all selected nodes or edge from the graph
||| (including nodes selected transitively via abbreviations).
|||
||| When we delete an edge, we are at risk of creating an orphaned
||| abbreviation: An invisible set of abbreviated nodes no longer
||| connected to the visible part of the molecule. In such a case,
||| we could either delete the whole abbreviation, or make the
||| orphaned nodes visible. Here, we opt for the latter. If users
||| want to delete the whole abbreviation, they can do so by
||| deleting the atom in question.
export
deleteSelected : CDGraph -> CDGraph
deleteSelected (G o g) =
  case selectedItems g of
    None => G o g
    N ns => delNodes (plusGroupNodes g ns) g
    E es =>
      let ns := es >>= \(x,y) => [x,y]
       in G o $ clearOrphanGroups ns (delEdges es g)

mapNodeIf : {k : _} -> (Fin k -> Bool) -> (n -> n) -> IGraph k e n -> IGraph k e n
mapNodeIf p f = mapWithCtxt (\n,a => if p n then f a.label else a.label) 

groupSelected : CDIGraph k -> List Nat -> Fin k -> Bool
groupSelected g ns n =
  let (CA _ a) := lab g n
   in isSelected g True n || any ((`elem` ns) . nr) a.label

||| Translates the selected atoms in a molecule by a vector given
||| as a start and end point.
export
moveSelected : CoreDims => (start, end : Point Mol) -> CDGraph -> CDGraph
moveSelected start end (G o g) =
  let gs := selectedNodes g True >>= toList . groupNr g
   in mergeCloseNodes $ mapNodeIf (groupSelected g gs) (translate $ end - start) g

export
rotateTempl :
     {auto cd : CoreDims}
  -> Bool
  -> (start, end : Point Mol)
  -> CDGraph
  -> CDGraph
rotateTempl cont start end g =
  let Just d := angle (end   - start) | Nothing => g
      phi    := fromMaybe d (closestAngle d $ if cont then [] else stepAngles)
   in rotateAt start phi g

||| Rotates the selected atoms around the center of the selection
||| by an angle defined by the two points.
|||
||| If the `Bool` argument is set to `False`, we use step-wise rotation
||| as defined in the `CoreDims` argument, otherwise, we use 
||| continuous rotation.
export
rotateSelected :
     {auto cd : CoreDims}
  -> Bool
  -> (start, end : Point Mol)
  -> CDGraph
  -> CDGraph
rotateSelected cont start end (G o g) =
  let ns := selectedNodes g True
      c  := center $ foldMap (bounds . lab g) ns
      Just ae := angle (end   - c) | Nothing => G o g
      Just as := angle (start - c) | Nothing => G o g
      d       := ae - as
      phi     := fromMaybe d (closestAngle d $ if cont then [] else stepAngles)
      gs := ns >>= toList . groupNr g
   in mergeCloseNodes $ mapNodeIf (groupSelected g gs) (rotateAt c phi) g

||| Extracts the selected subgraph (or the whole graph, if no atoms are
||| selected) from a drawing graph.
export
selectedSubgraph : (includeEmptySelection : Bool) -> CDGraph -> CDGraph
selectedSubgraph b (G o g) =
  case plusGroupNodes g (selectedNodes g False) of
    [] => if b then G o g else G 0 empty
    ns => snd <$> subgraphL g ns
