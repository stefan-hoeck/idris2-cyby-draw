||| Drawing Utilities
module CyBy.Draw.Draw

import CyBy.Draw.Internal.Abbreviations
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.CoreDims
import CyBy.Draw.Internal.DoubleBond
import CyBy.Draw.Internal.Color
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Label
import CyBy.Draw.Internal.Role
import CyBy.Draw.Internal.Settings
import CyBy.Draw.Internal.Wedge
import Geom
import Text.Molfile
import Text.SVG
import Text.SVG.Attribute as A

%default total

--------------------------------------------------------------------------------
--          Basic Shapes
--------------------------------------------------------------------------------

export
fillCircle : SVGColor -> Point Id -> Double -> SVGNode
fillCircle c (P x y) rv = circle [cx x.u, cy y.u, r rv.u, fill c, stroke none]

export
roundedRect :
     {auto cd : CoreDims}
  -> (p1,p2 : Point Id)
  -> List (SVGAttribute "rect")
  -> SVGNode
roundedRect (P a b) (P d e) as =
  rect $
       x (min a d).u
    :: y (min b e).u
    :: rx cd.radiusAtom.u
    :: ry cd.radiusAtom.u
    :: width (abs $ a - d).u
    :: height (abs $ b - e).u
    :: as

export
fillRect : (cd : CoreDims) => SVGColor -> (p1,p2 : Point Id) -> SVGNode
fillRect c x y = roundedRect x y [fill c, stroke c]

export
outlineRect : (cd : CoreDims) => SVGColor -> (p1,p2 : Point Id) -> SVGNode
outlineRect c x y = roundedRect x y [fill none, stroke c]

export
outlineRectD : (cd : CoreDims) => SVGColor -> (p1,p2 : Point Id) -> SVGNode
outlineRectD c x y =
  roundedRect x y [fill none, stroke c, strokeDasharray [5,5]]

export
singleLine : SVGColor -> Point Id -> Point Id -> SVGNode
singleLine c (P x1 y1) (P x2 y2) = path [d [M x1 y1, L x2 y2], stroke c]

export
text : (cd : CoreDims) => Text (Point Id) -> SVGNode
text (T _ "" _ _) = Empty
text l =
  let P a b := l.pos
      P x y := l.textPos
      -- disable pointer-events, because we do not want text to be
      -- selectable, nor do we want a different mouse pointer when
      -- over text nodes.
      as1   := [A.x x.u, A.y y.u, Style "pointer-events" "none"]
      as2   := if l.fsize == cd.fontSize then as1 else (fontSize (cast l.fsize).px :: as1)
   in text1 as2 l.text

--------------------------------------------------------------------------------
--          Collecting Shapes
--------------------------------------------------------------------------------

||| We group the different layers of the drawing - carbon skeleton, atom labels
||| background highlights - in lists of nodes wrapped by a `<g>` element
||| listing the key properties of the group.
|||
||| Snoc lists are the natural choice for assembling these groups of
||| nodes from head to tail.
public export
record Nodes where
  constructor NS
  ||| The skeleton of the molecule: All bonds collected in a single `<path>`
  ||| element except upward bonds, which are polygons rather than lines.
  skeleton : SnocList PathCmd

  ||| Background shapes mainly use for selected or otherwise highlighted
  ||| atoms and bonds.
  bgShapes : SnocList SVGNode

  ||| Upward wedges
  wedges   : SnocList SVGNode

  ||| All text labels (including charges, implici hydrogens,
  ||| mass numbers, and abbreviations)
  txtLbls : SnocList SVGNode

export
init : Nodes
init = NS [<] [<] [<] [<]

public export
0 TNodes : Type
TNodes = Nodes -> Nodes

pathAttrs : SVGColor -> (lw : Double) -> List (SVGAttribute "g")
pathAttrs c lw =
  [stroke c, strokeWidth lw.u, fill none, strokeLinecap Round]

-- shapes have fill and stroke (to allow for rounded corners)
shapeAttrs : SVGColor -> (lw : Double) -> List (SVGAttribute "g")
shapeAttrs c w =
  [stroke c, fill c, strokeWidth w.u, strokeLinecap Round, strokeLinejoin Round]

fontAttrs : (ds : DrawSettings) => List (SVGAttribute "g")
fontAttrs =
  [ stroke none
  , fill ds.textColor
  , fontFamily ds.core.font
  , fontSize (cast ds.core.fontSize).px
  , textAnchor Middle
  ]

group : List (SVGAttribute "g") -> List SVGNode -> SVGNode
group as [] = Empty
group as ns = g as ns

export
toNodes : (ds : DrawSettings) => TNodes -> List SVGNode
toNodes f =
  let (NS fgp bgs fgs lbls) := f init
   in [ group (shapeAttrs ds.selectBG ds.core.bondBGWidth) (bgs <>> [])
      , group (pathAttrs ds.bondColor ds.core.bondWidth) [path [d (fgp <>> [])]]
      , group (shapeAttrs ds.bondColor ds.core.bondWidth) (fgs <>> [])
      , group (fontAttrs) (lbls <>> [])
      ]

--------------------------------------------------------------------------------
--          Basic Shapes
--------------------------------------------------------------------------------

addToBG : SVGNode -> TNodes
addToBG n = {bgShapes $= (:< n)}

addLbl : SVGNode -> TNodes
addLbl n = {txtLbls $= (:< n)}

addCircle : SVGColor -> Point Id -> Double -> TNodes
addCircle c p rv = addToBG (fillCircle c p rv)

line : Point Id -> Point Id -> TNodes
line (P x1 y1) (P x2 y2) = {skeleton $= (:< M x1 y1 :< L x2 y2)}

lineBG : Maybe SVGColor -> Point Id -> Point Id -> TNodes
lineBG Nothing  _ _ ns = ns
lineBG (Just c) x y ns = addToBG (singleLine c x y) ns

wedgeDown : CoreDims => Point Id -> Point Id -> TNodes
wedgeDown p1 p2 = {skeleton $= (<>< Wedge.wedgeDown p1 p2)}

wedgeUp : CoreDims => Point Id -> Point Id -> TNodes
wedgeUp p1 p2 = {wedges $= (:< Wedge.wedgeUp p1 p2 [])}

wedgeBG : CoreDims => Maybe SVGColor -> Point Id -> Point Id -> TNodes
wedgeBG Nothing  _  _  ns = ns
wedgeBG (Just c) p1 p2 ns =
  addToBG (Wedge.wedgeUp p1 p2 [fill c, stroke c]) ns

wave : CoreDims => Point Id -> Point Id -> TNodes
wave p1 p2 = {skeleton $= (<>< Wedge.wave p1 p2)}

waveBG : CoreDims => Maybe SVGColor -> Point Id -> Point Id -> TNodes
waveBG Nothing  _  _  ns = ns
waveBG (Just c) p1 p2 ns =
  addToBG (path [d (Wedge.wave p1 p2), stroke c, fill none]) ns

atmLabels : CoreDims => SVGColor -> AtomLabels (Point Id) -> TNodes
atmLabels c ls = addLbl (group [fill c] (text <$> labels ls))

labelBG : SVGColor -> Text (Point Id) -> TNodes
labelBG c l =
  case radius l of
    Nothing => id
    Just r  => addCircle c l.pos r

abbrBG : CoreDims => SVGColor -> Text (Point Id) -> TNodes
abbrBG c t ns =
  let Just (p1,p2) := corners (bounds t) | Nothing => ns
   in addToBG (fillRect c p1 p2) ns

--------------------------------------------------------------------------------
--          Colors
--------------------------------------------------------------------------------

-- color to use as the background for bonds and atoms
background :
     {0 a : Type}
  -> {auto cst : Cast a Role}
  -> {auto s   : DrawSettings}
  -> (dflt     : Maybe SVGColor)
  -> a
  -> Maybe SVGColor
background deflt v =
  if      is New v then Just s.newBG
  else if is Origin v then Just s.originBG
  else if is Hover v then Just s.hoverBG
  else if is Selected v then Just s.selectBG
  else if is Highlight v then Just s.highlightBG
  else deflt

--------------------------------------------------------------------------------
--          Drawing Molecules
--------------------------------------------------------------------------------

parameters {auto s : DrawSettings}
           {k      : _}
           (g      : CDIGraph k)

  abbrText : AbbrPos -> String -> String
  abbrText AE t = t
  abbrText AW t = reverseLabel t s.abbreviations
  
  export
  label : Fin k -> Label
  label x =
    case visible g x of
      False => Hidden
      True  => case labelVisible s.showC g x of
        False => NoLabel (pointAt g x)
        True  => case group (lab g x) of
          Just (G _ a) =>
            let ap := abbrPos g x
                p  := pointAt g x
             in Abbreviation p $ abbrTextPos ap p (text False $ abbrText ap a)
          Nothing         =>
            let atm := atom $ lab g x
                sym := text False (symbol atm.elem.elem)
                ch  := text True (chargeLabel atm.charge)
                mn  := text True (massLabel atm.elem.mass)
                hl  := text False (hlabel atm.hydrogen)
                hc  := text True (hsubscript atm.hydrogen)
             in Explicit $ setPositions (hpos g x) (pointId atm) (AL sym ch mn hl hc)

  export
  labels : Labels k
  labels = generate k label

  atomBG : Labels k -> Fin k -> TNodes
  atomBG ls x ns =
    let atm  := lab g x
        dflt := if isInvalid atm.atom.type then Just s.errorBG else Nothing
     in case background dflt atm of
          Nothing => ns
          Just c  => case at ls x of
            Abbreviation _ abbr => abbrBG c abbr ns
            Explicit l          => foldl (flip $ labelBG c) ns (labels l)
            NoLabel p           => addCircle c p s.core.radiusAtom ns
            Hidden              => ns

  drawAtom : Labels k -> Nodes -> Fin k -> Nodes
  drawAtom ls ns x =
    let ns2 := atomBG ls x ns
     in case at ls x of
          Abbreviation _ abbr => addLbl (text abbr) ns2
          Explicit l => atmLabels (s.elemColor (cast $ lab g x)) l ns2
          NoLabel p  => ns2
          Hidden     => ns2

  snglBond : Maybe SVGColor -> Point Id -> Point Id -> BondStereo -> TNodes
  snglBond c x y Up       ns = wedgeUp x y $ wedgeBG c x y ns
  snglBond c x y UpOrDown ns = wave x y $ waveBG c x y ns
  snglBond c x y Down     ns = wedgeDown x y $ wedgeBG c x y ns
  snglBond c x y _        ns = line x y $ lineBG c x y ns

  dblBond : Maybe SVGColor -> Fin k -> Fin k -> Point Id -> Point Id -> TNodes
  dblBond c x y px py ns =
    let [a,b,d,e] := dblBond g x y (pointAt g x) (pointAt g y) px py
     in line a b . line d e . lineBG c a b $ lineBG c d e ns

  trplBond : Maybe SVGColor -> Point Id -> Point Id -> TNodes
  trplBond c x y ns =
    let r     := 0.8 * s.core.radiusAtom
        (a,b) := parallelLine r True x y
        (d,e) := parallelLine r False x y
     in line x y . line a b . line d e .
        lineBG c x y . lineBG c a b $ lineBG c d e ns

  addBond :
       MolBond
    -> Maybe SVGColor
    -> (x,y : Fin k)
    -> (px,py : Point Id)
    -> TNodes
  addBond (MkBond True  Single st) c x y px py = snglBond c px py st
  addBond (MkBond False Single st) c x y px py = snglBond c py px st
  addBond (MkBond _     Dbl    _ ) c x y px py = dblBond c x y px py
  addBond (MkBond _     Triple _ ) c x y px py = trplBond c px py

  drawBond : Labels k -> Nodes -> Edge k CDBond -> Nodes
  drawBond ls ns (E x y b) =
    let c  := background Nothing b
        px := pointAt g x
        py := pointAt g y
        lx := at ls x
        ly := at ls y
        Just (qx,qy) := endpoints px py lx ly | Nothing => ns
     in addBond b.molBond c x y qx qy ns

export
drawMolecule : DrawSettings => CDGraph -> List SVGNode
drawMolecule (G o g) = toNodes (scene $ labels g)
  where
    scene : Labels o -> TNodes
    scene ls ns =
      foldl (drawAtom g ls) (foldl (drawBond g ls) ns (edges g)) (nodes g)

--------------------------------------------------------------------------------
--          Drawing Utilities
--------------------------------------------------------------------------------

export
rotateTemplScene : DrawSettings => Point Mol -> Point Mol -> List SVGNode
rotateTemplScene @{ds} o m =
  [ fillCircle ds.hoverBG (convert o) ds.core.radiusAtom
  , singleLine ds.hoverBG (convert o) (convert m)
  ]
