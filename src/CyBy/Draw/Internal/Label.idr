module CyBy.Draw.Internal.Label

import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.CoreDims
import CyBy.Draw.Internal.Graph
import Data.Finite
import Geom
import Text.Measure
import Text.Molfile

%default total

-- Radius of (possibly) colored background circles around atom labels
bgRadiusFactor : Double
bgRadiusFactor = 0.7 -- 0.5 * sqrt 2 

export %inline
lineEndRadius : Double
lineEndRadius = 0.5

--------------------------------------------------------------------------------
--          Implicit Hydrogen position
--------------------------------------------------------------------------------

||| Position of implicit hydrogen label.
||| These are placed in such a way that they interfere as little as
||| possible with the bonds leading to an atom's neighbours
public export
data HPos = NoH | N | W | S | E

--------------------------------------------------------------------------------
--          Measured Text Labels
--------------------------------------------------------------------------------

||| A text label together with the text metrics we need to properly
||| center it around its position `pos`
public export
record Text a where
  constructor T
  fsize   : Nat
  text    : String
  pos     : a      -- Position (center) of the text label
  dims    : TextDims

export
(.h) : Text a -> Double
t.h = t.dims.capHeight

export
(.lh) : Text a -> Double
t.lh = t.dims.lineHeight

export
(.w) : Text a -> Double
t.w = t.dims.txtWidth

public export
Bounded (Text $ Point Id) where
  btrans = Id
  bounds (T _ "" _ _) = neutral
  bounds (T _ _ (P x y) (TD _ cs w)) =
    BS (range (x-w/2) (x+w/2)) (range (y-cs/2) (y+cs/2))

||| The empty text label
export
noLbl : Text ()
noLbl = T 0 "" () (TD 0 0 0)

||| Returns a `Label` for a string together with its `TextDims`
export
text : (cd : CoreDims) => (sub : Bool) -> (text : String) -> Text ()
text _   "" = noLbl
text sub s  =
  let fs := if sub then cd.subscriptSize else cd.fontSize
   in T fs s () $ cd.measure.measure fs cd.font s

||| Computes the radius of the background circle of a text label.
export
radius : Text a -> Maybe Double
radius (T _ "" _ _)           = Nothing
radius (T _ _  _ $ TD _ cs w) = Just $ max cs w * bgRadiusFactor

trans : Vector Id -> Double -> Maybe Double -> Point Id -> Point Id
trans v l Nothing  x = x
trans v l (Just r) x = translate (scale (r / l) v) x

||| Adjusts the end point of an edge based on the radius of
||| their background labels.
export
adjEndPoints :
     (x,y : Point Id)
  -> (rx,ry : Maybe Double)
  -> Maybe (Point Id, Point Id)
adjEndPoints x y rx ry =
  let v    := x - y
      lv   := length v
      True := lv > fromMaybe 0 rx + fromMaybe 0 ry | False => Nothing
   in Just (trans (negate v) lv rx x, trans v lv ry y)

||| Position where the text label should be placed in the canvas
||| to correctly center it around the point given in its `pos` field
export
(.textPos) : Text (Point Id) -> Point Id
l.textPos =
  let P x y := l.pos
   in P x (y + l.dims.capHeight / 2.0)

||| Text labels for an atom's symbol, charge, mass number,
||| implicit hydrogen, and implicit hydrogen count
public export
record AtomLabels a where
  constructor AL
  symbol   : Text a
  charge   : Text a
  mass     : Text a
  hydrogen : Text a
  hcount   : Text a

export
labels : AtomLabels a -> List (Text a)
labels (AL v w x y z) = [v,w,x,y,z]

public export
Bounded (AtomLabels $ Point Id) where
  btrans = Id
  bounds = foldMap bounds . labels

export
chargeLabel : Charge -> String
chargeLabel 0    = ""
chargeLabel 1    = "+"
chargeLabel (-1) = "-"
chargeLabel n    =
  if n > 0 then "\{show n.value}+" else "\{show $ abs n.value}-"

export
massLabel : Maybe MassNr -> String
massLabel = maybe "" (show . value)

export
hlabel : HCount -> String
hlabel 0 = ""
hlabel _ = "H"

export
hsubscript : HCount -> String
hsubscript h = if h > 1 then show h.value else ""

||| Compute the exact positions of all parts of an atom's labels based
||| on the determined position of the hydrogen label (`HPos`), the position
||| of the atom in the molecule, and the metrics of all labels we want
||| to display.
export
setPositions : HPos -> Point Id -> AtomLabels () -> AtomLabels (Point Id)
setPositions x p (AL s c m h hc) =
  let -- displacement of the "H" label (if any)
      vh  := case x of
               N => case c.h > 0.0 && hc.h > 0.0 of
                 True  => vid 0 (negate $ s.h + (c.h + hc.h) / 2.0)
                 False => vid 0 (negate $ s.h + s.lh)
               -- put H below atom label
               S => vid 0 (h.h + h.lh)

               -- put H left of atom label making space for hydrogen count
               -- and mass number
               W => vid (negate $ (s.w + h.w) / 2 + max hc.w m.w) 0

               -- put H to the right of atom label
               -- charge will come after H label.
               _ => vid ((s.w + h.w) / 2) 0

      -- displacement of the mass number label (if any)
      vm  := vid (negate $ (s.w + m.w) / 2) (m.h / 2 - s.h)

      -- displacement of the charge label (if any)
      vc  := case x of
               E => vid ((s.w + c.w) / 2 + h.w) (c.h / 2 - s.h)
               _ => vid ((s.w + c.w) / 2) (c.h / 2 - s.h)

      -- displacement of the H-count label (in case of more than 1 impl. H)
      vhc := vid ((h.w + hc.w) / 2) (h.h / 2.0)

      -- Text (Point Id) for the atom symbol
      sym := {pos := p} s

      -- Text (Point Id) for the "H" label
      -- We must make sure this is at same textual height as the atom
      -- symbol if we place it left or right of the atom symbol
      hyd := {pos := translate vh p} h

   in AL
        { symbol   = sym
        , charge   = {pos := translate vc p} c
        , mass     = {pos := translate vm p} m
        , hydrogen = hyd
        , hcount   = {pos := translate vhc hyd.pos} hc
        }

--------------------------------------------------------------------------------
-- Abbreviations
--------------------------------------------------------------------------------

public export
data AbbrPos = AE | AW

export
abbrTextPos : CoreDims => AbbrPos -> Point Id -> Text () -> Text (Point Id)
abbrTextPos @{cd} AE (P x y) t =
  case t.w < 2*cd.radiusAtom of
    True  => {pos := P x y} t
    False => {pos := P (x + t.w / 2.0 - cd.radiusAtom) y} t
abbrTextPos @{cd} AW (P x y) t =
  case t.w < 2*cd.radiusAtom of
    True  => {pos := P x y} t
    False => {pos := P (x - t.w / 2.0 + cd.radiusAtom) y} t

--------------------------------------------------------------------------------
-- Updating Labels
--------------------------------------------------------------------------------

firstAfter : Eq a => a -> List a -> a
firstAfter v vs =
  case break (v ==) vs of
    (_, _::x::_) => x
    (x::_, [_])  => x
    (x::_, [])   => x
    (_, _)       => v

getListElems : String -> List Elem
getListElems s = filter (isPrefixOf s . show) values

||| Based on a string input (currently, a single character) and the current
||| element, selects the next element from all elements the symbol of which
||| starts with the input character.
|||
||| This allows us to use keyboard shortcuts to change the element of the
||| atom over which we currently hover.
export
updateElem : String -> Elem -> Elem
updateElem s e = firstAfter e $ sortBy (comparing show) (getListElems s)

||| Uses `updateElem` to change the element of an isotope.
export
updateIsotope : String -> Isotope -> Isotope
updateIsotope s i = cast $ updateElem s i.elem

export
masses : Elem -> List (Maybe MassNr)
masses e = Nothing :: sort (map (Just . massNr) $ isotopes e)

isoList : Elem -> List Isotope
isoList el = MkI el <$> masses el

export
incIso : Isotope -> Isotope
incIso i = firstAfter i (isoList i.elem)

export
decIso : Isotope -> Isotope
decIso i = firstAfter i (reverse $ isoList i.elem)

--------------------------------------------------------------------------------
--          Arbitrary Atomlabels
--------------------------------------------------------------------------------

public export
data Label : Type where
  Hidden       : Label
  NoLabel      : Point Id -> Label
  Abbreviation : Point Id -> Text (Point Id) -> Label
  Explicit     : AtomLabels (Point Id) -> Label

public export
0 Labels : Nat -> Type
Labels k = IArray k Label

circleBounds : (cd : CoreDims) => Point Id -> Bounds2D Id
circleBounds (P x y) =
  let r := cd.radiusAtom in BS (range (x-r) (x+r)) (range (y-r)(y+r))

public export
(cd : CoreDims) => Bounded Label where
  btrans = Id
  bounds Hidden             = neutral
  bounds (NoLabel p)        = circleBounds p
  bounds (Explicit x)       = bounds x
  bounds (Abbreviation _ x) = bounds x

--------------------------------------------------------------------------------
--          Bond End Points
--------------------------------------------------------------------------------

||| Given a starting point and a vector, tries to find the
||| first intersection of the resulting line segment with a
||| circle given by its center and radius.
export
trimToCircle :
     Point t
  -> Vector (transform t)
  -> (c : Point t)
  -> (r : Double)
  -> Double
trimToCircle (P x y) (V vx vy) (P cx cy) r =
  let dx   := x - cx
      dy   := y - cy
   in case solveQuadratic (vx*vx+vy*vy) (2*(dx*vx+dy*vy)) (dx*dx+dy*dy-r*r) of
        Nothing      => 1.0
        Just (s1,s2) => if s1 < 0 then if s2 >= 0 then 0 else 1 else min s1 1

textFactor : Point Id -> Vector Id -> Text (Point Id) -> Double
textFactor p v t =
  case radius t of
    Nothing => 1
    Just r  => trimToCircle p v t.pos (r + lineEndRadius)

abbrFactor : CoreDims => Point Id -> Vector Id -> Point Id -> Double
abbrFactor @{cd} p v q = trimToCircle p v q (cd.radiusAtom + 4 * lineEndRadius)

factor : CoreDims => Point Id -> Vector Id -> Label -> Double
factor p v Hidden             = 0
factor p v (NoLabel _)        = 1
factor p v (Abbreviation q _) = abbrFactor p v q
factor p v (Explicit ls)      = foldl min 1 (textFactor p v <$> labels ls)

||| Computes the end points of a bond based on the atom positions and
||| atom labels so that the bonds do not overlap with the labels.
export
endpoints :
     {auto _: CoreDims}
  -> (x,y : Point Id)
  -> (lx,ly : Label)
  -> Maybe (Point Id, Point Id)
endpoints x y (NoLabel _) (NoLabel _) = Just (x,y)
endpoints x y Hidden      _           = Nothing
endpoints x y _           Hidden      = Nothing
endpoints x y lx          ly          =
  let vx := y - x
      vy := x - y
      fx := factor x vx ly
      fy := factor y vy lx
   in if fx + fy <= 1
         then Nothing
         else Just ( translate (scale (1-fy) vx) x
                   , translate (scale (1-fx) vy) y
                   )

--------------------------------------------------------------------------------
--          Label Positions
--------------------------------------------------------------------------------

||| Relative position, where the label of an abbreviated group will be
||| placed.
export
abbrPos : CDIGraph k -> Fin k -> AbbrPos
abbrPos g x =
  let [a] := bondAngles g x | _ => AE
   in if a <= halfPi || a >= (negate halfPi) then AW else AE

||| Determines the position of the "H"-label depending on the angles of
||| bonds leading to neighbours
export
bestHPos : List Geom.Angle.Angle -> HPos
bestHPos xs =
  if      all (\x => x >= halfPi && x <= threeHalfPi) xs then E
  else if all (\x => x <= halfPi || x >= threeHalfPi) xs then W
  else if all (\x => x <= pi) xs then N
  else if all (\x => x >= pi) xs then S
  else E -- catch-all pattern for very crowded atoms

||| Determines the position of the "H" label (if any)
||| relative to an atom's symbol. To do this, this computes the angles
||| of all bonds leading to an atom's neighbours and tries to find
||| a direction (north, west, south, or east) without any neighbouring bonds
export
hpos : CDIGraph k -> Fin k -> HPos
hpos g x =
  case Atom.hydrogen . atom $ lab g x of
    0 => NoH
    _ => bestHPos (bondAngles g x)
