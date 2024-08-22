||| This module containes the logic for properly placing double bonds, especiall
||| the pi-part of those bonds. We use a simple heuristic to place the pi-bond:
|||
||| In case of a double bond with three substituents, the pi bond will be placed
||| on the same side of the sigma bond as the lone substituent.
|||
||| In case of a double bond with four substituents, the dominant cycle
||| (if any; see below) will be determined, and the pi-bond will be placed
||| within that cycle.
|||
||| In all other cases, a symmetric double with both lines being displaced
||| from the center by the same amount of space, will be displayed.
|||
||| In order to define the dominant cycle to which a double bond belongs,
||| rings will be sorted by number of multiple bonds and then by ring size.
||| Six membered rings will always be preferred over other ring sizes.
module CyBy.Draw.Internal.DoubleBond

import Derive.Prelude
import Data.Graph.Indexed.Query.BFS
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.CoreDims
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Label
import Geom
import Text.Molfile

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Rings
--------------------------------------------------------------------------------

-- A data type for sorting rings by number of double- and triple bonds and by
-- size, so that six-membered rings will always be preferred.
data RingSize : Type where
  NoRing : RingSize
  Ring   : (multibonds, size : Nat) -> RingSize
  Hexane : (multibonds : Nat) -> RingSize

%runElab derive "RingSize" [Eq,Ord]

--------------------------------------------------------------------------------
-- Geometry
--------------------------------------------------------------------------------

export
parallelLine :
     (r   : Double)
  -> (pos : Bool)
  -> (x,y : Point Id)
  -> (Point Id, Point Id)
parallelLine r b x y =
  (perpendicularPoint x y r b, perpendicularPoint y x r $ not b)


||| Distance between the lines of a double or triple bond.
export %inline
ParallelDistance : (cd : CoreDims) => Double
ParallelDistance = 0.8 * cd.radiusAtom

||| Half the distance between the lines of a double or triple bond.
export %inline
HalfParallelDistance : (cd : CoreDims) => Double
HalfParallelDistance = 0.5 * ParallelDistance

-- `ox` and `oy` are the original positions of the atoms,
-- while `px` and `py` are the visible bond endings.
parameters {auto cd     : CoreDims}
           {k           : Nat}
           (g           : CDIGraph k)
           (x,y         : Fin k)
           (ox,oy,px,py : Point Id)

  countMBs : Nat -> List (Fin k) -> Nat
  countMBs n (a::t@(b::_)) =
    case (type . molBond) <$> elab g a b of
      Just Dbl    => countMBs (S n) t
      Just Triple => countMBs (S n) t
      _           => countMBs n t
  countMBs n _             = n

  ringSize : (nx,ny : Fin k) -> RingSize
  ringSize nx ny =
    case limitedBfs g [x,y] nx ny of
      Nothing => NoRing
      Just sn =>
        let path := x :: (sn <>> [y])
            mbs  := countMBs 0 path
         in case length path of
              6 => Hexane mbs
              n => Ring mbs n

  displace : Vector Id
  displace = scaleTo (tan (pi / 6) * ParallelDistance) (oy - ox)

  dx : Point Id
  dx = if ox == px then translate displace ox else px

  dy : Point Id
  dy = if oy == py then translate (negate displace) oy else py

  deflt : Vect 4 (Point Id)
  deflt =
    let (v,w) := parallelLine HalfParallelDistance True px py
        (x,y) := parallelLine HalfParallelDistance False px py
     in [v,w,x,y]

  notXorY : Fin k -> Bool
  notXorY n = n /= x && n /= y

  leftOfY : Angle -> Fin k -> Bool
  leftOfY phi n =
    let Just chi := angle (pointAt g n - px) | Nothing => False
     in chi - phi <= Angle.pi

  dominantNode : Fin k -> Vect 4 (Point Id)
  dominantNode n =
    let Just phi := angle (py - px) | Nothing => deflt
        (v,w)    := parallelLine ParallelDistance (leftOfY phi n) dx dy
     in [px,py,v,w]

  export
  dblBond : Vect 4 (Point Id)
  dblBond =
    case (filter (y /=) (neighbours g x), filter (x /=) (neighbours g y)) of
      ([nx1,nx2],[ny1,ny2]) =>
        let r1 := max (ringSize nx1 ny1) (ringSize nx2 ny1)
            r2 := max (ringSize nx1 ny2) (ringSize nx2 ny2)
         in if r1 >= r2 then dominantNode ny1 else dominantNode ny2
      ([_,_],[ny])  => dominantNode ny
      ([nx], [_,_]) => dominantNode nx
      ([nx], [_])   => dominantNode nx
      _             => deflt
