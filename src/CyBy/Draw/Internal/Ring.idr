module CyBy.Draw.Internal.Ring

import Data.Fin
import Data.Array.Indexed
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Label
import CyBy.Draw.Internal.Role
import Geom
import Text.Molfile

%default total

ringBonds : (n : Nat) -> List (Edge n CDBond)
ringBonds n =
  catMaybes $ natEdge 0 (pred n) :: map (\x => natEdge x (S x)) [0 .. pred n]
  where
    natEdge : Nat -> Nat -> Maybe (Edge n CDBond)
    natEdge x y = do
      fx <- tryNatToFin x
      fy <- tryNatToFin y
      mkEdge fx fy (CB None $ cast Single)

add : Integer -> Double -> Double
add n v = if n `mod` 2 == 0 then v / 2.0 else pi / (- 2.0)

||| Creates a regular, saturated n-cycle of carbon atoms.
export
nring : (n : Nat) -> {auto 0 p : LT 2 n} -> CDIGraph n
nring n =
  let step := TwoPi / cast n
      plus := add (cast n) step
      as   := generate n (\f => angle $ cast (cast {to = Nat} f) * step + plus)

      -- length of vectors pointing from the origin to the points of the n-gon
      -- this follows from `sine phi = opposite / hypotenuse`
      l    := scale $ value BondLengthInPixels / (2.0 * sin (step / 2.0))
   in adjAtomTypes $ mkGraph
        (toVect $ map (\a => elemAt C (translate (polar l a) origin) None) as)
        (ringBonds n)

||| Creates a regular, saturated n-cycle of carbon atoms.
export
ring : (n : Nat) -> {auto 0 p : LT 2 n} -> CDGraph
ring n = G n $ nring n
