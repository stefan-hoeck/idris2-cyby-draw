module Debug

import CyBy.Draw
import Data.List1
import Data.String
import Derive.Prelude
import Geom
import Text.Lex.Manual
import Text.Show.Pretty
import Web.Async.Util

%default total
%language ElabReflection

export
debugInfo : DrawEvent -> DrawState -> String
debugInfo e s =
  """
  Current Event: \{show e}
  """

--------------------------------------------------------------------------------
--          Molecule and Atom Info
--------------------------------------------------------------------------------

-- a monoid for accumulating information about molecules
record Info where
  constructor I
  formula   : Formula
  mass      : MolecularMass
  exactMass : MolecularMass

%runElab derive "Info" [Show,Eq,Semigroup,Monoid]

toMass : Double -> MolecularMass
toMass = fromMaybe 1.0e60 . refineMolecularMass

info : CDAtom -> Info
info (CA _ a) = I (cast a) (molecularMass a) (exactMolecularMass a)

formula : Formula -> List HTMLNode
formula (F ps) = pairs ps >>= dispPair
  where
    dispPair : (Elem,Nat) -> List HTMLNode
    dispPair (e,1) = [span [class "formula_elem"] [Text $ symbol e]]
    dispPair (e,n) =
      [ span [class "formula_elem"] [Text $ symbol e]
      , span [class "formula_count"] [Text $ show n]
      ]

infoEntry : String -> List HTMLNode -> List HTMLNode
infoEntry l s =
  [ div [class "info_label"] [Text l]
  , div [class "info_value"] s
  ]

infoStr : String -> String -> List HTMLNode
infoStr l = infoEntry l . pure . Text

iso : Maybe MassNr -> String
iso = maybe "Mix" (show . value)

bondType : BondType -> String
bondType Dbl = "double"
bondType x   = toLower $ show x

neighbour : Fin k -> MolBond -> List HTMLNode
neighbour n b =
  [ div [class "neighbour_index"] [Text $ show n]
  , div [class "neighbour_type"]  [Text $ bondType b.type]
  ]

neighbours : AssocList k CDBond -> HTMLNode
neighbours ns =
  div [class "neighbours"] $ pairs ns >>= \(n,b) => neighbour n b.molBond


atomDetails : CDGraph -> List HTMLNode
atomDetails (G _ g) =
  case find (is Hover) (contexts g) of
    Just (C n (CA _ a) ns) =>
      join
        [ infoStr "Element" (symbol a.elem.elem)
        , infoStr "Isotope" (iso a.elem.mass)
        , infoStr "Charge" (show a.charge.value)
        , infoStr "Implicit hydrogens" (show a.hydrogen.value)
        , infoStr "Atom type" a.type.name
        , infoStr "Index" (show n)
        , infoEntry "Neighbours" . pure $ neighbours ns
        ]
    Nothing => []

export
molDetails : CDGraph -> List HTMLNode
molDetails (G 0 _) = []
molDetails g =
  let I f m em := foldMap info g
   in join 
        [ infoEntry "Formula" (formula f)
        , infoStr "Mass" "\{printDouble 3 m.value} g/mol"
        , infoStr "Exact mass" "\{printDouble 7 em.value} g/mol"
        , atomDetails g
        ]
