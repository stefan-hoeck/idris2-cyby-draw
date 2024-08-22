module CyBy.Draw.Internal.Atom

import CyBy.Draw.Internal.Role
import Derive.Prelude
import Geom
import Text.Molfile

%language ElabReflection
%default total

||| Atom type used in the application state of cyby-draw.
|||
||| This is a mol-file atom with perceived atom type paired with a role
||| used for drawing.
public export
record CDAtom where
  constructor CA
  role : Role
  atom : MolAtomAT

%runElab derive "CDAtom" [Show,Eq]

export %inline
group : CDAtom -> Maybe AtomGroup
group = label . atom

export %inline
inAnyGroup : CDAtom -> Bool
inAnyGroup = isJust . group

||| Sets the given `AtomGroup` (abbreviation) at an atom
export
setGroup : AtomGroup -> CDAtom -> CDAtom
setGroup g (CA r a) = CA r $ {label := Just g} a

||| Unsets the abbreviation label of an atom if it belongs to
||| the given group.
export
clearGroup : (group : Nat) -> CDAtom -> CDAtom
clearGroup g (CA r a) = CA r $ {label $= (>>= clear)} a
  where
    clear : AtomGroup -> Maybe AtomGroup
    clear (G x l) = if x == g then Nothing else (Just $ G x l)

||| True, if the given atom is part of the abbreviation with the given ID.
export %inline
inGroup : Nat -> CDAtom -> Bool
inGroup n (CA _ a) = 
  case a.label of
    Nothing      => False
    Just (G m _) => n == m

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

export %inline
Cast CDAtom Role where cast = role

export %inline
Cast (Adj k b CDAtom) Role where cast = cast . label

export %inline
Cast (Context k b CDAtom) Role where cast = cast . label

export %inline
ModRole CDAtom where modRole f = {role $= f}

public export
GetPoint CDAtom where
  gtrans = Mol
  point  = point . position . atom

export %inline
Cast CDAtom Elem where cast = elem . elem . atom

public export
ModPoint CDAtom where
  mtrans = Mol
  modPoint f (CA r v) = CA r $ {position $= modPoint f} v
