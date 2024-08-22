module CyBy.Draw.Internal.Role

import Data.Bits
import Derive.Prelude

%default total
%language ElabReflection

||| The role(s) an object (typically an atom or bond) in the
||| drawing currently has (for instance, it is selected, or
||| the mouse hovers over it, or it is currently being drawn).
|||
||| We encode this as a bit pattern to facilitate adding new roles and
||| having several roles set simultaneously.
|||
||| `Role` is a semigroup (using bitwise "or", `(.|.)`, for append) and
||| a monoid, with 0 as the neutral element.
public export
record Role where
  constructor R
  role : Bits8

%runElab derive "Role" [Show,Eq,Ord]

export %inline
Semigroup Role where
  R x <+> R y = R (x .|. y)

export %inline
Monoid Role where neutral = R 0

public export
None, Hover, Selected, Origin, New, Persistent, HoverNew, Highlight : Role
None       = R 0
Hover      = R 1
Selected   = R 2
Origin     = R 4
New        = R 8
Highlight  = R 16
Persistent = Hover <+> Selected
HoverNew   = Hover <+> New

||| Interface for objects with a `Role` we can modify
public export
interface ModRole a where
  modRole : (Role -> Role) -> a -> a

export %inline
ModRole Role where modRole f = f

||| Sets the given role at an object in the drawing
export
setIf : ModRole a => Role -> Bool -> a -> a
setIf r True  = modRole (r <+>)
setIf r False = modRole $ \(R y) => R (y `xor` (r.role .&. y))

||| Sets the given role at an object in the drawing
export %inline
set : ModRole a => Role -> a -> a
set r = r `setIf` True

||| Sets the given role at an object in the drawing
export %inline
unset : ModRole a => Role -> a -> a
unset r = r `setIf` False

||| Keep only the given roles and unset all others
export %inline
keep : ModRole a => Role -> a -> a
keep (R x) = modRole $ \(R y) => R (x .&. y)

||| Completely remove all roles
export %inline
clear : ModRole a => a -> a
clear = modRole (const None)

||| Tests if the given role(s) is/are set at the given object
||| in the drawing
export
is : Cast a Role => Role -> a -> Bool
is (R x) v = (x .&. role (cast v)) == x

||| Selection mode we are currently in.
|||
||| `Ignore` means that we are currently not selecting this type of item.
||| `One`    means "single-select" mode (SHIFT is not down)
||| `Many`   means "multi-select" mode (SHIFT is down)
public export
data SelectMode = Ignore | One | Many

||| Selects a hovered node or edge.
|||
||| The boolean flag indicates, if we want to keep already selected
||| node or not (as indicated by the `Shift` key being down).
export
selectIfHovered : ModRole a => SelectMode -> a -> a
selectIfHovered Ignore = unset Selected
selectIfHovered One    = modRole (\x => setIf Selected (is Hover x) x)
selectIfHovered Many   =
  modRole (\x => setIf Selected (is Hover x || is Selected x) x)
