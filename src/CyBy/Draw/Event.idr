module CyBy.Draw.Event

import CyBy.Draw.Internal.Abbreviations
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.Graph
import Derive.Finite
import Derive.FromJSON.Simple
import Derive.ToJSON.Simple
import Derive.Prelude
import JSON.Simple
import Text.Molfile
import Web.Canvas

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Color Schemes
--------------------------------------------------------------------------------

public export
data ColorScheme = Black | CyBy | Groups | CPK | CDK | JMol | PyMol

%runElab derive "ColorScheme" [Show,Eq,Ord,Finite,FromJSON,ToJSON]

--------------------------------------------------------------------------------
--          Event
--------------------------------------------------------------------------------

||| Modifier key such as "Shift" or "Ctrl" currently being pressed.
public export
data Modifier = NoMod | Ctrl | Shift

%runElab derive "Modifier" [Show, Eq]

||| Resets the pressed modifier key if it matches the keyboard key being
||| lifted.
export
reset : (mod, current : Modifier) -> Modifier
reset m c = if m == c then NoMod else c

||| A data type for logging messages.
|||
||| Typically, these will not be handled by cyby-draw directly but
||| by applications embedding our drawing canvas into their own
||| UI.
public export
data DrawMsg : Type where
  ||| Data was copied to clipboard
  Copied  : DrawMsg
  
  ||| Invalid data was read from clipboard
  ReadErr : String -> DrawMsg

%runElab derive "DrawMsg" [Show, Eq]

public export
data DrawEvent : Type where
  ZoomIn           : (atPos : Bool) -> DrawEvent
  ZoomOut          : (atPos : Bool) -> DrawEvent
  Undo             : DrawEvent
  Redo             : DrawEvent
  SetElem          : Elem -> DrawEvent
  ChgElem          : Elem -> DrawEvent
  ChgCharge        : Charge -> DrawEvent
  ChgMass          : Maybe MassNr -> DrawEvent
  SelAbbr          : Abbreviation -> DrawEvent
  EnableAbbr       : DrawEvent
  SetBond          : MolBond -> DrawEvent
  Move             : (x,y : Double) -> DrawEvent
  LeftDown         : DrawEvent
  LeftUp           : DrawEvent
  MiddleDown       : DrawEvent
  MiddleUp         : DrawEvent
  SetTempl         : CDGraph -> DrawEvent
  Load             : CDGraph -> DrawEvent
  SelectMode       : DrawEvent
  KeyDown          : String -> DrawEvent
  KeyUp            : String -> DrawEvent
  EraseMode        : DrawEvent
  Focus            : DrawEvent
  Blur             : DrawEvent
  Clear            : DrawEvent
  Expand           : DrawEvent
  Center           : DrawEvent
  Redraw           : DrawEvent
  Resize           : (h,w : Double) -> DrawEvent
  StartPSE         : DrawEvent
  SVG              : DrawEvent
  SVGimp           : DrawEvent

%runElab derive "DrawEvent" [Show, Eq]
