module Html

import CyBy.Draw
import CyBy.Draw.Word
import Data.List
import Text.Molfile
import Text.CSS.Color
import Web.MVC
import Text.SVG

%default total

export
messages : Ref Div
messages = Id "messages"

printMsg : DrawMsg -> String
printMsg Copied        = "Structure copied to clipboard"
printMsg (ReadErr str) = "Error when pasting structure: \{str}"

clearMsg : DrawEvent -> Cmd DrawEvent
clearMsg (KeyUp str) = neutral
clearMsg _           = children messages []

logAndDisplay :
     {auto ex : Extension}
  -> {auto ds : DrawSettings}
  -> DrawEvent
  -> DrawState
  -> Cmd DrawEvent
logAndDisplay (Msg m) s = child messages $ Text (printMsg m)
logAndDisplay e       s = clearMsg e <+> displaySketcher {ds} {ex} "app" e s

covering export
app : IO ()
app =
  let se := defaultSettings abbreviations
      ex := WordExt lvlDebug
   in runMVC
        update
        (logAndDisplay @{ex} @{se})
        (putStrLn . dispErr)
        (KeyDown "Escape")
        (init @{se} (SD 400 266) Init "")
