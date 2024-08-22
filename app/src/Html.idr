module Html

import CyBy.Draw
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

logAndDisplay : DrawSettings => DrawEvent -> DrawState -> Cmd DrawEvent
logAndDisplay (Msg m) s = child messages $ Text (printMsg m)
logAndDisplay e       s = clearMsg e <+> displaySketcher "app" e s

covering export
app : IO ()
app =
  let se := defaultSettings abbreviations
   in runMVC
        update
        (logAndDisplay @{se})
        (putStrLn . dispErr)
        (KeyDown "Escape")
        (init @{se} (SD 600 400) Init "")
