module Html

import CyBy.Draw
import Data.List
import Text.Molfile
import Text.CSS.Color
import Web.Async.Util
import Web.Async.View
import Text.SVG

%default total

messages : Ref Div
messages = Id "messages"

printMsg : DrawMsg -> String
printMsg Copied        = "Structure copied to clipboard"
printMsg (ReadErr str) = "Error when pasting structure: \{str}"

clearMsg : DrawEvent -> Act ()
clearMsg (KeyUp str) = pure ()
clearMsg _           = children messages []

logAndDisplay : DrawSettings => Sink DrawEvent => Sink DrawMsg => DrawEvent -> DrawState -> Act DrawState
logAndDisplay e s =
 let s2 := update e s
  in clearMsg e >> displaySketcher "app" e s2 $> s2

ui : DrawSettings => Prog Void ()
ui = do
  des <- signal (KeyDown "Escape")
  dms <- signal Copied

  merge
    [ discrete dms |> tail |> foreach (child messages . Text . printMsg)
    , mvcActSignal (init (SD 600 400) Init "") logAndDisplay
    ]

export covering
app : IO ()
app = runProg $ ui @{defaultSettings abbreviations}
