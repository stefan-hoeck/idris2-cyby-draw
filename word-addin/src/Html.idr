module Html

import CyBy.Draw
import CyBy.Draw.Word
import Data.List
import Text.Molfile
import Text.HTML.DomID
import Text.CSS.Color
import Web.Async.Util
import Web.Async.View
import Text.SVG

%default total

messages : Ref Void
messages = elemRef $ the DomID "messages"

printMsg : DrawMsg -> String
printMsg Copied        = "Structure copied to clipboard"
printMsg (ReadErr str) = "Error when pasting structure: \{str}"

clearMsg : DrawEvent -> Act ()
clearMsg (KeyUp str) = pure ()
clearMsg _           = children messages []

%hint
logger : Logger JS
logger =
  filter Debug $ MkLogger $ \lvl,ms =>
    traverse_ putStrLn $ map (\x => "[ \{toLower $ show lvl} ] \{x}") ms


logAndDisplay : DrawSettings => Sink DrawEvent => Sink DrawMsg => DrawState -> DrawEvent -> Act DrawState
logAndDisplay s e =
 let s2 := update e s
  in clearMsg e >> displaySketcher {ex = WordExt} "app" e s2 $> s2


ui : DrawSettings => JSStream Void
ui = do
  E des <- exec $ event {fs = [JSErr]} DrawEvent
  E dms <- exec $ event {fs = [JSErr]} DrawMsg

  merge
    [ foreach (child messages . Text . printMsg) dms
    , P.cons (KeyDown "Escape") des
        |> P.evalScans1 (init (SD 600 400) Init "") logAndDisplay
        |> drain
    ]

export covering
app : IO ()
app = runProg $ ui @{defaultSettings abbreviations}
