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

messages : DomID
messages = "log-msg"

printMsg : DrawMsg -> String
printMsg Copied        = "[ info ] Structure copied to clipboard"
printMsg (ReadErr str) = "[ error ] Error when pasting structure: \{str}"

toLogRow : String -> HTMLNode
toLogRow str = li [ class "long-row" ] [Text str]

%hint
logger : Logger JS
logger =
  filter Debug $ MkLogger $ \lvl,ms =>
    traverse_ putStrLn $ map (\x => "[ \{toLower $ show lvl} ] \{x}") ms

parameters {auto ds : DrawSettings}
           {auto de : Sink DrawEvent}
           {auto dm : Sink DrawMsg}

  wordDisp : DrawState -> DrawEvent -> Act DrawState
  wordDisp s e =
   let s2 := update e s
    in displaySketcher {ex = WordExt} "app" e s2 $> s2

  logAndDisplay : DrawState -> DrawEvent -> Act DrawState
  logAndDisplay s SVGimp = importImage >>= wordDisp s . Load
  logAndDisplay s e      = wordDisp s e
  
  handled : DrawState -> DrawEvent -> JS [] DrawState
  handled s e =
    attempt (logAndDisplay s e) >>= \case
      Left (Here x) => logLoggable x $> s
      Right res     => pure res

ui : DrawSettings => AsyncStream JS [] Void
ui = do
  E des <- exec $ event {fs = []} DrawEvent
  E dms <- exec $ event {fs = []} DrawMsg

  merge
    [ foreach (elemPrepend messages . toLogRow . printMsg) dms
    , P.cons (KeyDown "Escape") des
        |> P.evalScans1 (init (SD 400 266) Init "") handled
        |> drain
    ]

export covering
app : IO ()
app = runProg $ weakenErrors $ ui @{defaultSettings abbreviations}
