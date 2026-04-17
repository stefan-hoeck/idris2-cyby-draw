module Html

import CyBy.Draw
import Data.List
import Text.CSS.Color
import Text.HTML.DomID
import Text.Molfile
import Text.SVG
import Web.Async.Util
import Web.Async.View

%default total

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

AppLog : DomID
AppLog = "app-log"

lvl : LogLevel -> Class
lvl l = C "loglvl-\{l}"

logNode : LogLevel -> List String -> HTMLNode
logNode l msgs =
  div [class "log-row"]
    [ div [class $ lvl l] [Text $ "[\{l}]"]
    , div [class "log-msg"] $ intersperse (br []) (map Text msgs)
    ]

printErr : JSErr -> JS [] ()
printErr x = putStrLn "Error: \{dispErr x}"

uilog : LogLevel -> Logger JS
uilog x =
  MkLogger $ \l,ml => Prelude.do
    when (l >= x) $ handle [printErr] (prepend (elemRef AppLog) $ logNode l ml)

parameters {auto lg : Logger JS}
  Loggable JS DrawMsg where
    logLoggable Copied      = info "Structure copied to clipboard"
    logLoggable (ReadErr s) = error "Error when pasting structure: \{s}"

  logAndDisplay : DrawSettings => Sink DrawEvent => DrawEvent -> DrawState -> Act DrawState
  logAndDisplay e s =
   let s2 := update e s
    in displaySketcher "app" e s2 $> s2

ui : DrawSettings => JSStream Void
ui = do
  let lg := uilog Info
  E des <- exec $ eventFrom (KeyDown "Escape")
  mvcActEvs des (init (SD 600 400) Init "") logAndDisplay

export covering
app : IO ()
app = runProg $ ui @{defaultSettings abbreviations}
