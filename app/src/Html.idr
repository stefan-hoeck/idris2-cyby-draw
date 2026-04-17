module Html

import CyBy.Draw
import Data.List
import Text.CSS.Color
import Text.HTML.DomID
import Text.Molfile
import Text.SVG
import Web.Async.Util
import Web.Async.View
import Web.Internal.Types

%default total

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

AppLog : DomID
AppLog = "app-log"

Content : Ref Tag.Body
Content = Id "content"

lvl : LogLevel -> Class
lvl l = C "cyby-draw-loglvl-\{l}"

logNode : LogLevel -> List String -> HTMLNode
logNode l msgs =
  div [class "cyby-draw-log-row"]
    [ div [class $ lvl l] [Text $ "[\{l}]"]
    , div [class "cyby-draw-log-msg"] $ intersperse (br []) (map Text msgs)
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
  r     <- exec $ castElementByRef Content >>= getClientRect
  let dims := SD (cast $ r.width - 350) (cast $ r.height - 100)
  mvcActEvs des (init dims Init "") logAndDisplay

export covering
app : IO ()
app = runProg $ ui @{defaultSettings abbreviations}
