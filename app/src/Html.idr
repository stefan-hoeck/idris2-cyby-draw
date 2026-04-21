module Html

import CyBy.Draw
import Data.Finite
import Data.List
import Text.CSS.Color
import Text.HTML.DomID
import Text.HTML.Select
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

--------------------------------------------------------------------------------
-- App
--------------------------------------------------------------------------------

data AppEvent : Type where
  SetColor : ColorScheme -> AppEvent

record AppST where
  constructor AST
  scheme : ColorScheme

drawSettings : AppST -> DrawSettings
drawSettings (AST s) = {elemColor := color s} (defaultSettings abbreviations)

parameters {auto st  : IORef AppST}
           {auto sde : Sink DrawEvent}
           {auto sdm : Sink DrawMsg}
           {auto sae : Sink AppEvent}
           {auto lg  : Logger JS}
           (ast      : IORef AppST)
           (dst      : IORef DrawState)

  btns : DrawEnv -> DrawState -> Act HTMLNodes
  btns _ s = Prelude.do
    AST c <- readref ast 
    pure
      [ expBtn "svg" "store image" s
      , selectFromList values (Just c) show SetColor [class "color-scheme"]
      ]

  ext : Extension
  ext =
    E
      { doExport = storeSVG . exportSVG
      , buttons  = btns
      , adjust   = \_,_,s => disableExport s
      }

  drawEv : DrawState -> DrawEvent -> Act DrawState
  drawEv s e = Prelude.do
    ds <- drawSettings <$> readref ast
    let s2 := update e s
    displaySketcher {ex = ext} "app" e s2
    pure s2

  appEv : AppEvent -> Act ()
  appEv (SetColor x) = mod ast {scheme := x} >> sink Redraw

ui : JSStream Void
ui = Prelude.do
  let lg := uilog Info
  E dms <- exec $ event {fs = [JSErr]} DrawMsg
  E aes <- exec $ event {fs = [JSErr]} AppEvent
  E des <- exec $ eventFrom {fs = [JSErr]} (KeyDown "Escape")
  r     <- exec $ castElementByRef Content >>= getClientRect
  ast   <- newref (AST CyBy)
  let ds   := drawSettings (AST CyBy)
      dims := SD (cast $ r.width - 350) (cast $ r.height - 100)
      st   := init dims Init ""
  dst   <- newref {s = World} st
  merge
    [ foreach logLoggable dms
    , foreach (appEv ast dst) aes
    , P.evalScans1 st (drawEv ast dst) des |> foreach (writeref dst)
    ]

export covering
app : IO ()
app = runProg ui
