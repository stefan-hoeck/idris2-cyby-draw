module Html

import CyBy.Draw
import CyBy.UI.CSS.Classes
import Data.ByteString
import Data.Finite
import Data.List
import Data.List1
import Geom.Gen2D.Debug
import Text.CSS.Color
import Text.HTML.DomID
import Text.HTML.Select
import Text.Molfile
import Text.SVG
import Web.Async
import Web.Async.Confirm as C
import Web.Internal.Types

%default total
%hide Text.SVG.Types.Path.t

LoadIn : DomID
LoadIn = "load-input"

fileEdit : Editor FileEv
fileEdit = E $ \_ => fileIn [acceptAll [".mol",".smi",".svg"]]

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

App : String
App = "app"

AppLog : DomID
AppLog = "app-log"

Content : Ref Tag.Body
Content = Id "content"

logNode : LogLevel -> List String -> HTMLNode
logNode l msgs =
  div [class formRow]
    [ div [classes [formLabel, level l]] [Text $ "[\{l}]"]
    , div [class formValue] $ intersperse (br []) (map Text msgs)
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
  LoadMol  : FileEv -> AppEvent

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
      [ expBtn "Save..." s
      , label [forID LoadIn, class widget] ["Load..."]
      , input
          [ ref LoadIn
          , class hidden
          , type File
          , onFileIn LoadMol
          , acceptAll [".mol",".smi",".svg"]
          ]
      , selectFromList values (Just c) show SetColor [class widget]
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
    displaySketcher {ex = ext} App e s2
    pure s2

  loadFile : FileEv -> Act ()
  loadFile (FE f p) = Prelude.do
    info "file opened: \{p}"
    bs <- blobBytes (up f)
    case [<] <>< forget (String.split ('.' ==) p) of
      _:<"mol" =>
        case readMolfileE (cast bs) of
          Left x  => logLoggable (ReadErr x)
          Right g => sink (Event.Load g)
      _:<"smi" =>
        case smilesToMol (cast bs) of
          Left x  => logLoggable (ReadErr x)
          Right m => sink (Event.Load $ initGraph m.graph)
      _:<"svg" =>
        case between "<metadata>" "</metadata>" (cast bs) of
          Nothing  => logLoggable (ReadErr ".svg file does not contain required metadata")
          Just bs2 => case readMolfileE (toString bs2) of
            Left x  => logLoggable (ReadErr x)
            Right g => sink (Event.Load g)
      _ => logLoggable (ReadErr "unsupported file type")

  appEv : AppEvent -> Act ()
  appEv (SetColor x) = mod ast {scheme := x} >> sink Redraw
  appEv (LoadMol ev) = loadFile ev

appLog : HTMLNode
appLog =
  div
    [ class drawLog ]
    [ div [class compTitle] ["Log"]
    , div [ref AppLog, classes [compList]] []
    ]

ui : JSStream Void
ui = Prelude.do
  let lg := uilog Info
  E dms  <- exec $ event {fs = [JSErr]} DrawMsg
  E aes  <- exec $ event {fs = [JSErr]} AppEvent
  E des  <- exec $ event {fs = [JSErr]} DrawEvent
  r      <- exec $ castElementByRef Content >>= getClientRect
  ast    <- newref (AST CyBy)
  let ds   := drawSettings (AST CyBy)
      dims := SD (cast $ r.width - 350) (cast $ r.height - 100)
      st   := init dims Init ""
  dst    <- newref {s = World} st
  topadd <- exec (buttons (ext ast dst) (DE App) st)
  exec $ child Content (sketcher App topadd st)
  exec $ append (sketcherDiv App) appLog
  merge
    [ foreach logLoggable dms
    , foreach (appEv ast dst) aes
    , P.evalScans1 st (drawEv ast dst) des |> foreach (writeref dst)
    ]

export covering
app : IO ()
app = runProg ui
