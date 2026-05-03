module Html

import CyBy.Draw
import CyBy.Draw.I18n.EN
import CyBy.UI.JS
import Data.ByteString
import Data.Finite
import Data.Linear.Sink
import Data.List
import Data.List1
import Geom.Gen2D.Debug
import Text.CSS.Color
import Text.HTML.Select
import Text.Molfile
import Text.SVG
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

Content : Ref Tag.Body
Content = Id "content"

--------------------------------------------------------------------------------
-- App
--------------------------------------------------------------------------------

data AppEvent : Type where
  SetColor : Sink DrawEvent => ColorScheme -> AppEvent
  LoadMol  : Sink DrawEvent => FileEv -> AppEvent

record AppST where
  constructor AST
  scheme : ColorScheme

getDS : (r : IORef AppST) => Act DrawSettings
getDS =
  map
    (\(AST s) => {elemColor := color s} (defaultSettings abbreviations))
    (readref r)

parameters {auto st  : IORef AppST}
           {auto sae : Sink AppEvent}
           {auto lg  : Logger JS}
           (ast      : IORef AppST)
           (dst      : IORef DrawState)

  btns : DrawEnv -> DrawState -> Act HTMLNodes
  btns de@(DE {}) s = Prelude.do
    AST c <- readref ast 
    pure
      [ expBtn "Save..." s
      , label [forID LoadIn, class widget] ["Load..."]
      , input
          [ ref LoadIn
          , hidden True
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

  loadFile : Sink DrawEvent => FileEv -> Act ()
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

ui : Act (JSStream Void)
ui = Prelude.do
  L ln ls lg <- logger Info
  E aes      <- event {fs = [JSErr]} AppEvent
  ast        <- newref (AST CyBy)
  ds         <- getDS
  dst        <- newref {s = World} $ fromMol (SD 0 0) Init (G 0 empty)
  W mn ss    <- molWidget {ex = ext ast dst} getDS App (SD 300 200) Nothing

  child Content mn
  append (infoID App) ln
  pure $ merge
    [ foreach (appEv ast dst) aes
    , foreach (writeref dst) ss
    , ls
    ]

export covering
app : IO ()
app = runProg (join $ exec ui)
