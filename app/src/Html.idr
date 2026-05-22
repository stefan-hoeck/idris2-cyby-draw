module Html

import CyBy.Draw
import CyBy.Draw.I18n.EN
import HTTP.API.Client.I18n
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

getDS : (r : IORef ColorScheme) => JS es DrawSettings
getDS =
  map
    (\s => {elemColor := color s} (defaultSettings abbreviations))
    (readref r)

parameters {auto st  : IORef ColorScheme}
           {auto sae : Sink AppEvent}
           {auto loc : DrawLocal}
           (ast      : IORef ColorScheme)
           (dst      : IORef DrawState)

  btns : DrawEnv -> DrawState -> JS es HTMLNodes
  btns de@(DE {}) s = Prelude.do
    c <- readref ast 
    pure
      [ expBtn saveTxt s
      , label [forID LoadIn, class widget] [Text loadTxt]
      , input
          [ ref LoadIn
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
    logOpened p
    bs <- blobBytes (up f)
    case [<] <>< forget (String.split ('.' ==) p) of
      _:<"mol" =>
        case readMolfileE (cast bs) of
          Left x  => readErr x
          Right g => sink (Event.Load g)
      _:<"smi" =>
        case smilesToMol (cast bs) of
          Left x  => readErr x
          Right m => sink (Event.Load $ initGraph m.graph)
      _:<"svg" =>
        case between "<metadata>" "</metadata>" (cast bs) of
          Nothing  => noMetadata p
          Just bs2 => case readMolfileE (toString bs2) of
            Left x  => readErr x
            Right g => sink (Event.Load g)
      _ => wrongFileType p

  appEv : AppEvent -> Async JS [] ()
  appEv (SetColor x) = writeref ast x >> sink Redraw
  appEv (LoadMol ev) = logErrs $ loadFile ev

ui : Act (AsyncStream JS [] Void)
ui = Prelude.do
  L ln ls lg <- logger Info
  E aes      <- event {fs = []} AppEvent
  ast        <- newref CyBy
  ds         <- getDS
  dst        <- newref {s = World} $ fromMol (SD 0 0) Init (G 0 empty)
  W mn ss    <- molWidget {ex = ext ast dst} getDS App (SD 300 200) Nothing

  child Content mn
  append (infoID App) ln
  pure $ Concurrent.merge
    [ foreach (appEv ast dst) aes
    , tryStream ss |> foreach (writeref dst)
    , ls
    ]

export covering
app : IO ()
app = runProg (exec ui >>= weakenErrors)
