module Html

import CyBy.Draw
import CyBy.Draw.I18n.EN
import Data.ByteString
import Data.Finite
import Data.Linear.Sink
import Data.List
import Data.List1
import Geom.Gen2D.Debug
import HTTP.I18n.EN
import Text.CSS.Color
import Text.HTML.Extra
import Text.Molfile
import Text.SVG
import Web.Async.Confirm
import Web.Async.Extra.I18n.EN
import Web.Async.Extra.Widget
import Web.Internal.Types

%default total
%hide Text.SVG.Types.Path.t

LoadIn : DomID
LoadIn = "load-input"

App : String
App = "app"

Content : Ref Tag.Body
Content = Id "content"

data AppEvent : Type where
  SetColor : Sink DrawEvent => ColorScheme -> AppEvent
  LoadMol  : Sink DrawEvent => FileEv -> AppEvent

getDS : (r : IORef ColorScheme) => JS es DrawSettings
getDS =
  map
    (\s => {elemColor := color s} (defaultSettings abbreviations))
    (readref r)

parameters {auto sae : Sink AppEvent}
           {auto loc : DrawLocal}

  btns : DrawEnv -> DrawState -> ColorScheme -> HTMLNodes
  btns de@(DE {}) s c =
    [ expBtn saveTxt s
    , label [forID LoadIn, class Class.btn] [Text loadTxt]
    , input
        [ ref LoadIn
        , type File
        , onFileIn LoadMol
        , acceptAll [".mol",".smi",".svg"]
        ]
    , selectFromList' values (Just c) show SetColor []
    ]

  ext : IORef ColorScheme => Extension
  ext @{st} =
    E
      { doExport = storeSVG . exportSVG
      , buttons  = \e,x => btns e x <$> readref st
      , adjust   = \_,_,s => disableExport s
      }

  loadFile : IORef ColorScheme => Sink DrawEvent => FileEv -> Act ()
  loadFile @{st} (FE f p) = Prelude.do
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

  appEv : (st : IORef ColorScheme) => AppEvent -> Async JS [] ()
  appEv (SetColor x) = writeref st x >> sink Redraw
  appEv (LoadMol ev) = logErrs $ loadFile ev

ui : Act (AsyncStream JS [] Void)
ui = Prelude.do
  L ln ls lg <- logger @{HTTPEN} @{ExtraEN} Info
  let den    := DrawEN {log = lg}
  E aes      <- event {fs = []} AppEvent
  ast        <- newref CyBy
  ds         <- getDS
  W mn ss    <- molWidget {ex = ext} getDS App (SD 300 200) Nothing

  children Content mn
  append (infoID App) ln
  pure $ Concurrent.merge
    [ foreach appEv aes
    , drain $ tryStream ss
    , ls
    ]

export covering
app : IO ()
app = runProg (exec ui >>= weakenErrors)
