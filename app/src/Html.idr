module Html

import CyBy.Draw
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

--------------------------------------------------------------------------------
-- Dialog
--------------------------------------------------------------------------------

icon : Sink e => Class -> e -> List (Attribute Tag.Button) -> HTMLNode
icon v ev as = button (classes ["cyby-icon",v] :: onClick ev :: as) []

btnCls : {0 t : _} -> EditRes t -> Attribute Tag.Button
btnCls (Valid _) = classes ["cyby-icon", "ok"]
btnCls _         = classes ["cyby-icon", "ok-disabled"]

EditDialog : DomID
EditDialog = "edit-dialog"

EditOK : DomID
EditOK = "dialog-edit-ok"

fileEdit : Editor FileEv
fileEdit = E $ \_ => fileIn [accept ".mol"]

parameters (addr : Sink ConfirmEv => String -> HTMLNode -> HTMLNode)
           (ttl  : String)

  conf : HTMLNode -> Act (Sink (EditRes t), Widget ConfirmEv)
  conf n = Prelude.do
    E cs <- event ConfirmEv
    E es <- event (EditRes t)
    pure $ MkPair %search $
      W (addr ttl n) $ merge [cs, foreach (\x => let c := btnCls x in putStrLn (displayAttributes [c]) >> attr (btnRef EditOK) c) es]

  dialogEdit : Editor t -> Maybe t -> Act (JSStream $ Maybe t)
  dialogEdit ed m = confirmedModal conf EditDialog ed m

endEdit : Act ()
endEdit = cleanupDialog EditDialog

iok, icancel : Sink ConfirmEv => HTMLNode
iok = icon "ok-disabled" OK [ref EditOK]
icancel = icon "cancel" C.Cancel []

confAttrs : {0 t : _} -> Sink ConfirmEv => Attributes t
confAttrs = [onEnterDown OK, onRemove C.Cancel]

addRow : Sink ConfirmEv => String -> HTMLNode -> HTMLNode
addRow s n =
  dialog
    [ ref EditDialog, class "cyby-draw-edit-dialog", onClose C.Cancel ]
    [ div (class "cyby-draw-cancel-edit" :: confAttrs)
       [ div [class "cyby-draw-header"] [Text s]
       , n
       , div [class "cancel-edit"] [iok, icancel]
       ]
    ]

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
  Load     : AppEvent

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
      , icon "load" Html.Load []
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

  loadFile : Maybe FileEv -> Act ()
  loadFile Nothing  = info "file opening aborted"
  loadFile (Just $ FE f p) = Prelude.do
    info "file opened: \{p}"
    bs <- blobBytes (up f)
    case [<] <>< forget (String.split ('.' ==) p) of
      _:<"mol" =>
        case readMolfileE (cast bs) of
          Left x  => logLoggable (ReadErr x)
          Right g => sink (Event.SetTempl g)
      _:<"smi" =>
        case smilesToMol (cast bs) of
          Left x  => logLoggable (ReadErr x)
          Right m => sink (Event.SetTempl $ initGraph m.graph)
      _:<"svg" =>
        case between "<metadata>" "</metadata>" (cast bs) of
          Nothing  => logLoggable (ReadErr ".svg file does not contain required metadata")
          Just bs2 => case readMolfileE (toString bs2) of
            Left x  => logLoggable (ReadErr x)
            Right g => sink (Event.SetTempl g)
      _ => logLoggable (ReadErr "unsupported file type")


  appEv : AppEvent -> JSStream Void 
  appEv (SetColor x) = exec $ mod ast {scheme := x} >> sink Redraw
  appEv Load         = Prelude.do
    s <- exec $ dialogEdit addRow "Load Molecule" fileEdit Nothing
    P.head s |> foreach (\m => loadFile m >> endEdit)

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
    , flatMap aes (appEv ast dst)
    , P.evalScans1 st (drawEv ast dst) des |> foreach (writeref dst)
    ]

export covering
app : IO ()
app = runProg ui
