module CyBy.UI.JS

import Data.List
import Text.HTML.Select

import public CyBy.UI.HTML
import public Web.Async

%default total

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

data LogEv = Clear | Lvl LogLevel

printErr : JSErr -> JS [] ()
printErr x = putStrLn "Error: \{dispErr x}"

export
logNode : LogLevel -> List String -> HTMLNode
logNode l msgs =
  li []
    [ label [class $ level l] [Text $ "[\{l}]"]
    , div [] $ intersperse (br []) (map Text msgs)
    ]

export
uilog : IORef LogLevel => Logger JS
uilog @{ref} =
  MkLogger $ \l,ml => Prelude.do
    x <- readref ref
    when (l >= x) $ handle [printErr] (prepend (elemRef CyByLog) $ logNode l ml)

levels : List LogLevel
levels = [Trace,Debug,Info,Warn,Error,Fatal]

appLog : Sink LogEv => HTMLNode
appLog =
  section
    [ class drawLog ]
    [ header []
        [ label [] [Text "Log"]
        , spacer
        , button [onClick Clear] ["Clear"]
        , selectFromList' levels (Just Info) show Lvl []
        ]
    , ul [ref CyByLog] []
    ]

public export
record Logger where
  constructor L
  node   : HTMLNode
  stream : AsyncStream JS [] Void
  logger : Logger JS

onev : (ref : IORef LogLevel) => LogEv -> Async JS [] ()
onev Clear   = handle [printErr] $ children (elemRef CyByLog) []
onev (Lvl x) = writeref ref x

export
logger : LogLevel -> Act Logger
logger l = Prelude.do
  ref  <- newref l
  E es <- event LogEv
  pure $ L appLog (foreach onev es) uilog

--------------------------------------------------------------------------------
-- Input Validation
--------------------------------------------------------------------------------

validIcon : DOMLocal => Ref Tag.Div -> EditRes t -> HTMLNode
validIcon r (Valid _)   = div [class iconPlaceholder, Id r] []
validIcon r (Invalid s) = div [class iconError, Id r, title s] [iwarn]
validIcon r Missing     =
  div [class iconMissing, Id r, title $ editRes {t} Missing] [iwarn]

export
validated : DOMLocal => Editor t -> Editor t
validated ed =
  E $ \m => Prelude.do
    lbl     <- uniqueRef Tag.Div
    W ns vs <- ed.widget m
    pure $ W
      [div [class validatedInput] $ ns ++ [validIcon lbl $ Missing {t}]]
      (observe (replace lbl . validIcon lbl) vs)
