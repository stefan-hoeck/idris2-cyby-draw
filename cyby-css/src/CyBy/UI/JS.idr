module CyBy.UI.JS

import public CyBy.UI.HTML
import public Web.Async

%default total

--------------------------------------------------------------------------------
-- Logging
--------------------------------------------------------------------------------

printErr : JSErr -> JS [] ()
printErr x = putStrLn "Error: \{dispErr x}"

export
uilog : IORef LogLevel => Logger JS
uilog @{ref} =
  MkLogger $ \l,ml => Prelude.do
    x <- readref ref
    when (l >= x) $ handle [printErr] (prepend (elemRef CyByLog) $ logNode l ml)
