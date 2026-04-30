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
uilog : LogLevel -> Logger JS
uilog x =
  MkLogger $ \l,ml => Prelude.do
    when (l >= x) $ handle [printErr] (prepend (elemRef CyByLog) $ logNode l ml)
