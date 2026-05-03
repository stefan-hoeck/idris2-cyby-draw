module Html

import CyBy.Draw
import CyBy.Draw.Word
import CyBy.UI.JS
import Data.List
import Text.CSS.Color
import Text.HTML.DomID
import Text.Molfile
import Text.SVG
import Web.Async.Util
import Web.Async.View

%default total

-- parameters {auto ds : DrawSettings}
--            {auto de : Sink DrawEvent}
--            {auto lg : Logger JS}
-- 
--   wordDisp : DrawState -> DrawEvent -> Act DrawState
--   wordDisp s e =
--    let s2 := update e s
--     in displaySketcher {ex = WordExt} "app" e s2 $> s2
-- 
--   logAndDisplay : DrawState -> DrawEvent -> Act DrawState
--   logAndDisplay s SVGimp = importImage >>= wordDisp s . Load
--   logAndDisplay s e      = wordDisp s e
--   
--   handled : DrawState -> DrawEvent -> JS [] DrawState
--   handled s e =
--     attempt (logAndDisplay s e) >>= \case
--       Left (Here x) => logLoggable x $> s
--       Right res     => pure res

ui : DrawSettings => AsyncStream JS [] Void
-- ui = do
--   E des      <- exec $ event {fs = []} DrawEvent
--   E dms      <- exec $ event {fs = []} DrawMsg
--   L ln ls lg <- exec $ logger Info
-- 
--   merge
--     [ foreach logLoggable dms
--     , ls
--     , P.cons (KeyDown "Escape") des
--         |> P.evalScans1 (init (SD 400 266) Init "") handled
--         |> drain
--     ]

export covering
app : IO ()
-- app = runProg $ weakenErrors $ ui @{defaultSettings abbreviations}
