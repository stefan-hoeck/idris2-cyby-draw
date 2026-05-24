module Html

import CyBy.Draw
import CyBy.Draw.Word
import CyBy.Draw.Word.I18n.EN
import CyBy.UI.JS
import Data.List
import Text.CSS.Color
import Text.HTML.DomID
import Text.Molfile
import Text.SVG
import Web.Async.Util
import Web.Async.View

%default total

App : String
App = "app"

Content : Ref Tag.Body
Content = Id "content"

getDS : (r : IORef ColorScheme) => JS es DrawSettings
getDS =
  map
    (\s => {elemColor := color s} (defaultSettings abbreviations))
    (readref r)

ui : Act (AsyncStream JS [] Void)
ui = do
  ast        <- newref CyBy
  L ln ls lg <- logger Info
  W mn ss    <- molWidget {ex = WordExt} getDS App (SD 300 200) Nothing

  children Content mn
  append (infoID App) ln

  pure $ Concurrent.merge [ls, tryStream (drain ss)]

export covering
app : IO ()
app = runProg $ exec ui >>= weakenErrors
