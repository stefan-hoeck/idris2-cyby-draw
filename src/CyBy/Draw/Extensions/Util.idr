module CyBy.Draw.Extensions.Util

import Web.Dom
import Web.MVC
import Web.Html

import Data.Graph.Indexed
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Atom
import Data.SnocList
import Data.String

%default total

public export
data ExtensionEvent = ExportSVG | ImportSVG

export
extractAndParseMetadata : String -> Either String CDGraph
extractAndParseMetadata str =
  let len := cast {to=Int} $ length str
   in case strSubstr 7 (len - 16) str of
        "" => Left $ "No mol file data found!"
        m  => readMolfileE m

namespace Prim

  %foreign "browser:lambda:(x,f) => x.run(async (context) => {f});"
  wordRun : Alias.Object -> (Alias.Object -> ()) -> PrimIO ()

  %foreign "browser:lambda:x => x.getOoxml()"
  getOxXml : Alias.Body -> PrimIO String

  %foreign "browser:lambda:x => x.sync()"
  sync : Alias.Object -> PrimIO ()
