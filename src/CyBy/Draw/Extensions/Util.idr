module CyBy.Draw.Extensions.Util

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
