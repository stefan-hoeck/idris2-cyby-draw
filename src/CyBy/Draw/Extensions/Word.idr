||| This module takes care of all events used for the word add-in (extension)
module CyBy.Draw.Extensions.Word

import Web.MVC
import Web.Internal.DomTypes
import Data.String
import Data.Vect

import Data.Graph.Indexed
import Derive.Prelude
import CyBy.Draw.Event
import CyBy.Draw.MoleculeCanvas
import CyBy.Draw.Internal.Settings
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Extensions.Util

import CyBy.Draw.Extensions.DomBindings
import CyBy.Draw.Extensions.PromiseMonad

%default total
%language ElabReflection

--------------------------------------------------------------------------------
-- Debugging
--------------------------------------------------------------------------------

data Level = Trace | Debug | Info | Silence

%runElab derive "Level" [Show,Eq,Ord]

export
record LogLevel where
  [noHints]
  constructor L
  lvl : Level

failing "Can't find an implementation for LogLevel"
  testLogLevel : LogLevel
  testLogLevel = %search

export
lvlDebug : LogLevel 
lvlDebug = L Debug

export
lvlTrace : LogLevel 
lvlTrace = L Trace

export
lvlInfo : LogLevel 
lvlInfo = L Info

export
lvlSilence : LogLevel 
lvlSilence = L Silence

export
log : HasIO io => (d : LogLevel) => Level -> Lazy String -> io ()
log lvl msg = when (lvl >= d.lvl) (putStrLn msg)

export
trace : HasIO io => (d : LogLevel) => Lazy String -> io ()
trace = log Trace

export
debug : HasIO io => (d : LogLevel) => Lazy String -> io ()
debug = log Debug

export
info : HasIO io => (d : LogLevel) => Lazy String -> io ()
info = log Info

--------------------------------------------------------------------------------
-- Export / Import Structures
--------------------------------------------------------------------------------

||| A quick note to the mechanism used in this module:
||| As Word is based on the XML-Structure, its JS API allows
||| the extraction of it in form of a string. Therefore, the
||| information of the structure is saved inside certain XML
||| nodes.
||| For exporting a new structure from CyBy-Draw to
||| Word, the JS API offers a simple image insert function
||| which generates the needed nodes. To retrieve the structure
||| at a later time, the MOL-Graph is stored inside a metadata
||| node inside the svg. However, in case a structure (generated
||| with the CyBy-Draw Add-In) should only be modified, the
||| whole svg node has to be replaced with help of the JS
||| string function `replace`. TODO: Size modification!
||| To get a structure from Word, the cursor selection XML-String
||| can be searched for the first occurrence of the svg node
||| and its first attribute `xmlns`. For Word always adds a
||| `viewBox` attribute and places it as the first attribute
||| of an svg node, only CyBy-Draw generated svg's current
||| selection are searched for. As the RegEx is non-greedy,
||| only the first svg occurrence is relevant.

exportImgEmptSel : LogLevel => Selection -> (svg,mol : String) -> Prog ()
exportImgEmptSel s svg mol = do
  debug "Selection is empty or no svg is present for replacing the structure"
  -- encode and insert the image to Word
  -- the JS API creates an xml entry, where the graph is stored
  -- in the svg reference for later use
  b64 <- bToA svg
  ignore $ insertInlinePictureFromB64 s b64
  debug "exportImage succcesfull"

exportImage : LogLevel => (svg,mol : String) -> Prog ()
exportImage svg mol =
  wordRun $ \c => do
    debug "Begin of function `exportImage`"
    -- replace the selected svg (or the first in the selection)
    -- with the updated structure if an svg is selected
    -- if the selection is empty or does not include an svg,
    -- insert the new structure after the selection / cursor
    s <- getSelection c
    False <- isEmpty s | True => exportImgEmptSel s svg mol

    -- load the whole selection as xml
    ooxml <- getSelectionOoxml c s

    hasSvg <- hasSvg ooxml
    if not hasSvg
      then exportImgEmptSel s svg mol
      else do
        -- TODO: Adjust the size of the image
        -- replace the first occurring svg with the updated one
        -- non- selective is it is actually a chemical structure
        -- or just a normal svg
        ooxmlS <- replaceRegEx ooxml svg
        replaceOoxml s ooxmlS

        debug "Function `exportImage` succcesfull"

exportImageToWord : LogLevel => (svg,molFile : String) -> JSIO ()
exportImageToWord svg mol =
  liftIO $ runProg (putStrLn . ("Error: " ++) . dispErr) (exportImage svg mol)


importImageFromWord : LogLevel => (String -> PrimIO ()) -> Prog ()
importImageFromWord f =
  wordRun $ \c => do
    debug "Begin of function `importImageFromWord`"
    -- return an empty string if the selection is empty
    s <- getSelection c
    False <- isEmpty s | True => debug "Selection is empty" >> return f ""

    -- load the whole selection as xml
    ooxml <- getSelectionOoxml c s

    -- extracting the MOL file directly from the xml structure
    -- of the current selection
    -- if there are several cyby-draw generated structures, the
    -- first in the selection is imported
    graph <- extractMetadata ooxml

    if graph == ""
      then debug "No MOL-File found"
      else debug "Function `exportImage` succcesfull" >> return f graph 

fromWord : LogLevel => Cmd DrawEvent
fromWord =
  C (\h =>
      liftIO $ runProg
        (
        putStrLn . ("Error: " ++) . dispErr)
        (importImageFromWord (\s,w =>
          case readMolfileE s of
            Left e  => toPrim (runJS $ h (Msg $ ReadErr e)) w
            Right m => toPrim (runJS $ h (SetTempl m)) w
        )
        )
    )


||| Parses a word event and forms a DrawEvent command.
export
dispWordExt : LogLevel => DrawSettings => ExtensionEvent -> DrawState -> Cmd DrawEvent
dispWordExt ExportSVG s = cmd_ $ exportImageToWord (exportSVG s) (toMolStr s)
dispWordExt ImportSVG s = fromWord
