||| This module takes care of all events used for the word add-in (extension)
module CyBy.Draw.Extensions.Word

import Web.MVC
import Web.Internal.DomTypes
import Data.String
import Data.Vect

import Data.Graph.Indexed
import CyBy.Draw.Event
import CyBy.Draw.MoleculeCanvas
import CyBy.Draw.Internal.Settings
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Extensions.Util

import CyBy.Draw.Extensions.DomBindings
import CyBy.Draw.Extensions.PromiseMonad

%default total

--------------------------------------------------------------------------------
-- Util
--------------------------------------------------------------------------------

getSelection : Context -> Prog Selection
getSelection c = do
  s <- selection c
  load c s "isEmpty"
  syncContext c
  pure s

-- keeping only the valid id's
selectIDs : Indexed.Array String -> Indexed.Array String
selectIDs =
  foldMap
    (\x => if isPrefixOf "cyby_draw_img_" x then A _ (array [x]) else A _ empty)

-- keeping the Xml objects in the array that are not linked to an
-- image in word
xmlToDelete :
     Context
  -> (ids : Indexed.Array String)
  -> CustomXmlPart
  -> Prog $ Indexed.Array CustomXmlPart
xmlToDelete c ids cxp = do
  query <- query cxp "//graphInfo/id" c
  case query of
    (A Z arr)     => pure empty
    (A (S n) arr) => do
      matchingS <-  extractId (atNat arr Z) "<id>(.*?)</id>"
      if elem matchingS ids then pure empty else pure $ A _ (array [cxp])

-- checking for deleted images and removing their linked xml
-- object
checkValidXmlObjects : Prog ()
checkValidXmlObjects = do
  wordRun $ \c => do
    -- load the whole doc as xml
    ooxml <- getOoxml c
    prs <- domParser
    xmlS <- parseFromStringXml prs ooxml
    -- search for the shapes and inlinePictures and collect
    -- the id's
    imgElems <- getElementsByTagName xmlS "wp:docPr"
    as <- traverse (getAttribute "descr") imgElems 
    ids <- pure $ selectIDs as
    
    -- search for id's not linked to an image in the word file
    -- and delete the whole xml object of that id's
    allCustomXmlParts <- customXmlParts c
    load c allCustomXmlParts "items"
    parts <- itemsCustomXmlParts c allCustomXmlParts

    -- keep the ids of the deleted images in an array and
    -- delete their xml object afterwards
    toDelCXPs <- map join $ traverse (xmlToDelete c ids) parts
    ignore $ traverse {f = Prog} delCustomXmlPart toDelCXPs
    syncContext c
    allCustomXmlParts <- customXmlParts c
    load c allCustomXmlParts "items"

exportImgEmptSel : Context -> Selection -> (svg,mol : String) -> Prog ()
exportImgEmptSel c s svg mol = do
  -- encode, insert and add the image to the tracked objects
  b64 <- bToA svg
  img <- insertInlinePictureFromB64 s b64
  addTrackedObj c img
  load c img ""

  -- creating an id and adding it to the image's alt
  -- description for reference
  id <- uniqueId
  addAltTextDescr img id
  syncContext c
  removeTrackedObj c img
  syncContext c

  -- creating a new XML structure to store the MOL graph and
  -- adding it to the context object as a `customXmlPart`
  let xmlContent :=
    #"<graphInfo><id>\#{id}</id><graph>\#{mol}</graph></graphInfo>"#
  addCustomXMLParts c xmlContent

exportImage : (svg,mol : String) -> Prog ()
exportImage svg mol =
  wordRun $ \c => do
    --debug
    putStrLn "Begin of exportImage"
    -- return an empty string if the selection is empty
    s <- getSelection c
    False <- isEmpty s | True => exportImgEmptSel c s svg mol

    -- load the whole selection as xml
    ooxml <- getSelectionOoxml c s

    -- use the DOM-Parser to search for the id of the structure
    prs <- domParser
    xmlS <- parseFromStringXml prs ooxml

    -- extract the image elements
    imgElems <- getElementsByTagName xmlS "wp:docPr"
    as <- map toList $ traverse (getAttribute "descr") imgElems 
    -- if a selected image with a cyby-draw id is found,
    -- replace the image and the corresponding customXml
    -- with the modified version
    let Just id := find (isPrefixOf "cyby_draw_img_") as
      -- if no cyby-draw image is present in the current selection,
      -- insert the image after the selection as a new image
      | _ => exportImgEmptSel c s svg mol

    -- debug
    putStrLn $ "an id was found " ++ id
--  
--    -- 1. Find the xml of the id
--    -- 2. replace the MOL-file with in the customXmlPart with the new one
--

    -- testing
    ooxmlS <- getSelectionString c s
    putStrLn $ "Old selection:\n" ++ ooxmlS
    putStrLn $ "New svg:\n" ++ svg
    ooxmlS' <- replaceRegEx ooxmlS svg
    replaceOoxml s ooxmlS'
    ooxmlSnew <- getSelectionString c s
    putStrLn $ "New selection:\n" ++ ooxmlSnew

    ----
--    -- encode, insert and add the image to the tracked objects
--    b64 <- bToA svg
--    img <- insertInlinePictureFromB64 s b64
--    addTrackedObj c img
--    load c img ""
--  
--    -- creating an id and adding it to the image's alt
--    -- description for reference
--    id <- uniqueId
--    addAltTextDescr img id
--    syncContext c
--    removeTrackedObj c img
--    syncContext c
--  
--    -- creating a new XML structure to store the MOL graph and
--    -- adding it to the context object as a `customXmlPart`
--    let xmlContent :=
--      #"<graphInfo><id>\#{id}</id><graph>\#{mol}</graph></graphInfo>"#
--    addCustomXMLParts c xmlContent
  
    -- clean up unused XML objects
    checkValidXmlObjects
    putStrLn "exportImage succcesfull"

-- extracting the xml of the CustomXmlPart and getting the MOL-Graph
-- if the ids match
findIDGraph :
     Context
  -> (id : String)
  -> (acc : Prog String)
  -> CustomXmlPart
  -> Prog String 
findIDGraph c id acc cxp = do
  mol <- acc
  if mol /= "" then pure mol else do
    xml <- getXml cxp
    getGraphById xml id

importImageFromWord : (String -> PrimIO ()) -> Prog ()
importImageFromWord f =
  wordRun $ \c => do
    -- clean up unused XML objects
    checkValidXmlObjects
    
    -- return an empty string if the selection is empty
    s <- getSelection c
    False <- isEmpty s | True => return f ""

    -- load the whole selection as xml
    ooxml <- getSelectionOoxml c s

    -- use the DOM-Parser to search for the id of the structure
    prs <- domParser
    xmlS <- parseFromStringXml prs ooxml


--    -- testing
--    putStrLn "elem"
--    elem <- getFirstElemByTagName xmlS "svg"
--    putStrLn "replaceElem"
--    replaceElemNodeBy elem "<svg>test</svg>"
--    syncContext c
--    putStrLn "docToOoxml"
--    newOoxml <- docToOoxml xmlS
--    putStrLn newOoxml
--    putStrLn "replaceOoxml"
--    replaceOoxml s newOoxml
--    syncContext c
--    printSelection c s
--    ----


    -- extract the image elements
    imgElems <- getElementsByTagName xmlS "wp:docPr"
    as <- map toList $ traverse (getAttribute "descr") imgElems 
    let Just id := find (isPrefixOf "cyby_draw_img_") as
      | _ => putStrLn "Error: No image ID was found" >> return f ""

    -- extract the mol file data (of the selected image) by id
    -- from the stored XML objects
    allCustomXmlPartCollection <- customXmlParts c
    load c allCustomXmlPartCollection "items"
    parts <- itemsCustomXmlParts c allCustomXmlPartCollection
    syncContext c
    graph <- foldl (findIDGraph c id) (pure "") parts

    return f graph

exportImageToWord : (svg,molFile : String) -> JSIO ()
exportImageToWord svg mol =
  liftIO $ runProg (putStrLn . ("Error: " ++) . dispErr) (exportImage svg mol)

fromWord : Cmd DrawEvent
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
dispWordExt : DrawSettings => ExtensionEvent -> DrawState -> Cmd DrawEvent
dispWordExt ExportSVG s = cmd_ $ exportImageToWord (exportSVG s) (toMolStr s)
dispWordExt ImportSVG s = fromWord
