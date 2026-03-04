module CyBy.Draw.Word.DomBindings

import public Web.Async
import Data.SortedMap as SM

import public Data.Buffer
import public Data.ByteString

%hide JS.ByteString.ByteString
%hide Text.HTML.Node.a
%default total

export
record Ooxml where
  constructor O
  value : ByteString

export %inline
Cast Ooxml String where cast = toString . value

export %inline
Cast String Ooxml where cast = O . fromString

||| As word uses EMU's (English Metric Units) as image sizes,
||| a conversion from pixels to EUM's had to be done.
||| 1 Inch = 914400 EMU
||| 1 Inch = 96 px (as Microsoft uses 96 ppi as standard)
||| EMU = (914400 / 96) * px = 9525 * px
||| As EMU should be an Integer the Nat type is used here.
||| The difference of the floor rounding is (I think)
||| negligible.
public export
record EMU where
  constructor E
  value : Nat

export %inline
Cast Double EMU where
  cast = E . cast . (* 9525)

export %inline
Interpolation EMU where interpolate = cast . value

-------------------------------------------------------------------------------
-- Word API Types
-------------------------------------------------------------------------------

-- Context
export
data Context : Type where [external]

-- Selection
export
data Selection : Type where [external]

-- ClientResult a
export
data ClientResult : Type -> Type where [external]

-------------------------------------------------------------------------------
-- Prim Functions
-------------------------------------------------------------------------------

%foreign "browser:lambda:(a,fun,w) => Word.run((c) => fun(c)(w))"
prim__wordRun : (Context -> PrimIO ()) -> PrimIO ()

%foreign "browser:lambda:(c,w)=> c.document.getSelection()"
prim__selection : Context -> PrimIO Selection

%foreign "browser:lambda:(c,w)=> c.document.body.getOoxml()"
prim__getOoxml : Context -> PrimIO (ClientResult String)

%foreign "browser:lambda:(s,w)=> s.getOoxml()"
prim__getSelectionOoxml : Selection -> PrimIO (ClientResult String)

%foreign "browser:lambda:(s,p,w)=> { s.insertInlinePictureFromBase64(btoa(p), Word.InsertLocation.end);}"
prim__insertInlinePicture : Selection -> String -> PrimIO ()

%foreign "browser:lambda:(a,o,w)=> o.isEmpty?1:0"
prim__isEmpty : a -> PrimIO Bool

%foreign "browser:lambda:(a,o,w)=> o.value"
prim__valueClientResult: ClientResult a -> PrimIO a

%foreign "browser:lambda:(c,w)=> c.sync()"
prim__syncContext : Context -> PrimIO (Promise ())

%foreign "browser:lambda:(a,o,s,w)=> o.load(s || undefined)"
prim__load : a -> String -> PrimIO ()

%foreign "browser:lambda:(s,ooxmls,w)=> s.insertOoxml(ooxmls,Word.InsertLocation.replace)"
prim__replaceOoxml : Selection -> String -> PrimIO ()

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

export
syncContext : Context -> Act ()
syncContext c = primIO (prim__syncContext c) >>= promise

|||  Queuing a request to fetch data for a proxy object, which initially
|||  contains no real values. `load("")` requests all properties.
|||  `context.sync()` is required afterward to retrieve the data and make it
|||  accessible!
export
load : {0 a : _} -> Context -> a -> (properties : String) -> Act ()
load c o props = primIO (prim__load o props) >> syncContext c

export
replaceOoxml : HasIO io => Selection -> Ooxml -> io ()
replaceOoxml s x = primIO (prim__replaceOoxml s $ cast x)

export
wordContext : Act Context
wordContext =
  primAsync_ $ \f => ffi (prim__wordRun $ \c => primRun $ f (Right c))

export
valueClientResult : HasIO io => ClientResult a -> io a
valueClientResult a = primIO (prim__valueClientResult a)

-- loaded and synced selection
export
getSelection : Context -> Act Selection
getSelection c = do
  s <- primIO (prim__selection c)
  load c s "isEmpty"
  syncContext c
  pure s

export
getOoxml : Context -> Act Ooxml
getOoxml c = do
  crOoxml <- primIO (prim__getOoxml c)
  syncContext c
  s <- valueClientResult crOoxml
  pure (cast s)

export
getSelectionOoxml : Context -> Selection -> Act Ooxml
getSelectionOoxml c s = do
  crOoxml <- primIO (prim__getSelectionOoxml s)
  syncContext c
  s <- valueClientResult crOoxml
  pure (cast s)

export
insertInlinePicture : HasIO io => Selection -> String -> io ()
insertInlinePicture s b = primIO (prim__insertInlinePicture s b)

export
isEmpty : HasIO io => a -> io Bool
isEmpty o = primIO (prim__isEmpty o)

--------------------------------------------------------------------------------
-- Image Extraction
--------------------------------------------------------------------------------

Quote : ByteString
Quote = #"""#

Created : ByteString
Created = "created by cyby-draw"

EndTag : ByteString
EndTag = "/>"

MediaPrefix : ByteString
MediaPrefix = #"pkg:name="/word/media/"#

embed : ByteString -> ByteString
embed id = "r:embed=\"" <+> id <+> Quote

coords : (x,y : EMU) -> ByteString
coords x y = fromString "\{x}\" cy=\"\{y}\""

export %inline
extractMol : Ooxml -> Maybe ByteString
extractMol = between "<metadata>" "</metadata>" . value

0 Relationships : Type
Relationships = SortedMap ByteString ByteString

-- Extract `Id` and `Target` from all `<Relationship` entries found
-- in the given XML document and puts them in a dictionary from
-- `Target` to `Id`.
-- 
-- Entries, where either `Id` or `Target` is undefined or empty, will
-- be silently dropped.
relationships : ByteString -> Relationships
relationships =
  SM.fromList . mapMaybe getPair . manyBetween "<Relationship " EndTag
  where
    getPair : ByteString -> Maybe (ByteString,ByteString)
    getPair x =
      [| MkPair
           (betweenNonEmpty #"Target="media/"# Quote x)
           (betweenNonEmpty #"Id=""# Quote x)
      |]

first : (a -> Maybe b) -> List a -> Maybe b
first f []        = Nothing
first f (x :: xs) =
  case f x of
    Nothing => first f xs
    m       => m

-- Extracts the image ID of the first image in the selection.
imageID : Relationships -> ByteString -> Maybe (ByteString, ByteString)
imageID rel = first findCyByID . manyBetween "<pkg:part" "</pkg:part>"
  where
    findCyByID : ByteString -> Maybe (ByteString, ByteString)
    findCyByID bs = do
      guard (Created `isInfixOf` bs)
      name  <- between MediaPrefix Quote bs
      (name,) <$> lookup name rel

-- drops `<svg` start and end tag and converts to byte vector
dropTags : String -> ByteString
dropTags = drop 4 . dropEnd 6 . fromString

export %inline
replaceSvgAndSize : Ooxml -> (svg : String) -> (cx,cy : EMU) -> Ooxml
replaceSvgAndSize o svg cx cy  =
  case imageID (relationships o.value) o.value of
    Nothing      => o
    Just (nm,id) => O . replaceSVG nm . replaceCoords id $ o.value

  where
    replaceSVG : ByteString -> ByteString -> ByteString
    replaceSVG name =
      modBetween (MediaPrefix <+> name) "</pkg:part>" $
        modBetween "<svg" "</svg>" (const $ dropTags svg)

    replaceCoords : ByteString -> ByteString -> ByteString
    replaceCoords id =
      modBetweenAll "<w:drawing" "</w:drawing>" $ \t =>
        case embed id `isInfixOf` t of
          False => t
          True  => modBetweenAll " cx=\"" EndTag (const $ coords cx cy) t

export
checkSingleSelection : Ooxml -> Act ()
checkSingleSelection sel = do
  when (length (splitAtSubstring "</w:drawing>" sel.value) /= 2)
       (throw $ Caught "None or multiple images selected!")
  when (not $ isInfixOf Created $ sel.value)
       (throw $ Caught "No CyBy-Draw image selected!")
