module CyBy.Draw.Word.DomBindings

import CyBy.Draw.Word.PromiseMonad
import Data.String
import JS
import Web.Internal.DomTypes

%default total

public export
record Ooxml where
  constructor O
  value : String

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
prim__wordRun : (Context -> PrimIO (Promise a)) -> PrimIO (Promise a)

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

%foreign
  """
  browser:lambda:(ooxml,w)=> {
    const regEx = new RegExp(`<metadata>(.*?)<\/metadata>`,'s');
    const match = ooxml.match(regEx);
    return match ? match[1] : '';
  }
  """
prim__extractMetadata: String -> String

%foreign
  """
  browser:lambda:(str,w)=> {
    // search for the image number
    const regExImg = /<pkg:part\\s+pkg:name="\\/word\\/media\\/([^"]+?)\\.svg"[^>]*>(?:(?!<pkg:part)[\\s\\S])*?created by\\s/s;
    const matchImg = str.match(regExImg);
    const imageNo = matchImg ? matchImg[1] : ''
    // search the id with the corresponding image number
    const regExId = new RegExp(`<Relationship Id="([^"]+)"[^>]*Target="media\\/${imageNo}\\.svg"`,'s');
    const matchId = str.match(regExId);
    const idNo = matchId ? matchId[1] : ''
    return idNo
  }
  """
prim__extractImageIdWordSel : String -> String

%foreign 
  """
  browser:lambda:(ooxml,svg,idSel,cx,cy,w)=> {
    // first, replace the old svg with the new one
    const regExSvg = new RegExp(/<svg xmlns[\\s\\S]*?svg>/,'s');
    const newSvg = ooxml.replace(regExSvg, svg);
    // second, search for the `cx` and `cy` properties (there are two
    // occurrences for each of them) and replace their values with
    // the new sizes
    const regExSize = new RegExp(`<w:drawing>(?:(?!<\\/w:drawing>).)*?<wp:extent cx="[^"]+?" cy="[^"]+?"(?:(?!<\\/w:drawing>).)*?:embed="` + idSel + `(?:(?!<\\/w:drawing>).)*?:ext cx="[^"]+?" cy="[^"]+?"`,'s');
    const newSizeAndSvg = newSvg.replace(regExSize, (match) => {
      return match
        .replace(/cx="[^"]+?"/g, `cx="${cx}"`)
        .replace(/cy="[^"]+?"/g, `cy="${cy}"`);
      });
    return newSizeAndSvg;
  }
  """
prim__replaceSvgAndSize : String -> (svg,idSel : String) -> (cx,cy : String) -> String

%foreign 
  """
  browser:lambda:(ooxml,w)=> {
    const regEx = new RegExp(/<svg xmlns[\\s\\S]*?svg>/,'s');
    const match = regEx.test(ooxml);
    return match?1:0;
  }
  """
prim__hasCyBySvg : String -> Bool

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
syncContext : Context -> Prog ()
syncContext c = liftPrimPromise (prim__syncContext c)

|||  Queuing a request to fetch data for a proxy object, which initially
|||  contains no real values. `load("")` requests all properties.
|||  `context.sync()` is required afterward to retrieve the data and make it
|||  accessible!
export
load : Context -> a -> (properties : String) -> Prog ()
load c o props = primIO (prim__load o props) >> syncContext c

export
replaceOoxml : HasIO io => Selection -> Ooxml -> io ()
replaceOoxml s x = primIO (prim__replaceOoxml s x.value)

export
wordRun : (Context -> Prog a) -> Prog a
wordRun f = P $ fromPrim (prim__wordRun (\c => (toPrim (f c).run)))

export
valueClientResult : HasIO io => ClientResult a -> io a
valueClientResult a = primIO (prim__valueClientResult a)

-- loaded and synced selection
export
getSelection : Context -> Prog Selection
getSelection c = do
  s <- primIO (prim__selection c)
  load c s "isEmpty"
  syncContext c
  pure s

export
getOoxml : Context -> Prog Ooxml
getOoxml c = do
  crOoxml <- primIO (prim__getOoxml c)
  syncContext c
  s <- valueClientResult crOoxml
  pure (O s)

export
getSelectionOoxml : Context -> Selection -> Prog Ooxml
getSelectionOoxml c s = do
  crOoxml <- primIO (prim__getSelectionOoxml s)
  syncContext c
  s <- valueClientResult crOoxml
  pure (O s)

export
insertInlinePicture : HasIO io => Selection -> String -> io ()
insertInlinePicture s b = primIO (prim__insertInlinePicture s b)

export
isEmpty : HasIO io => a -> io Bool
isEmpty o = primIO (prim__isEmpty o)

export %inline
extractMetadata : Ooxml -> String
extractMetadata cxp = prim__extractMetadata cxp.value

export %inline
extractImageIdWordSel : Ooxml -> String
extractImageIdWordSel s = prim__extractImageIdWordSel s.value

export %inline
replaceSvgAndSize :
     Ooxml
  -> (svg,idSel : String)
  -> (cx,cy : EMU)
  -> Ooxml
replaceSvgAndSize ooxml svg idSel  cx cy  =
  O $ prim__replaceSvgAndSize ooxml.value svg idSel "\{cx}" "\{cy}"

export
hasCyBySvg : Ooxml -> Bool
hasCyBySvg ooxml = prim__hasCyBySvg ooxml.value
