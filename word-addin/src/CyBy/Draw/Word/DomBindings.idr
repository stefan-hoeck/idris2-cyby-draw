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

-------------------------------------------------------------------------------
-- Word API Types
-------------------------------------------------------------------------------

-- Context
export
data Context : Type where [external]

-- Selection
export
data Selection : Type where [external]

-- InlinePicture
export
data InlinePicture : Type where [external]

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

%foreign "browser:lambda:(s,b64,w)=> s.insertInlinePictureFromBase64(b64, Word.InsertLocation.end)"
prim__insertInlinePictureFromB64 : Selection -> String -> PrimIO InlinePicture

%foreign "browser:lambda:(a,o,w)=> o.isEmpty?1:0"
prim__isEmpty : a -> PrimIO Bool

export
%foreign "browser:lambda:(s)=> btoa(s)"
btoa : String -> String

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
  browser:lambda:(ooxml,id,w)=> {
    const regEx = new RegExp(`<w:drawing>(?:(?!<\\/w:drawing>).)*?<wp:extent cx="([^"]+?)" cy="([^"]+?)"(?:(?!<\\/w:drawing>).)*?:embed="` + id,'s');
    const match = ooxml.match(regEx);
    return match ? `${match[1]} ${match[2]}` : '';
  }
  """
prim__extractTempSvgSize : String -> String -> String

-- getting the id from the temporary image
%foreign
  """
  browser:lambda:(str,w)=> {
    // search for the image number
    const regExImg = /<pkg:part\\s+pkg:name="\\/word\\/media\\/([^"]+?)\\.svg"[^>]*>(?:(?!<pkg:part)[\\s\\S])*?<temp/s;
    const matchImg = str.match(regExImg);
    const imageNo = matchImg ? matchImg[1] : ''
    // search the id with the corresponding image number
    const regExId = new RegExp(`<Relationship Id="([^"]+)"[^>]*Target="media\\/${imageNo}\\.svg"`,'s');
    const matchId = str.match(regExId);
    const idNo = matchId ? matchId[1] : ''
    return idNo
  }
  """
prim__extractTempImageId : String -> String

export %foreign
  """
  browser:lambda:(svg,w)=> {
    const regEx = new RegExp(`<\/metadata><\/svg>`,'s');
    return svg.replace(regEx,`</metadata><temp></temp></svg>`);
  }
  """
createTempSvg : String -> String

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

%foreign "browser:lambda:(inlPic,w)=> inlPic.delete()"
prim__deleteInlinePicture : InlinePicture -> PrimIO ()


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
deleteInlinePicture : HasIO io => InlinePicture -> io ()
deleteInlinePicture inlPic = primIO (prim__deleteInlinePicture inlPic)

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
insertInlinePictureFromB64 : HasIO io => Selection -> String -> io InlinePicture
insertInlinePictureFromB64 s b = primIO (prim__insertInlinePictureFromB64 s b)

export
isEmpty : HasIO io => a -> io Bool
isEmpty o = primIO (prim__isEmpty o)

export %inline
extractMetadata : Ooxml -> String
extractMetadata cxp = prim__extractMetadata cxp.value

export
extractTempSvgSize : Ooxml -> (id : String) -> Maybe (String,String)
extractTempSvgSize ooxml id =
  case words $ prim__extractTempSvgSize ooxml.value id of
    [x,y] => Just (x,y)
    _     => Nothing

export %inline
extractImageIdWordSel : Ooxml -> String
extractImageIdWordSel s = prim__extractImageIdWordSel s.value

||| does nearly the same as `getImageIdWordSel` but searches the whole
||| document for the temporary image's id
export %inline
extractTempImageId : Ooxml -> String
extractTempImageId s = prim__extractTempImageId s.value

export %inline
replaceSvgAndSize :
     Ooxml
  -> (svg,idSel : String)
  -> (String,String)
  -> Ooxml
replaceSvgAndSize ooxml svg idSel (cx,cy) =
  O $ prim__replaceSvgAndSize ooxml.value svg idSel cx cy

export
hasCyBySvg : Ooxml -> Bool
hasCyBySvg ooxml = prim__hasCyBySvg ooxml.value
