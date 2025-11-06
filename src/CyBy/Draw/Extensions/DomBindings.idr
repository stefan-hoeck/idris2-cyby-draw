module CyBy.Draw.Extensions.DomBindings

import JS
import Web.Internal.DomTypes
import Data.Array.Indexed
import CyBy.Draw.Extensions.PromiseMonad

%default total

-------------------------------------------------------------------------------
-- Word API Types
-------------------------------------------------------------------------------

-- Context
export
data Context : Type where [external]

export
ToFFI Context Context where toFFI = id

export
FromFFI Context Context where fromFFI = Just

-- Selection
export
data Selection : Type where [external]

export
ToFFI Selection Selection where toFFI = id

export
FromFFI Selection Selection where fromFFI = Just

-- InlinePicture
export
data InlinePicture : Type where [external]

-- DOMParser
export
data DOMParser : Type where [external]

-- Ooxml
-- Is a ooxml object in form of a string
export
data Ooxml : Type where [external]

-- ClientResult a
export
data ClientResult : Type -> Type where [external]

-- CustomXmlPartCollection
export
data CustomXmlPartCollection : Type where [external]

-- CustomXmlPart
export
data CustomXmlPart : Type where [external]


-------------------------------------------------------------------------------
-- Prim Functions
-------------------------------------------------------------------------------

-- Accessor functions

%foreign "browser:lambda:(a,fun,w) => Word.run((c) => fun(c)(w))"
prim__wordRun : (Context -> PrimIO (Promise a)) -> PrimIO (Promise a)

%foreign "browser:lambda:(c,w)=> c.document.getSelection()"
prim__selection : Context -> PrimIO Selection

%foreign "browser:lambda:(c,w)=> c.document.body.getOoxml()"
prim__getOoxml : Context -> PrimIO (ClientResult Ooxml)

%foreign "browser:lambda:(s,w)=> s.getOoxml()"
prim__getSelectionOoxml : Selection -> PrimIO (ClientResult Ooxml)

%foreign "browser:lambda:(s,w)=> s.getOoxml()"
prim__getSelectionString : Selection -> PrimIO (ClientResult String)

%foreign "browser:lambda:(o,w)=> o"
prim__ooxmlToString : Ooxml -> PrimIO String

%foreign "browser:lambda:(s,b64,w)=> s.insertInlinePictureFromBase64(b64, Word.InsertLocation.end)"
prim__insertInlinePictureFromB64 : Selection -> String -> PrimIO InlinePicture

%foreign "browser:lambda:(a,o,w)=> o.isEmpty"
prim__isEmpty : a -> PrimIO Boolean

%foreign "browser:lambda:(s,w)=> btoa(s)"
prim__btoa : String -> PrimIO String

-- TODO: Do this directly in Idris!
%foreign "browser:lambda:(w)=> { return 'cyby_draw_img_' + Date.now() + Math.floor(Math.random() * 10000);}"
prim__uniqueID : PrimIO String

%foreign "browser:lambda:(w)=> new DOMParser()"
prim__DOMParser : PrimIO DOMParser

%foreign "browser:lambda:(p,ooxml,w)=> p.parseFromString(ooxml,'text/xml')"
prim__parseFromStringXml : DOMParser -> Ooxml -> PrimIO Document

%foreign "browser:lambda:(ooxmls,str,w)=> Array.from(ooxmls.getElementsByTagName(str))"
prim__getElementsByTagName : Document -> String -> PrimIO AnyPtr

%foreign "browser:lambda:(elem,str,w)=> elem.getAttribute(str) ? elem.getAttribute(str) : '' "
prim__getAttribute : Element -> String -> PrimIO  String

%foreign "browser:lambda:(c,w)=> c.document.customXmlParts"
prim__customXmlParts : Context -> PrimIO CustomXmlPartCollection

%foreign "browser:lambda:(cxpc,w)=> cxpc.items"
prim__itemsCustomXmlParts : CustomXmlPartCollection -> PrimIO AnyPtr

%foreign "browser:lambda:(e,q,w)=> e.query(q,{})"
prim__query : CustomXmlPart -> String -> PrimIO $ ClientResult AnyPtr

%foreign "browser:lambda:(o,w)=> o.items"
prim__items : Ooxml -> PrimIO AnyPtr

%foreign "browser:lambda:(a,o,w)=> o.value"
prim__valueClientResult: ClientResult a -> PrimIO a

%foreign "browser:lambda:(cxp,exp,w)=> cxp.match(new RegExp(exp))[1]"
prim__extractId: Ooxml -> String -> PrimIO String

%foreign "browser:lambda:(f,s,w)=> {return f(s)(w);}"
prim__return : (String -> PrimIO ()) -> String -> PrimIO ()

%foreign "browser:lambda:(o,w)=> o.xml"
prim__getXml : CustomXmlPart -> PrimIO String

%foreign 
  """
  browser:lambda:(xml,id,w)=> {
    const regEx = new RegExp(`<id>${id}<\/id><graph>(.*)<\/graph>`,'s');
    const match = xml.match(regEx);
    return match ? match[1].replace(/\\r/g,'') : '';
  }
  """
prim__getGraphById : String -> String -> PrimIO String

%foreign "browser:lambda:(o,w)=> o.xml"
prim__getInlinePicutes : Selection -> PrimIO AnyPtr

%foreign "browser:lambda:(img,w)=> img.altTextDescription"
prim__getAltTextDescr : InlinePicture -> PrimIO String


-- Mutator functions

%foreign "browser:lambda:(c,w)=> c.sync()"
prim__syncContext : Context -> PrimIO (Promise ())

%foreign "browser:lambda:(a,o,s,w)=> o.load(s || undefined)"
prim__load : a -> String -> PrimIO ()

%foreign "browser:lambda:(a,c,o,w)=> c.trackedObjects.add(o)"
prim__addTrackedObj : Context -> a -> PrimIO ()

%foreign "browser:lambda:(a,c,o,w)=> c.trackedObjects.remove(o)"
prim__removeTrackedObj : Context -> a -> PrimIO ()

%foreign "browser:lambda:(img,descr,w)=> img.altTextDescription = descr"
prim__addAltTextDescr : InlinePicture -> String -> PrimIO ()

%foreign "browser:lambda:(c,xmlContent,w)=> c.document.customXmlParts.add(xmlContent)"
prim__addCustomXMLParts : Context -> String -> PrimIO ()

%foreign "browser:lambda:(cxp,w)=> cxp.delete()"
prim__delCustomXmlPart : CustomXmlPart -> PrimIO ()


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- Mutator functions

export
syncContext : Context -> Prog ()
syncContext c = liftPrimPromise (prim__syncContext c)

||| Takes an JS object and a comma-delimited string of properties for loading
||| this properties for later use.
export
load : Context -> a -> (properties : String) -> Prog ()
load c o props = do
  liftIO $ fromPrim (prim__load o props)
  syncContext c

export
addTrackedObj : Context -> (object : a) -> Prog ()
addTrackedObj c o = liftIO $ fromPrim (prim__addTrackedObj c o)

export
removeTrackedObj : Context -> (object : a) -> Prog ()
removeTrackedObj c o = liftIO $ fromPrim (prim__removeTrackedObj c o)

export
addAltTextDescr : InlinePicture -> String -> Prog ()
addAltTextDescr i s = liftIO $ fromPrim (prim__addAltTextDescr i s)

export
addCustomXMLParts : Context -> String -> Prog ()
addCustomXMLParts c xml = do
  liftIO $ fromPrim (prim__addCustomXMLParts c xml)
  syncContext c

export
delCustomXmlPart : CustomXmlPart -> Prog ()
delCustomXmlPart cxp = liftIO {io=Prog} $ fromPrim (prim__delCustomXmlPart cxp)


-- Accessor functions

export
wordRun : (Context -> Prog a) -> Prog a
wordRun f = P $ fromPrim (prim__wordRun (\c,w => (toPrim (f c).run w)))

export
valueClientResult : ClientResult a -> Prog a
valueClientResult a = liftIO $ fromPrim (prim__valueClientResult a)

export
selection : Context -> Prog Selection
selection c = liftIO $ fromPrim (prim__selection c)

export
getOoxml : Context -> Prog Ooxml
getOoxml c = do
  crOoxml <- liftIO $ fromPrim (prim__getOoxml c)
  syncContext c
  valueClientResult crOoxml

export
getSelectionOoxml : Context -> Selection -> Prog Ooxml
getSelectionOoxml c s = do
  crOoxml <- liftIO $ fromPrim (prim__getSelectionOoxml s)
  syncContext c
  valueClientResult crOoxml

export
getSelectionString : Context -> Selection -> Prog String
getSelectionString c s = do
  crString <- liftIO $ fromPrim (prim__getSelectionString s)
  syncContext c
  valueClientResult crString

-- TODO: Maybe add the ability to change the `InsertLocation`, now the img
-- is added at the end of the selection.
export
insertInlinePictureFromB64 : Selection -> String -> Prog InlinePicture
insertInlinePictureFromB64 s b = liftIO $ fromPrim (prim__insertInlinePictureFromB64 s b)

export
isEmpty : a -> Prog Bool
isEmpty o = fromBool $ fromPrim (prim__isEmpty o)

export
bToA : String -> Prog String
bToA str = liftIO $ fromPrim (prim__btoa str)

export
uniqueId : Prog String
uniqueId = liftIO $ fromPrim prim__uniqueID

export
domParser : Prog DOMParser
domParser = liftIO $ fromPrim prim__DOMParser

export
parseFromStringXml : DOMParser -> Ooxml -> Prog Document
parseFromStringXml p xml = liftIO $ fromPrim (prim__parseFromStringXml p xml)

export
getAttribute : String -> Element -> Prog String
getAttribute str e = liftIO $ fromPrim (prim__getAttribute e str)

export
customXmlParts : Context -> Prog CustomXmlPartCollection
customXmlParts c = liftIO $ fromPrim (prim__customXmlParts c)

export
extractId : Ooxml -> (regEx : String) -> Prog String
extractId cxp regEx = liftIO $ fromPrim (prim__extractId cxp regEx)

export
return : (String -> PrimIO ()) -> String -> Prog ()
return f s = liftIO {io=Prog} $ fromPrim (prim__return f s)

export
ooxmlToString : Ooxml -> Prog String
ooxmlToString o = liftIO $ fromPrim (prim__ooxmlToString o)

export
getXml : CustomXmlPart -> Prog String
getXml cxp = liftIO $ fromPrim (prim__getXml cxp)

export
getGraphById : (xml,id : String) -> Prog String
getGraphById xml id = liftIO {io=Prog} $ fromPrim (prim__getGraphById xml id)

export
getInlinePictures :  Selection -> Context -> Prog $ Indexed.Array InlinePicture
getInlinePictures s c = do
  ptr <- liftIO {io=Prog} $ fromPrim (prim__getInlinePicutes s)
  inlPics <- unsafeJSArrayOf InlinePicture ptr
  load c inlPics ""
  pure inlPics

export
getAltText : InlinePicture -> Prog String
getAltText img = liftIO {io=Prog} $ fromPrim (prim__getAltTextDescr img)
  


-- Array functions

export
getElementsByTagName : Document -> String -> Prog $ Indexed.Array Element
getElementsByTagName d str = do
  ptr <- liftIO $ fromPrim (prim__getElementsByTagName d str)
  unsafeJSArrayOf Element ptr

export
query : CustomXmlPart -> String -> Context -> Prog (Indexed.Array Ooxml)
query cxp s c = do
  crPtr <- liftIO $ fromPrim (prim__query cxp s)
  syncContext c
  ptr <- valueClientResult crPtr
  unsafeJSArrayOf {io=Prog} Ooxml ptr

export
itemsCustomXmlParts : Context -> CustomXmlPartCollection -> Prog $ Indexed.Array CustomXmlPart
itemsCustomXmlParts c cc = do
  ptr <- liftIO $ fromPrim (prim__itemsCustomXmlParts cc)
  unsafeJSArrayOf CustomXmlPart ptr

export
items : Ooxml -> Prog $ Indexed.Array Ooxml
items o = do
  ptr <- liftIO $ fromPrim (prim__items o)
  unsafeJSArrayOf Ooxml ptr

--------------------------------------------------------------------------------
-- Testing
--------------------------------------------------------------------------------

%foreign "browser:lambda:(cxp,w)=> console.log(cxp.xml)"
prim__printXmlPart : CustomXmlPart -> PrimIO ()

export
printXmlPart : CustomXmlPart -> Prog ()
printXmlPart cxp = liftIO $ fromPrim (prim__printXmlPart cxp)

%foreign "browser:lambda:(s,w)=> s.getOoxml()"
prim__printSelection : Selection -> PrimIO $ ClientResult String

export
printSelection : Context -> Selection -> Prog ()
printSelection c s = do
  crPrintSel <- liftIO $ fromPrim (prim__printSelection s)
  syncContext c
  printSel <- valueClientResult crPrintSel
  consoleLog printSel

  

%foreign "browser:lambda:(ooxmls,str,w)=> Array.from(ooxmls.getElementsByTagName(str))[0]"
prim__getFirstElemByTagName : Document -> String -> PrimIO Element

export
getFirstElemByTagName : Document -> String -> Prog Element
getFirstElemByTagName d str = liftIO $ fromPrim (prim__getFirstElemByTagName d str)


%foreign "browser:lambda:(ooxmls,str,w)=> Array.from(ooxmls.getElementsByTagName(str))"
prim__replaceElemNodeBy : Element -> String -> PrimIO ()

export
replaceElemNodeBy : Element -> String -> Prog ()
replaceElemNodeBy e str = liftIO $ fromPrim (prim__replaceElemNodeBy e str)

%foreign "browser:lambda:(s,ooxmls,w)=> {s.insertOoxml(ooxmls,Word.InsertLocation.replace); console.log('Done');}"
prim__replaceOoxml : Selection -> String -> PrimIO ()

export
replaceOoxml : Selection -> String -> Prog ()
replaceOoxml s str = liftIO $ fromPrim (prim__replaceOoxml s str)

%foreign "browser:lambda:(d,w)=> {const serializer = new XMLSerializer(); return serializer.serializeToString(d);}"
prim__docToOoxml : Document -> PrimIO String

export
docToOoxml : Document -> Prog String
docToOoxml d = liftIO $ fromPrim (prim__docToOoxml d)

%foreign 
  """
  browser:lambda:(s,svg,w)=> {
    const regEx = new RegExp(/<svg xmlns[\\s\\S]*?svg>/,'s');
    const newString = s.replace(regEx, svg);
    return newString;
  }
  """
prim__replaceRegEx : (elem,svg : String) -> PrimIO String

export
replaceRegEx : (elem,svg : String) -> Prog String
replaceRegEx e svg = liftIO $ fromPrim (prim__replaceRegEx e svg)
