module CyBy.Draw.Extensions.DomBindings

import JS
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

export
ToFFI InlinePicture InlinePicture where toFFI = id

export
FromFFI InlinePicture InlinePicture where fromFFI = Just

-------------------------------------------------------------------------------
-- Prim Functions
-------------------------------------------------------------------------------

-- Accessor functions

%foreign "browser:lambda:(a,fun,w) => Word.run((c) => fun(c)(w))"
prim__wordRun : (Context -> PrimIO (Promise a)) -> PrimIO (Promise a)

%foreign "browser:lambda:(c,w)=> c.document.getSelection()"
prim__selection : Context -> PrimIO Selection

%foreign "browser:lambda:(s,b64,w)=> s.insertInlinePictureFromBase64(b64, Word.InsertLocation.end)"
prim__inlinePictureFromB64 : Selection -> String -> PrimIO InlinePicture

%foreign "browser:lambda:(a,o,w)=> o.isEmpty"
prim__isEmpty : a -> PrimIO Boolean

%foreign "browser:lambda:(s,w)=> btoa(s)"
prim__btoa : String -> PrimIO String

-- TODO: Do this directly in Idris!
%foreign "browser:lambda:(w)=> { return 'cyby_draw_img_' + Date.now() + Math.floor(Math.random() * 10000);}"
prim__uniqueID : PrimIO String


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


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- Accessor functions

export
wordRun : (Context -> Prog a) -> Prog a
wordRun f = P $ fromPrim (prim__wordRun (\c,w => (toPrim (f c).run w)))

export
selection : Context -> Prog Selection
selection c = liftIO $ fromPrim (prim__selection c)

-- TODO: Maybe add the ability to change the `InsertLocation`, now the img
-- is added at the end of the selection.
export
inlinePictureFromB64 : Selection -> String -> Prog InlinePicture
inlinePictureFromB64 s b = liftIO $ fromPrim (prim__inlinePictureFromB64 s b)

export
isEmpty : a -> Prog Bool
isEmpty o = fromBool $ fromPrim (prim__isEmpty o)

export
bToA : String -> Prog String
bToA str = liftIO $ fromPrim (prim__btoa str)

export
uniqueId : Prog String
uniqueId = liftIO $ fromPrim prim__uniqueID

-- Mutator functions

export
syncContext : Context -> Prog ()
syncContext c = liftPrimPromise (prim__syncContext c)

||| Takes an JS object and a comma-delimited string of properties for loading
||| this properties for later use.
export
load : a -> (properties : String) -> Prog ()
load o props = liftIO $ fromPrim (prim__load o props)

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
addCustomXMLParts c xml = liftIO $ fromPrim (prim__addCustomXMLParts c xml)
