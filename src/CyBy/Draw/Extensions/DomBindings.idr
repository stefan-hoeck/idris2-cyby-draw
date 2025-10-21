module CyBy.Draw.Extensions.DomBindings

import Web.Dom
import Web.MVC
import Web.Html

import JS


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

export
data Context : Type where [external]

export
ToFFI Context Context where toFFI = id

export
FromFFI Context Context where fromFFI = Just

-------------------------------------------------------------------------------
-- Prim Functions
-------------------------------------------------------------------------------

namespace Word

  export
  %foreign "browser:lambda:(fun,w) => {Word.run(async context => {fun(context); await context.sync(); console.log('End of wordRun function');})}"
  prim__wordRun : (Context -> PrimIO ()) -> PrimIO ()
  
  export
  %foreign "browser:lambda:(c,w)=> {return c.document;}"
  prim__document : Context -> PrimIO Document
  
  export
  %foreign "browser:lambda:(d,w)=> {return d.body}"
  prim__body : Document -> PrimIO Types.Body
  
  export
  %foreign "browser:lambda:(b,s,w)=> {b.insertText(s, Word.InsertLocation.start); console.log('insText -> context: ', context);}"
  prim__insText : Types.Body -> String -> PrimIO ()

  export
  %foreign "browser:lambda:(c,w)=> (async c => {await c.sync();})"
  prim__syncContext : Context -> PrimIO ()


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

export
wordRun : (Context -> JSIO ()) -> JSIO ()
wordRun f = primIO  $ prim__wordRun $ \c => toPrim $ ignore $ runEitherT $ f c

export
document : Context -> JSIO Document
document = tryJS "document" . Word.prim__document

export
body : Document -> JSIO Types.Body
body = tryJS "body" . Word.prim__body

export
insText : Types.Body -> String -> JSIO ()
insText b str = primIO $ prim__insText b str

export
syncContext : Context -> JSIO ()
syncContext c = primIO $ prim__syncContext c
