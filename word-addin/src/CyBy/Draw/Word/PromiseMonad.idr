-- TODO: This should be moved to the word addin
module CyBy.Draw.Extensions.PromiseMonad

-- TODO: cleanup imports
import Web.Dom
import Web.MVC
import Web.Html

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

-- primitive (FFI)
%foreign "javascript:lambda:(a,b,p,succ,err,w) => p.then((x) => succ(x)(w),(x) => err(x)(w))"
prim__then :
     {0 a,b : _}
  -> Promise a
  -> (a -> PrimIO b)
  -> (JSErr -> PrimIO b)
  -> PrimIO (Promise b)

-- primitive (FFI)
%foreign "javascript:lambda:(a,b,p,succ,err,w) => p.then((x) => succ(x)(w),(x) => err(x)(w))"
prim__thenp :
     {0 a,b : _}
  -> Promise a
  -> (a -> PrimIO (Promise b))
  -> (JSErr -> PrimIO (Promise b))
  -> PrimIO (Promise b)

-- primitive (FFI)
%foreign "javascript:lambda:(a,val,w) => new Promise((f) => f(val(w)))"
prim__pure : {0 a : _} -> PrimIO a -> PrimIO (Promise a)

-- primitive (FFI)
%foreign "javascript:lambda:(a,ms,val,w) => new Promise((f) => setTimeout(() => f(val(w)), Number(ms)))"
prim__delayed : {0 a : _} -> Nat -> PrimIO a -> PrimIO (Promise a)

--------------------------------------------------------------------------------
-- Prog Monad
--------------------------------------------------------------------------------

-- not a primitive
prim__veryPure : {0 a : _} -> a -> PrimIO (Promise a)
prim__veryPure = prim__pure . MkIORes

public export
record Prog (a : Type) where
  constructor P
  run : IO (Promise (Either JSErr a))

-- primitve
liftIOEither : {0 a : _} -> IO (Either JSErr a) -> Prog a
liftIOEither io = P (fromPrim $ prim__pure (toPrim io))

liftEither : {0 a : _} ->  Either JSErr a -> Prog a
liftEither = liftIOEither . pure

pureProg : {0 a : _} -> a -> Prog a
pureProg = liftEither . Right

failProg : {0 a : _} -> JSErr -> Prog a
failProg = liftEither . Left

-- primitve
bindProg : {0 a,b : _} -> Prog a -> (a -> Prog b) -> Prog b
bindProg (P run) f = P $ do
  prom <- run
  fromPrim $
    prim__thenp
      prom
      (either (prim__veryPure . Left) (\va => toPrim $ (f va).run))
      (prim__veryPure . Left)

-- primitve
withError : {0 a : _} -> Prog a -> Prog (Either JSErr a)
withError (P run) = P $ do
  prom <- run
  fromPrim $
    prim__thenp
      prom
      (prim__veryPure . Right . either Left Right)
      (prim__veryPure . Right . Left)

export
Functor Prog where
  map f p = bindProg p (pureProg . f) 

export
Applicative Prog where
  pure = pureProg
  ff <*> fa = bindProg ff (<$> fa)

export
Monad Prog where
  (>>=) = bindProg

export
HasIO Prog where
  liftIO = liftIOEither . map Right

export
handle : {0 a : _} -> (JSErr -> Prog a) -> Prog a -> Prog a
handle f x =
  withError x >>= \case
    Right v  => pure v
    Left err => f err

export
liftPromise : {0 a : _} -> Promise a -> Prog a
liftPromise p =
  P $ fromPrim $ prim__then p
      (\a,w => MkIORes (Right a) w)
      (\e,w => MkIORes (Left e) w)

export
liftPrimPromise : {0 a : _} -> PrimIO (Promise a) -> Prog a
liftPrimPromise p = primIO p >>= liftPromise

-- primitive
run' : HasIO io => (JSErr -> IO ()) -> Prog () -> io (Promise ())
run' handle (P run) = do
  prom <- liftIO run
  primIO $ prim__then prom
    (either (\x => toPrim $ handle x) MkIORes)
    (\x => toPrim $ handle x)

export
runProg : HasIO io => (JSErr -> IO ()) -> Prog () -> io ()
runProg f = ignore . run' f

export
runDeflt : HasIO io => Prog () -> io ()
runDeflt = runProg (\x => putStrLn "Error: \{dispErr x}")

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

export
delayed : Nat -> Prog ()
delayed n = P $ fromPrim (prim__delayed n (MkIORes (Right ())))

-- TODO: Remove this eventually
prog : Prog ()
prog = do
  handle (putStrLn . ("Oops: " ++) . dispErr) $ do
    delayed 200
    putStrLn "hello world"
    delayed 200
    putStrLn "Goodbye"
  delayed 200
  delayed 200
  putStrLn "Tschüss"

main : IO ()
main = runProg (putStrLn . dispErr) prog
