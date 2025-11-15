module CyBy.Draw.Word.PromiseMonad

import JS

%default total

--------------------------------------------------------------------------------
-- FFI
--------------------------------------------------------------------------------

%foreign "javascript:lambda:(a,b,p,succ,err,w) => p.then((x) => succ(x)(w),(x) => err(x)(w))"
prim__then :
     Promise a
  -> (a -> PrimIO b)
  -> (JSErr -> PrimIO b)
  -> PrimIO (Promise b)

%foreign "javascript:lambda:(a,b,p,succ,err,w) => p.then((x) => succ(x)(w),(x) => err(x)(w))"
prim__thenp :
     Promise a
  -> (a -> PrimIO (Promise b))
  -> (JSErr -> PrimIO (Promise b))
  -> PrimIO (Promise b)

%foreign "javascript:lambda:(a,val,w) => new Promise((f) => f(val(w)))"
prim__pure : PrimIO a -> PrimIO (Promise a)

%foreign "javascript:lambda:(a,ms,val,w) => new Promise((f) => setTimeout(() => f(val(w)), Number(ms)))"
prim__delayed : Nat -> PrimIO a -> PrimIO (Promise a)

--------------------------------------------------------------------------------
-- Prog Monad
--------------------------------------------------------------------------------

prim__veryPure : a -> PrimIO (Promise a)
prim__veryPure = prim__pure . MkIORes

public export
record Prog (a : Type) where
  constructor P
  run : IO (Promise (Either JSErr a))

liftIOEither : IO (Either JSErr a) -> Prog a
liftIOEither io = P (fromPrim $ prim__pure (toPrim io))

liftEither : Either JSErr a -> Prog a
liftEither = liftIOEither . pure

pureProg : a -> Prog a
pureProg = liftEither . Right

failProg : JSErr -> Prog a
failProg = liftEither . Left

bindProg : Prog a -> (a -> Prog b) -> Prog b
bindProg (P run) f = P $ do
  prom <- run
  fromPrim $
    prim__thenp
      prom
      (either (prim__veryPure . Left) (\va => toPrim $ (f va).run))
      (prim__veryPure . Left)

withError : Prog a -> Prog (Either JSErr a)
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
handle : (JSErr -> Prog a) -> Prog a -> Prog a
handle f x =
  withError x >>= \case
    Right v  => pure v
    Left err => f err

export
liftPromise : Promise a -> Prog a
liftPromise p =
  P $ fromPrim $ prim__then p
      (\a,w => MkIORes (Right a) w)
      (\e,w => MkIORes (Left e) w)

export
liftPrimPromise : PrimIO (Promise a) -> Prog a
liftPrimPromise p = primIO p >>= liftPromise

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
