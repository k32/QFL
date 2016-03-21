{- This code intentionally was made slightly cryptic -}
{-# LANGUAGE GADTs, StandaloneDeriving, UnicodeSyntax, KindSignatures,
             FlexibleInstances, LambdaCase, BangPatterns #-}
import System.Exit
import Data.Functor
import Control.Monad.Cont
import System.Random
import System.Posix.Signals
import System.Environment
import Control.Concurrent.MVar

instance Show (Int → Int) where
  show _ = "<function>"
instance Eq (Int → Int) where
  _ == _ = True -- It's a hack

infixl 7 :.

data T ∷ * where {J, Â, Â', S, K ∷ T; (:.) ∷ T → T → T; Ψ ∷ {σ ∷ String} → T
                 ;F ∷ (Int → Int) → T; N ∷ Int → T}
deriving instance Show T
deriving instance Eq T

parse ∷ String → [T] → T
parse ('f':'u':c) t = parse c (J:t)
parse ('b':'a':'r':c) t = parse c (Â:t)
parse ('~':c) (a:b:t) = parse c (b:.a:t)
parse ('~':_) _ = error "Parse error: missing operand(s)"
parse (_:c) t = parse c t
parse [] (h:_) = h :. Ψ []
parse [] [] = error "Parse error: empty program"

s ∷ T → T
-- Control flow combinators
s (J :. x) = (x :. S) :. K
s (K :. x :. _) = x
s (S :. x :. y :. z) = (x :. z) :. (y :. z)
-- Church to int conversion
s (F f :. N i) = N $ f i
s (F f :. F g) = F $ f . g
-- IO
s (Â' :. N i :. ψ @ (Ψ {})) = ψ {σ = toEnum i : σ ψ}
-- Using Â → Â' to indicate that Church → Int conversion has been started
s (Â :. n :. ψ @ (Ψ {})) = Â' :. (n :. F (+1) :. N 0) :. ψ
-- Other cases
s (a :. b) = (s a) :. (s b)
s x = x

eval ∷ (T → t) → (T → t) → T → t
eval fp done t | t == t'   = done t
               | otherwise = fp t'
    where t' = s t

ψs a@Ψ{σ=s} = [(a, s)]
ψs (a:.b) = ψs a ++ ψs b
ψs _ = []

r' ∷ T → [(T, String)]   -- Very inefficient; should be rewritten
r' a | null t = [(a, s)] where ((_, s):t) = ψs a
r' (a :. b) = r' a ++ r' b
r' _ = []

r ∷ T → IO (Maybe T)
r t = case r' t of
        [] → return Nothing
        t' → ((t' !!) <$> randomRIO (0, length t' - 1)) >>= \case
           (Ψ{}, s) → putStrLn (reverse s) >> return Nothing
           (t'', s) → putStrLn (reverse s) >> return (Just t'')

setMVar v = (tryTakeMVar v >>) . putMVar v

loop v f n = callCC $ \done → loop1 done (\fp → f fp done) n
  where loop2 interrupt f' n = do
          n' ← liftIO (readMVar v) >>= \case
            0 → f' interrupt n
            _ → callCC $ \fp → f' fp n
          liftIO $ modifyMVar_ v $ (\k → return $ k-1)
          loop2 interrupt f' n'

        loop1 done f' n = do
          n' ← callCC $ \int → loop2 int f' n
          liftIO $ putStrLn "Measure (m) Abort (a) Continue (c) Run steps (number)"
          (liftIO getLine) >>= \case
            "a" → f' done n' >> return ()
            "c" → liftIO $ setMVar v (-1)
            "m" → liftIO (r n') >>= \case
              Nothing → liftIO exitSuccess
              Just n'' → loop1 done f' n'' >> return ()
            a → case readsPrec 0 a of
                   (n,_):_ → liftIO $ setMVar v n
                   _ → liftIO $ putStrLn "Not understood."
          loop1 done f' n'

main ∷ IO ()
main = do
  (file, n) ← getArgs >>= \case
    [f] → return (f, -1)
    ["-s", n, f] → case readsPrec 0 n of
                    (n',_):_ → return (f, n')
                    _ → error "Argument of -s should be a number"
    _ → error "Insufficient arguments. Expected [-s NUMBER_OF_STEPS] FILE"
  cnt ← newMVar n
  installHandler keyboardSignal (Catch $ setMVar cnt 0) Nothing
  t ← (flip parse) [] <$> readFile file
  void $ (`runContT` r) $ loop cnt eval t
