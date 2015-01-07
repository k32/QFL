{- Quantum Fubar Language -}
{-# LANGUAGE GADTs, StandaloneDeriving, UnicodeSyntax, KindSignatures,
             FlexibleInstances, LambdaCase #-}

import Data.Functor
import Control.Monad.IO.Class
import Control.Monad.Trans.Cont
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
                 ;F ∷ (Int → Int) → T; N ∷ Int → T; Ø ∷ String → T}
deriving instance Show T
deriving instance Eq T

parse ∷ String → T
parse = flip parse2 []

parse2 ∷ String → [T] → T
parse2 ('f':'u':c) t = parse2 c (J:t)
parse2 ('b':'a':'r':c) t = parse2 c (Â:t)
parse2 ('~':c) (a:b:t) = parse2 c (b:.a:t)
parse2 ('~':_) _ = error "Parse error: missing operand(s)"
parse2 (_:c) t = parse2 c t
parse2 [] (h:_) = h :. Ψ []
parse2 [] [] = error "Parse error: empty program"

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

r' ∷ T → [T]
r' ψ@(Ψ {}) = [ψ]
r' t@(_ :. Ψ {}) = [t]
r' (a :. b) = r' a ++ r' b
r' _ = []

r ∷ T → IO (Maybe T)
r t = let t' = r' t in
  ((t' !!) <$> randomRIO (0, length t' - 1)) >>= \case
    Ψ{σ = s}      → putStrLn (reverse s) >> return Nothing
    t :. Ψ{σ = s} → putStrLn (reverse s) >> return (Just t)

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
            a → case readsPrec 0 a of
                   (n,_):_ → liftIO $ setMVar v n
                   _ → liftIO $ putStrLn "Not understood."
          loop1 done f' n'

main ∷ IO ()
main = do
  cnt ← newMVar $ -1
  installHandler keyboardSignal (Catch $ setMVar cnt (-1)) Nothing
  t ← parse <$> (readFile =<< head <$> getArgs)
  (r =<<) $ evalContT $ loop cnt eval t
  return ()
