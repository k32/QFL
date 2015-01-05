{- Quantum Fubar Language -}
{-# LANGUAGE GADTs, StandaloneDeriving, UnicodeSyntax, KindSignatures,
             FlexibleInstances, LambdaCase #-}

module Main (main) where

import Data.Functor
import Data.List
import Control.Monad
import Control.Monad.Writer
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

eval ∷ MVar Int → T → IO T
eval i t = takeMVar i >>= \case
  0 → return t
  i' | t == t' → return t
     | otherwise → putMVar i (i'-1) >> eval i t'
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

loop ∷ MVar Int → T → IO ()
loop c t = do
  putStrLn $ "Waiting for decision: Abort (a), Perform measurement (m)," ++
             " Continue (c), Run steps (number)"
  getLine >>= \case
   "a" → return ()
   "m" → r t >>= \case
     Just t'' → loop c t''
     Nothing → return ()
   "c" → setMVar c (-1) >> eval c t >>= loop c
   a → case readsPrec 0 a of
        (n,_):_ → setMVar c n >> eval c t >>= loop c
        _ → putStrLn "Not understood." >> loop c t

main ∷ IO ()
main = do
  cnt ← newEmptyMVar
  installHandler keyboardSignal (Catch $ setMVar cnt (-1)) Nothing
  t ← parse <$> (readFile =<< head <$> getArgs)
  loop cnt t
