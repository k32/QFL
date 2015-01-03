{- Wchan Fubar Compiler -}
{-# LANGUAGE GADTs, StandaloneDeriving, UnicodeSyntax, KindSignatures,
             FlexibleInstances, FlexibleContexts #-}

-- TODO: Ability to continue execution after collapse

module Main (main) where

import Data.Functor
import Data.List
import Control.Monad
import Control.Monad.Writer
import System.Random
import System.Environment

instance Show (Int → Int) where
  show _ = "<function>"
instance Eq (Int → Int) where
  _ == _ = True -- It's a hack

infixl 7 :.

data T ∷ * where {J, Â, Â', S, K ∷ T; (:.) ∷ T → T → T; Σ ∷ {σ ∷ String} → T
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
parse2 [] (h:_) = h :. Σ []
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
s (Â' :. N i :. ψ @ (Σ {})) = ψ {σ = toEnum i : σ ψ}
-- Using Â → Â' to indicate that Church → Int conversion has been started
s (Â :. n :. ψ @ (Σ {})) = Â' :. (n :. F (+1) :. N 0) :. ψ
-- Other cases
s (a :. b) = (s a) :. (s b)
s x = x

eval ∷ T → T
eval t | t == t' = t
       | otherwise = eval t'
  where t' = s t

r' ∷ T → [String]
r' (Σ {σ = s}) = [reverse s]
r' (a :. b) = r' a ++ r' b
r' _ = []

r ∷ T → IO ()
r t = putStrLn =<< (t' !!) <$> randomRIO (0, length t' - 1)
  where t' = r' t

main ∷ IO ()
main = r =<< eval . parse <$> (readFile =<< head <$> getArgs)
