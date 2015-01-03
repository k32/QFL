{-# LANGUAGE FlexibleContexts #-}
import Data.Functor
import Control.Monad
import Data.List
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Token
import Control.Monad.Random

type Id = String

data SKI = S | K | I | SKIW | App SKI SKI | Var Id deriving (Show, Eq)
data Lambda =  LVar Id | LApp [Lambda] | LW | Abs [Id] Lambda deriving (Show, Eq)
data R = U | W | R :. R deriving (Show)

fv (Var id) = [id]
fv (App e1 e2) = fv e1 ++ fv e2
fv _ = []

freeIn id term = id `elem` fv term

lambda2ski :: Lambda -> SKI
lambda2ski LW = SKIW
lambda2ski (LVar id) = Var id
lambda2ski (LApp [e1, e2]) = App (lambda2ski e1) (lambda2ski e2)
lambda2ski (Abs [id] term) = lamb id $ lambda2ski term

lamb id term | not (id `freeIn` term) = App K term
lamb id (Var id') | id == id' = I
lamb id (App e1 e2) = App (App S (lamb id e1)) (lamb id e2)

ski2iot :: SKI -> R
ski2iot SKIW = W
ski2iot S = U :. (U :. (U :. (U :. U)))
ski2iot K = U :. (U :. (U :. U))
ski2iot I = U :. U
ski2iot (App a b) = ski2iot a :. ski2iot b

l2r = ski2iot . lambda2ski

lDef :: Monad m => GenLanguageDef String s m
lDef = LanguageDef {
   commentStart = "{-"
  ,commentEnd = "-}"
  ,commentLine = "--"
  ,nestedComments = True
  ,identStart = letter <|> char '_'
  ,identLetter = alphaNum <|> char '_'
  ,opStart = undefined
  ,opLetter = undefined
  ,reservedNames = ["io", "def"]
  ,reservedOpNames = []
  ,caseSensitive = True
  }

tp = makeTokenParser lDef
      
parseL :: String -> Lambda
parseL s = case runParser pProg M.empty "" s of
           Left e -> error $ show e
           Right r -> r

mkChurch :: Integer -> Lambda
mkChurch =  Abs ["f", "x"] . f where
  f 0 = LVar "x"
  f n = LApp [LVar "f", f (n - 1)]

pProg = many (try pDefun) >> pLam

pDefun = parens tp $ do
  symbol tp "def"
  i <- lexeme tp $ identifier tp
  d <- lexeme tp $ pLam
  modifyState (M.insert i d)
desugar :: Lambda -> Lambda
desugar (LApp (a:b:t)) | not $ null t = desugar $ LApp $ (LApp [a, b]) : t
desugar (Abs (a:t) x) | not $ null t = Abs [a] $ desugar $ Abs t x
desugar (LApp [a, b]) = LApp [desugar a, desugar b]
desugar (Abs a x) = Abs a $ desugar x
desugar x = x

pLam :: Parsec String (M.Map String Lambda) Lambda
pLam = try pAbs <|>
       try pLiteral <|>
       try (mkChurch <$> natural tp) <|> -- TODO: Move to desugar
       try pW  <|>
       pVar <|>
       pApp <|>
       pFun
  where
    pLiteral = do
      char '"'
      c <- alphaNum
      return $ mkChurch $ fromIntegral $ fromEnum c
    pFun = do
      i <- char '\'' >> identifier tp
      s <- getState
      case M.lookup i s of
       Just x -> return x
       Nothing -> error $ "Unknown id: " ++ i
    pW = symbol tp "io" >> return LW
    pApp = LApp <$> (parens tp $ many1 pLam)
    pAbs = parens tp $ do
      v <- brackets tp $ many1 $ identifier tp
      b <- pLam
      return $ Abs v b
    pVar = LVar <$> identifier tp

transform :: String -> IO String
transform = evalRandIO . pprint . ski2iot . lambda2ski . desugar . parseL

main :: IO ()
main = putStrLn =<< transform =<< getContents

pprint :: MonadRandom m => R -> m String
pprint W = return "bar"
pprint U = pprint1 U
pprint (a :. b) = do
  [r1, r2, r3] <- replicateM 3 j4f
  a' <- pprint1 a
  b' <- pprint2 b
  return $ (intercalate ""  $ filter (not.null) $ r1 ++ [a'] ++ r2 ++ [b']) ++ "~"

j4f :: MonadRandom m => m [String]
j4f = do
  n <- getRandomR (0, 2)
  replicateM n $ fromList [("", 1)]

pprint1 U = fromList [("fu", 20), ("fu\n", 1)]
pprint1 x = pprint x
            
pprint2 U = fromList [("fu", 1)]
pprint2 x = pprint x
