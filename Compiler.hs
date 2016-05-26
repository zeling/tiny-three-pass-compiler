{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}
module TinyThreePassCompiler where

import Control.Applicative
import Control.Monad.State
import qualified Data.Map as M
import Data.Data.Lens
import Control.Lens.Plated
import Data.Data
import Data.Maybe (fromJust)
import Data.List (foldl')

newtype Parser tok a = P { unP :: [tok] -> Maybe (a, [tok]) } 
  deriving (Functor)

parse :: Parser tok a -> [tok] -> Maybe a
parse p toks = case unP p toks of
                 Just (a, []) -> Just a
                 _ -> Nothing

instance Applicative (Parser tok) where
  pure a = P $ \toks -> Just (a, toks)
  p1 <*> p2 = P $ \toks -> case unP p1 toks of
                             Nothing -> Nothing
                             Just (f, rest) -> case unP p2 rest of
                                                 Nothing -> Nothing
                                                 Just (a, rest') -> Just (f a, rest')


instance Monad (Parser tok) where
  return = pure
  p >>= f = P $ \toks -> case unP p toks of
                           Nothing -> Nothing
                           Just (a, rest) -> unP (f a) rest

instance Alternative (Parser tok) where
  empty = P $ const Nothing
  p1 <|> p2 = P $ (<|>) <$> (unP p1) <*> (unP p2)

instance MonadPlus (Parser tok) where
  mzero = empty
  mplus = (<|>)

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show, Data, Typeable)

instance Plated AST where
  plate = uniplate

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

data Scope = Scope {
  bound :: M.Map String Int,
  fresh :: Int
} deriving (Eq, Show)

type P = StateT Scope (Parser Token)

initial :: Scope
initial = Scope M.empty 0

parseLanguage :: String -> Maybe AST
parseLanguage str = parse (evalStateT function initial) $ tokenize str

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

tok :: Token -> P Token
tok t = StateT $ g
  where g scope = P $ f
                where f (t':ts) | t == t' = Just ((t', scope), ts)
                                | otherwise = Nothing
                      f _ = Nothing

punc :: String -> P Char
punc str = StateT $ g
  where g scope = P $ f
                where f ((TChar c):ts) = if c `elem` str 
                                         then Just ((c, scope), ts)
                                         else Nothing
                      f _ = Nothing

identifier :: P String
identifier = StateT $ g
  where g scope = P $ f
                where f ((TStr str):ts) = Just ((str, scope), ts)
                      f _ = Nothing

integer :: P Int
integer = StateT $ g
  where g scope = P $ f
                where f ((TInt int):ts) = Just ((int, scope), ts)
                      f _ = Nothing


function :: P AST
function = do
  tok $ TChar '['
  args <- arglist
  tok $ TChar ']'
  expression
  

arglist :: P ()
arglist = many arg *> pure ()

arg :: P ()
arg = do 
  ident <- identifier
  modify $ \scope -> Scope {
    bound = M.insert ident (fresh scope) (bound scope),
    fresh = (fresh scope) + 1
  }
  

variable :: P AST
variable = do
  ident <- identifier
  scope <- get
  let num = M.lookup ident (bound scope)
  case num of
    (Just id) -> return (Arg id)
    Nothing -> empty

number :: P AST
number = Imm <$> integer

expression :: P AST
expression = do
  lhs <- term
  foldl' step lhs <$> (many $ (,) <$> (punc "+-") <*> term)
           where step l ('+',r) = Add l r
                 step l ('-',r) = Sub l r

term :: P AST
term = do
  lhs <- factor
  foldl' step lhs <$> (many $ (,) <$> (punc "*/") <*> factor)
           where step l ('*',r) = Mul l r
                 step l ('/',r) = Div l r

factor :: P AST
factor = number 
     <|> variable 
     <|> tok (TChar '(') *> expression <* tok (TChar ')')

pass1 :: String -> AST
pass1 = fromJust . parseLanguage

rules :: AST -> AST
rules (Add (Imm a) (Imm b)) = Imm (a + b)
rules (Sub (Imm a) (Imm b)) = Imm (a - b)
rules (Mul (Imm a) (Imm b)) = Imm (a * b)
rules (Div (Imm a) (Imm b)) = Imm (a `div` b)
-- rules (Add (Imm a) (Add (Imm b) r)) = Add (Imm (a + b)) r
-- rules (Add (Imm a) (Add l (Imm b))) = Add l (Imm (a + b))
-- rules (Add (Add (Imm a) r) (Imm b)) = Add (Imm (a + b)) r
rules ast                   = ast

pass2 :: AST -> AST
pass2 = transform rules

-- emit instructions with no optimization
pass3 :: AST -> [String]
pass3 (Imm a) = ["IM " ++ show a]
pass3 (Arg a) = ["AR " ++ show a]
pass3 (Add lhs rhs) = pass3 lhs ++ ["PU"] ++ pass3 rhs ++ ["SW", "PO"] ++ ["AD"]
pass3 (Sub lhs rhs) = pass3 lhs ++ ["PU"] ++ pass3 rhs ++ ["SW", "PO"] ++ ["SU"]
pass3 (Mul lhs rhs) = pass3 lhs ++ ["PU"] ++ pass3 rhs ++ ["SW", "PO"] ++ ["MU"]
pass3 (Div lhs rhs) = pass3 lhs ++ ["PU"] ++ pass3 rhs ++ ["SW", "PO"] ++ ["DI"]

compile :: String -> [String]
compile = pass3 . pass2 . pass1


simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl' step (0, 0, []) asm where
  step (r0,r1,stack) ins =
    case ins of
      ('I':'M':xs) -> (read xs, r1, stack)
      ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
      "SW" -> (r1, r0, stack)
      "PU" -> (r0, r1, r0:stack)
      "PO" -> (head stack, r1, tail stack)
      "AD" -> (r0 + r1, r1, stack)
      "SU" -> (r0 - r1, r1, stack)
      "MU" -> (r0 * r1, r1, stack)
      "DI" -> (r0 `div` r1, r1, stack)
  takeR0 (r0, _, _) = r0


