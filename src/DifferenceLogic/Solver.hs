module DifferenceLogic.Solver(
  zF,
  zLit,
  eq, gt, lt, geq, leq,
  satisfiableOverZ) where

import Data.List as L
import Data.Set as S

-- This solver solves conjuctions of literals
data ZFormula = ZFormula (Set ZLiteral)
                deriving (Eq, Ord, Show)

zF :: [ZLiteral] -> ZFormula
zF litList = ZFormula $ S.fromList litList

data ZLiteral = ZLiteral {
  left :: String,
  right :: String,
  predicate :: Predicate,
  const :: Int
  } deriving (Eq, Ord, Show)

zLit = ZLiteral

data Predicate
  = Eq
  | Lt
  | Gt
  | Leq
  | Geq
    deriving (Eq, Ord, Show)

eq = Eq
lt = Lt
gt = Gt
leq = Leq
geq = Geq

satisfiableOverZ :: ZFormula -> Bool
satisfiableOverZ formula = False
