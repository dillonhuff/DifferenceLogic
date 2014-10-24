module DifferenceLogic.Solver(
  zF,
  zLit,
  eq, gt, lt, geq, leq,
  consistentOverZ) where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.List as L
import Data.Map as M
import Data.Maybe
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

normalizeLiteral :: ZLiteral -> [ZLiteral]
normalizeLiteral (ZLiteral l r Eq c) = [zLit l r leq c, zLit l r gt (c - 1)]
normalizeLiteral (ZLiteral l r Lt c) = [zLit l r leq (c - 1)]
normalizeLiteral (ZLiteral l r Geq c) = [zLit l r gt (c - 1)]
normalizeLiteral l = [l]

literalNames :: ZLiteral -> [String]
literalNames (ZLiteral l r _ _) = [l, r]

buildLiteralEdge :: Map String Node -> ZLiteral -> LEdge Int
buildLiteralEdge namesToNodes (ZLiteral l r Leq c) = (lNode, rNode, c)
  where
    lNode = fromJust $ M.lookup l namesToNodes
    rNode = fromJust $ M.lookup r namesToNodes
buildLiteralEdge namesToNodes (ZLiteral l r Gt c) = (rNode, lNode, c)
  where
    lNode = fromJust $ M.lookup l namesToNodes
    rNode = fromJust $ M.lookup r namesToNodes
buildLiteralEdge _ l = error $ "Trying to build literal from " ++ show l

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

consistentOverZ :: ZFormula -> Bool
consistentOverZ formula = not $ containsNegCycle formulaGraph
  where
    normedForm = normalizeFormula formula
    nodeList = buildNodeList normedForm
    litNameNodeMap = M.fromList $ L.map (\(n, s) -> (s, n)) nodeList
    edgeList = buildEdgeList normedForm litNameNodeMap
    formulaGraph = mkGraph nodeList edgeList

buildNodeList :: ZFormula -> [(Node, String)]
buildNodeList (ZFormula lits) = L.zip [1..(length litNames)] litNames
  where
    litNames = L.nub $ L.concat $ L.map literalNames $ S.toList lits

buildEdgeList :: ZFormula -> Map String Node -> [LEdge Int]
buildEdgeList (ZFormula lits) namesToNodes = edgeList
  where
    edgeList = S.toList $ S.map (buildLiteralEdge namesToNodes) lits

normalizeFormula :: ZFormula -> ZFormula
normalizeFormula (ZFormula lits) = ZFormula $ S.fromList normedLits
  where
    normedLits = L.concat $ L.map normalizeLiteral $ S.toList lits

containsNegCycle :: Gr String Int -> Bool
containsNegCycle formulaGraph = case isEmpty formulaGraph of
  True -> False
  False -> negCycleFrom headNode graphNodes graphEdges
  where
    graphNodes = labNodes formulaGraph
    graphEdges = labEdges formulaGraph
    headNode = head graphNodes

negCycleFrom :: LNode String -> [LNode String] -> [LEdge Int] -> Bool
negCycleFrom source nodes edges = checkNegCycle edges weights
  where
    initialWeights = M.fromList $ L.zip nodes $ L.map (initWeight source) nodes
    weights = computeWeights nodes edges initialWeights 0

initWeight :: LNode String -> LNode String -> Int
initWeight source node = case source == node of
  True -> 0
  False -> maxBound :: Int

computeWeights :: [LNode String] ->
                  [LEdge Int] ->
                  Map (LNode String) Int ->
                  Int ->
                  Map (LNode String) Int
computeWeights nodes edges weights i = weights

checkNegCycle :: [LEdge Int] -> Map (LNode String) Int -> Bool
checkNegCycle edges weights = False
