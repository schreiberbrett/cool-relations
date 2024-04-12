-- A file for describing miniKanren syntax and performing static analysis.

import Data.Map (Map)
import qualified Data.Map as Map

data Defrel = Defrel String [String] Conj

data Conj = Conj [Syntax]

data Syntax
	= Fresh [String] Conj
	| Conde [Conj]
	| Relate String [L1]

data L1
	= Var String
	| Atom String
	| Cons L1 L1
	| Nil

substitute :: Defrel -> [L1] -> Conj
substitute (Defrel _ xs conj) vals = go xs vals conj where
	go [] [] c = c
	go (x:xs) (val:vals) c = go xs vals $ subConj x val c


subConj :: String -> L1 -> Conj -> Conj
subConj x val (Conj c) =
	Conj $ map (subSyntax x val) c

subSyntax :: String -> L1 -> Syntax -> Syntax
subSyntax x val syntax = case syntax of
	(Fresh vs conj) -> Fresh vs $ subConj x val conj
	(Conde conjs) -> Conde $ map (subConj x val) conjs
	(Relate r args) -> Relate r $ map (subL1 x val) args

subL1 :: String -> L1 -> L1 -> L1
subL1 x val l1 = case l1 of
	(Var str) -> if x == str then val else l1
	(Cons a d) -> Cons (subL1 x val a) (subL1 x val d)
	_ -> l1

