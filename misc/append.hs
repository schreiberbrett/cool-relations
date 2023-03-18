{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Avoid lambda" #-}
append :: [a] -> [a] -> [a]
append [] ys     = ys
append (x:xs) ys = output
	where
		theRestOfTheOutput = (append xs ys)
		output = x:theRestOfTheOutput


{-

If the first input is nonempty:
	- The REST of the output is a list (that could be empty) that is the result of appending two things: the REST of the first input and the second input
	- The output must be a cons
	- Then the first input must be a cons (and must have a first element)
	- The output has a FIRST and REST
	- The FIRST element in the output is exactly equal to the FIRST element in the first input
	
If the first input is empty, then the output is exactly equal to the second input.

A && B 
B && A (commutativity of AND)
-}

-- map (+10) xs

algorithm :: [Clause] -> Maybe ([Clause], Relation, [Clause])
algorithm clauses =
	if (numberOfOccurences mostFrequentRelation clauses) == 1
	then Nothing
	else Just $ ((map (\clause -> removeRelation mostFrequentRelation clause) incl), mostFrequentRelation, excl) 

		where
			relations :: [Relation]
			relations = uniqueRelations clauses

			mostFrequentRelation :: Relation
			mostFrequentRelation = maxBy relations (\relation -> numberOfOccurences relation clauses)

			pair :: ([Clause], [Clause])
			pair = partition clauses (\clause -> containsRelation mostFrequentRelation clause)

			incl :: [Clause]
			incl = fst pair

			excl :: [Clause]
			excl = snd pair


numberOfOccurences :: Relation -> [Clause] -> Int
numberOfOccurences relation clauses = length $ filter (\clause -> containsRelation relation clause) clauses

unique [] = []
unique (h:t) = if contains t h then unique t else h:(unique t)

uniqueRelations :: [Clause] -> [Relation]
uniqueRelations clauses = unique $ appendStar (map (\x -> map fst x) clauses)

appendStar [] = []
appendStar (h:t) = h ++ (appendStar t)

type Relation = String
type Clause = [(Relation, [String])]

containsRelation :: Relation -> Clause -> Bool
containsRelation relation clause = contains (map fst clause) relation

removeRelation :: Relation -> Clause -> Clause
removeRelation relation [] = []
removeRelation relation ((r, x): t) = if r == relation then t else (r, x):(removeRelation relation t)

contains :: (Eq a) => [a] -> a -> Bool
contains [] _ = False
contains (h:t) x = if x == h then True else contains t x

partition :: [a] -> (a -> Bool) -> ([a], [a])
partition [] predicate = ([], [])
partition (h:t) predicate = if (predicate h) then (h:yes, no) else (yes, h:no)
	where
		(yes, no) = partition t predicate


maxBy :: [a] -> (a -> Int) -> a
maxBy [x] f = x
maxBy (h:t) f = let m = maxBy t f in if ((>=) (f h) (f m)) then h else m

sampleClauses :: [Clause]
sampleClauses = [
	[("primeo", ["x"]), ("==", ["primes", "x"])],
	[
		("*o", ["a", "b", "x"]),
		("primeo", ["a"]),
		("fundamental-theorem-of-arithmetico", ["b" , "rest-primes"]),
		("==", ["primes", "(a . rest-primes)"])]]


data Defrel = Defrel String [String] [Exp]

data Exp = Fresh [String] [Exp] | Conde [[Exp]] | Rel String [String]


containsRelation2 :: String -> [Exp] -> Bool
containsRelation2 sym [] = False
containsRelation2 sym (exp:exps) = case exp of
	Rel y ys -> sym == y || containsRelation2 sym exps
	_ -> containsRelation2 sym exps

frequency :: String -> [[Exp]] -> Int
frequency sym conde = length $ filter (\clause -> containsRelation2 sym clause) conde


allRelations :: [[Exp]] -> [String]
allRelations expss = concatMap (\exps -> concatMap
	(\exp -> case exp of
			Rel x _ -> [x]
			Fresh _ _-> []
			Conde _ -> []) exps
		) expss