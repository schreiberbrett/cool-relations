{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

newtype And a = And [Either a (Or a)]
newtype Or a = Or [And a]


newtype Band a = Band [Either a [Band a]]

data Band3 a = Band3 [a] [[Band3 a]]


troll2 = Band3 ['h'] [
    [Band3 ['a', 'b'] [], Band3 ['c'] []],
    [Band3 ['f'] [[Band3 ['d'] [], Band3 ['e'] []]]],
    [Band3 ['g'] []]
    ]


toDNFBand3 :: Band3 a -> [[a]]
toDNFBand3 (Band3 atoms disjs) = let
    convertedDisjs = map (\x -> concatMap (\y -> toDNFBand3 y) x) disjs
    result = map (\z -> atoms ++ z) (xprodAllGPT convertedDisjs)
    in result


toDNF :: And a -> [[a]]
toDNF (And []) = [[]]
toDNF (And (x:xs)) = case x of
    (Left a) -> map (\disjunct -> a:disjunct) $ toDNF (And xs)
    (Right (Or ands)) -> [a ++ b | a <- concatMap toDNF ands, b <- toDNF (And xs)]


brett = toDNF (And [Right (Or [And [Left 'a'], And [Left 'b']]), Left 'c'])


troll = And [
    Right (Or [And [Left 'a', Left 'b'], And [Left 'c']]),
    Right (Or [And [Right (Or [And [Left 'd'], And [Left 'e']]),  Left 'f']]),
    Right (Or [And [Left 'g']]),
    Left 'h'
    ]

x = [['a', 'a'], ['b', 'b']]
y = [['c', 'c'], ['d', 'd'], ['e']]

result = [a ++ b | a <- x, b <- y] -- = ["aacc", "aadd", "aae", "bbcc", "bbdd", "bbe"]

type Conj = [Exp]

data Exp = Fresh Conj | Conde [Conj] | Eq | Rel

-- to disjunction

f :: [Exp] -> [[Exp]]
f exps = let
    x = concatMap (\exp -> g exp) exps
    y = xprodAll undefined
    in y

g :: Exp -> [[Exp]]
g (Fresh exps) = f exps
g (Conde expss) = expss
g Eq = [[Eq]]
g Rel = [[Rel]]


xprodAll :: [[[a]]] -> [[a]]
xprodAll [] = undefined
xprodAll [x] = x
xprodAll (x:xs) = xprod2 x (xprodAll xs)


xprod2 :: [[a]] -> [[a]] -> [[a]]
xprod2 xs ys = [x ++ y | x <- xs, y <- ys]


type DNF a = [[a]]


-- new code given by chatGPT
xprodAllGPT :: {- A conjunction of DNFs -} [DNF a] -> DNF a
xprodAllGPT = foldr (\a b -> [x ++ y | x <- a, y <- b]) [[]]
