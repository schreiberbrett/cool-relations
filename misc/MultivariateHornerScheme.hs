import Data.Map
-- A reimplementation of the Multivariate Horner Algorithm into Haskell


-- A polynomial is a sum of monomials

newtype Polynomial a = Polynomial [Monomial a]

-- A monomial is a nonempty product of variables, represented as a bag
newtype Monomial a = Monomial (Map a Int)


-- A multivariate Horner Scheme is of the form xA + B.
data HornerScheme a = HornerScheme {
    factor :: a,
    multiplicand :: Maybe (HornerScheme a),
    summand :: Maybe (HornerScheme a)
} deriving (Eq, Ord, Show)


convert :: Polynomial a -> Maybe (HornerScheme a)
convert ms = undefined
    where
        mostFrequentVariable = maximumBy 
