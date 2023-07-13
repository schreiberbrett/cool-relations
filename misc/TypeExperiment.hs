-- Given the types
data List a = Nil | Cons a
data Either a b = Left a | Right b

-- Write the type isomorphic to List (Either a b)

data List (Either a b)
	= Nil | Cons (Either a b) (List (Either a b))
	= Nil | Cons (Left a | Right b) (List (Either a b))
-- Stop expanding at this step, when it has exactly one recursive call


