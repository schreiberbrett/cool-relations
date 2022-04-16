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
