import System.Environment
import Data.List
import ErrorAnalysis as E

{-main = do
	argsString <- getArgs
	--To input 2*(3^4)*(5^6) + (9*3)(8*5) 
	-- Or 2(x^4)(y^6) + 9x8y for (x,y) = (3,5) with error in x of .04 and in y of .05
	-- Input: 2 [4,6] [9,3] [3,5] [.04,.05]
	let coeff = read (argsString !! 0) :: Float
	let powers = read (argsString !! 1) :: [Integer]
	let sums = read (argsString !! 2) :: [Float]
	let values = read (argsString !! 3) :: [Float]
	let errors = read (argsString !! 4) :: [Float]


	let a = E.experimentalValue coeff powers sums values
	let b = E.multError (productList powers values) (productErrorList (map  fromIntegral powers) values errors)
	putStrLn $ "The experimental value is " ++ show a
	putStrLn $ "The percent error in the product is " ++ show b
	putStrLn $ "Using given uncertainties, the product lies within " ++ show [a-b,a+b]

--Requires a major rewrite!!!!!-}

main = do
	putStrLn "Input the expression: "
	expression <- getLine
	putStrLn "Input the Values: "
	values <- getLine
	putStrLn "Input the Errors: "
	errors <- getLine
	let parsedExpression = parseExpression expression
	let coeff = map getCoeff parsedExpression
	let powers = map getPow parsedExpression
	let a = zipWith E.experimentalValue coeff powers
	print 4


--Move this to the other file
data Polynomial = Polynomial Int [Int] deriving (Eq, Show)

getCoeff (Polynomial a _) = a
getPow (Polynomial _ b) = b


getCoefficient :: String -> Int
getCoefficient a = read $ takeWhile (/= '[') a :: Int

getPowers :: String -> [Int]
getPowers a = read (takeWhile (/= ' ') (dropWhile (/= '[') a)) :: [Int]


--"3 [3,1,5] 1 [0,0,2]" is a sample input
parseExpression :: String -> [Polynomial]
parseExpression "" = [Polynomial 0 []]
parseExpression a = Polynomial (getCoefficient a) (getPowers a) : parseExpression (dropWhile (/= ' ') (dropWhile (/= ']') a))








