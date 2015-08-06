import System.Environment
import Data.List
import ErrorAnalysis as E

main = do
    putStrLn "Input the expression: "
    expressionString <- getLine
    putStrLn "Input the Values: "
    valueString <- getLine
    putStrLn "Input the Errors: "
    errorString <- getLine
    let parsedExpression = parseExpression expressionString
    let values = read valueString :: [Float]
    let errors = read errorString :: [Float]
    --need to make the values a list of a list of floats so that they can be zipped with the list of polynomials
    let expValue = zipWith E.experimentalValue parsedExpression (repeat values)
    putStr "The experimental value is: "
    print $ sum expValue
    print parsedExpression

    --this leads to a mismatch error - not sure why
    let errorList = zipWith3 E.errorInProducts (map E.getPow parsedExpression) (repeat values) (repeat errors)
    print $ E.errorInProducts (E.getPow $ parsedExpression !! 0) values errors
    print $ E.errorInProducts (E.getPow $ parsedExpression !! 1) values errors
    print errorList

--takes from the string until getting to the first opening bracket
getCoefficient :: String -> Float
getCoefficient a = read $ takeWhile (/= '[') a :: Float


--Removes the portions of the string before the opening braket and
--takes from the string until we get to the space after the ending bracked
getPowers :: String -> [Float]
getPowers a = read (takeWhile (/= ' ') (dropWhile (/= '[') a)) :: [Float]


--"3 [3,1,5] 1 [0,0,2]" is a sample input 
-- returns this expressed as a list of polynomials
parseExpression :: String -> [E.Polynomial]
parseExpression "" = [E.Polynomial 0 []]
parseExpression a = E.Polynomial (getCoefficient a) (getPowers a) : parseExpression (dropWhile (/= ' ') (dropWhile (/= ']') a))








