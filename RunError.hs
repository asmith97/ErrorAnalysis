import ErrorAnalysis as E
import Data.Char

main = do
    putStrLn "Input the expression: (for a*x^3*y^2 + bz input 3 [2,2,0] 1 [0,0,1]) "
    expressionString <- getLine
    putStrLn "Input the Values: (of the form [x,y,z]"
    valueString <- getLine
    putStrLn "Input the Errors: (of the form [Δx, Δy, Δz] "
    errorString <- getLine

    let parsedExpression = parseExpression expressionString

    let values = read valueString :: [Float]
    let errors = read errorString :: [Float]

    --need to make the values a list of a list of floats so that they can be zipped with the list of polynomials
    let expValue = zipWith E.experimentalValue parsedExpression (repeat values)
    putStrLn $ " "
    putStrLn $ "The experimental value is: " ++ show (sum expValue)
    putStrLn $ " "

    let errorList = zipWith3 E.errorInProducts (map E.getPow parsedExpression) (repeat values) (repeat errors)

    let totalFractionalError = sum errorList
    putStrLn $ "The fractional uncertainty is: " ++ (show totalFractionalError)
    putStrLn $ " "

    let totalError = totalFractionalError * (sum expValue)
    putStrLn $ "The total error is: " ++ (show totalError)
    putStrLn $ " "
    --Check to see if this is actually the fractional error or the total or something else?
    putStrLn $ "Accounting for uncertainty the value should lie between: " ++ show [(sum expValue)-totalError,(sum expValue)+totalError]
    putStrLn $ " "

    putStrLn "Would you like to keep running? (y/n) "
    response <- getLine
    if response == "n" then putStrLn "If you enjoyed using ErrorAnalysis by Smith, please consider donating" else main


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


checkForNum = and . map isNumber






