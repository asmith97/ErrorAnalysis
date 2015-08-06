module ErrorAnalysis 
(multError,
exponentError,
experimentalValue,
productErrorList,
errorInProducts,
productList,
Polynomial (Polynomial),
getCoeff,
getPow) where

data Polynomial = Polynomial Float [Float] deriving (Eq, Show)

getCoeff (Polynomial a _) = a
getPow (Polynomial _ b) = b

--All functions compute the FRACTIONAL Error
--Takes the following arguments:
-- Coefficient of the products [List of pruducts (their powers)] [List of sums (their coefficients)] 
--[List of Values] [List of errors]

--For example:
--In x^3 * y^2 * z this function takes in a list of the values of
-- [x^3, y^2, z] and the second argument is [error in x^3, error in y^2, error in z]

multError :: (Floating a) => [a] -> [a] -> a
multError products errors =  sqrt $ sum . map (**2) $ zipWith (/) errors products

exponentError :: (Fractional a) => a -> a -> a -> a
exponentError base power err = (abs power) * err / (abs base)

--Takes in a Polynomial and the values of the variables and returns the result
--Ex. for 3x^3 * y^4 at x = 2, y = 4 it would take in Polynomial 3 [3,4] and [2,4]
--And return 3(2^3)*(4^4) as a float
-- Make sure that decimals (for square roots) are expressed as 0.05 not .05
experimentalValue :: Polynomial -> [Float] -> Float
experimentalValue polynomial values =
    getCoeff polynomial * (product (zipWith (**) values (getPow polynomial)))

--Makes a list of the products
productList :: [Float] -> [Float] -> [Float]
productList powers values = zipWith (**) values powers

{-productList :: Polynomial -> [Float] -> [Float]
productList polynomial values = zipWith (**) (getPow polynomial) values-}

--Makes a list of the sums
sumList sums values = zipWith (*) sums values

--Takes in a list of the product's exponents, the values, the errors
--Outputs a list of the errors in that given exponenet
--Ex. For x^3 * y with x = 4, y = 2 and del x = .01 and del y = .02]
-- It takes in [3,1] and [4,2] and [0.01,0.02] and will output
--[Error in x^3, error in y] (these will not be fractional errors)
--SCRAP WHAT I HAVE ABOVE takes in the result of productList and the errors
--productErrorList (p:ps) (e:es) = exponentError

--returns the uncertainy in all of the exponents that make up the product
--send this to multError to find the total 
productErrorList :: (Fractional t) => [t] -> [t] -> [t] -> [t]
productErrorList [] [] [] = []
productErrorList [] _ _ = error "Mismatch"
productErrorList _ [] _ = error "Mismatch"
productErrorList _ _ [] = error "Mismatch"
productErrorList (p:ps) (v:vs) (e:es) = exponentError v p e : productErrorList ps vs es

errorInProducts :: [Float] -> [Float] -> [Float] -> Float
errorInProducts [] values errors = 0
errorInProducts powers values errors = multError (productList powers values) (productErrorList powers values errors)

{-
errorInProducts :: Polynomial -> [Float] -> [Float] -> [Float]
errorInProducts polynomial values errors =
    multError (productList polynomial values) (productErrorList (getPow polynomial) values errors)

-}

{-Want to input: 3(x^2)(y^3) + 3xz^2 + 4yx + 4x with errors in x, y, z
Break this into small chunks which will be added
3 [2,3,0] + 3 [1,0,2] + 4 [1,1,0] + 4 [1,0,0] with values for [x,y,z] and errors for [x,y,z]
First Prompt: For the expressoin
Second Promt: For the values and the errors

Output: The experimental value
Output: The fractional uncertainty
Output: The error range

Functions:
errorInProducts - calling multError with the arguments

-}






