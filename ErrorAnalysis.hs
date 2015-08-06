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

productList :: [Float] -> [Float] -> [Float]
productList powers values = zipWith (**) values powers

productErrorList :: (Fractional t) => [t] -> [t] -> [t] -> [t]
productErrorList [] [] [] = []
productErrorList [] _ _ = error "Mismatch"
productErrorList _ [] _ = error "Mismatch"
productErrorList _ _ [] = error "Mismatch"
productErrorList (p:ps) (v:vs) (e:es) = exponentError v p e : productErrorList ps vs es

errorInProducts :: [Float] -> [Float] -> [Float] -> Float
errorInProducts [] values errors = 0
errorInProducts powers values errors = multError (productList powers values) (productErrorList powers values errors)






