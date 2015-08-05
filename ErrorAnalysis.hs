module ErrorAnalysis 
(multError,
exponentError,
experimentalValue,
productErrorList,
errorInProducts,
productList) where
--All functions compute the FRACTIONAL Error
--Takes the following arguments:
-- Coefficient of the products [List of pruducts (their powers)] [List of sums (their coefficients)] 
--[List of Values] [List of errors]

--For example:
--In x^3 * y^2 * z this function takes in a list of the values of
-- [x^3, y^2, z] and the second argument is [error in x^3, error in y^2, error in z]
multError :: (Floating a) => [a] -> [a] -> a
multError products errors =  sqrt $ sum . map (^2) $ zipWith (/) errors products

exponentError :: (Fractional a) => a -> a -> a -> a
exponentError base power err = (abs power) * err / (abs base)

--Takes the following arguments:
-- Coefficient of the products [List of pruducts (their powers)] [List of sums (their coefficients)] 
--[List of Values]
--put negative exponents for division, change the Integral requirement to allow square roots and such
experimentalValue :: (Num a, Integral b) => a -> [b] -> [a] -> [a] -> a
experimentalValue coeff powers sums values = 
	(coeff * product (zipWith (^) values powers)) + (sum $ zipWith (*) sums values)

--Makes a list of the products
productList :: (Num c, Integral b) => [b] -> [c] -> [c]
productList powers values = zipWith (^) values powers

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

errorInProducts :: (Integral a, Floating a) => [a] -> [a] -> [a] -> a
errorInProducts powers values errors = multError (productList powers values) (productErrorList powers values errors)


{-errorInSums :: (Integral a, Floating a) => [a] -> [a] -> [a] -> a
errorInSums sums values errors = sumList sums values-}

--c1 + c2x + c3x^2 + c4x^3 + ... [c1,c2,c3,c4..]
{-data Polynomial = Polynomial [Int] deriving (Eq)

add a b = zipWith (+) a b-}

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






