module Poly where

import Data.List as D
import Data.List.Extra 
import Data.Ratio
import Data.Composition
import Data.List.Ordered

type Coeff = Rational
type Exponent = Integer 

-- data Monomial = Mono Coeff Exponent Constant -- Mono n p c = n⋅xᵖ + c 
type Term = (Coeff, Exponent)
data Polynomial = PolySum [Term] -- PolySum [(n₁,2), (n₂, 1), (n₃, 0)] = n₁⋅x² + n₂⋅x + n₃
                | PolyFact Polynomial Polynomial -- PolyFact n⋅xᵖ+c P = (n⋅xᵖ + c)⋅P
                -- deriving (Eq)

showExp :: Term -> String 
showExp (co, ex) 
    | ex == 0 = showCoeff co 
    | ex == 1 && co == 1 = "x"
    | ex == 1 = showCoeff co ++ "x"
    | otherwise = showCoeff co ++ "x^" ++ show ex

showCoeff :: Coeff -> String 
showCoeff co | denominator co == 1 = show $ numerator co 
             | otherwise = show co

instance Show Polynomial where
    show (PolySum []) = "0"
    show (PolySum xs) = "(" ++ (intercalate " + " $ map showExp xs) ++ ")"
    show (PolyFact a b) = show a ++ show b 

canonicalise :: Polynomial -> Polynomial
canonicalise = normalise . PolySum . asSumForm 


-- normalise (PolyFact xs) = PolyFact $ map normalise xs

normalise :: Polynomial -> Polynomial
normalise (PolyFact a b) = PolyFact (normalise a) (normalise b)
normalise (PolySum xs) = if isNormalForm xs then 
        PolySum xs 
    else 
        PolySum $ reverse $ filter ((/=) 0 . fst) $ map combine $ groupSortOn snd xs
    where 
        combine :: [Term] -> Term
        combine xs = mapBoth sum head $ unzip xs

isNormalForm :: [Term] -> Bool 
isNormalForm = isSortedBy (\a b -> snd a > snd b) 

asSumForm :: Polynomial -> [Term] 
asSumForm (PolySum a) = a 
asSumForm (PolyFact a b) = concatMap (multiply' b') a' 
    where  
        a' = asSumForm a 
        b' = asSumForm b 
        multiply' :: [Term] -> Term -> [Term]    
        multiply' ys (co,ex) = map (mapBoth ((*) co) ((+) ex)) ys

add :: Polynomial -> Polynomial -> Polynomial 
add (PolySum a) (PolySum b) = PolySum $ a ++ b
add a b = PolySum ((asSumForm a) ++ (asSumForm b))

multiply :: Polynomial -> Polynomial -> Polynomial 
multiply a b = PolyFact a b


asPolySumList :: Polynomial -> [[Term]]
asPolySumList (PolySum a) = [a]
asPolySumList (PolyFact a b) = asPolySumList a ++ asPolySumList b

asPoly :: [[Term]] -> Polynomial
asPoly [] = PolySum []
asPoly (a : []) = PolySum a 
asPoly (a : as) = PolyFact (PolySum a) (asPoly as) 


divide :: Polynomial -> Polynomial -> (Polynomial, Polynomial)  
divide a b | degree b_norm == -1 = error "Division by zero"
           | otherwise = case mapBoth asPoly asPoly $ removeCommon (asPolySumList a_norm) (asPolySumList b_norm) of 
                        (a, PolySum []) -> (a, PolySum [])
                        (a, b) -> divide' (canonicalise a) (canonicalise b)
    where 
        a_norm = normalise a
        b_norm = normalise b

-- We assume that the polynomials are in canonical form.
divide' :: Polynomial -> Polynomial -> (Polynomial, Polynomial) 
divide' a b 
    | degree a < degree b = (PolySum [], a)
    | otherwise = let quot_term = appBoth (mapBoth (/) (-) (largestTerm a)) (largestTerm b) in
                  let a' = canonicalise (add a (neg (multiply b (PolySum [quot_term])))) in 
                  let (PolySum quot_rest, r) = divide' a' b in 
                  (PolySum (quot_term : quot_rest), r)
    where 
        largestTerm :: Polynomial -> Term 
        largestTerm (PolySum xs) = head xs

degree :: Polynomial -> Exponent 
degree (PolySum []) = -1 --error "Undefined degree of zero polynomial"
degree (PolySum xs) = snd $ head xs

--         a/bx^(n-m)
--          ---------------
-- bxᵐ + .. | axⁿ + ...
--            (bxᵐ + ..)*a/bx^(n-m)

            -- (axⁿ + ...) - (bxᵐ + ..)*a/bx^(n-m) = ??? 

            -- ??? / bxᵐ + ..

modulo :: Polynomial -> Polynomial -> Polynomial 
modulo = snd .: divide 

div_ :: Polynomial -> Polynomial -> Polynomial 
div_ = fst .: divide

gcd_ :: Polynomial -> Polynomial -> Polynomial
gcd_ a b = if isZero b then a else gcd_ b (modulo a b)
    where 
        isZero (PolySum []) = True
        isZero _ = False


neg :: Polynomial -> Polynomial
neg (PolySum xs) = PolySum $ map (\(a,b) -> (-a, b)) xs
neg (PolyFact a b) = PolyFact (neg a) b

pow :: Polynomial -> Integer -> Polynomial 
pow a 1 = a 
pow a n = PolyFact a $ pow a (n-1)

--  addition, subtraction, multiplication and natural exponents of variables

-- PolyFact (Mono 1 2 3) ( 



mapFst :: (a -> x) -> (a, b) -> (x, b)
mapFst f (x, y) = (f x, y)
mapSnd :: (b -> y) -> (a, b) -> (a, y)
mapSnd f (x, y) = (x, f y)
mapBoth :: (a -> x) -> (b -> y) -> (a, b) -> (x, y)
mapBoth fx fy (x, y) = (fx x, fy y)
appBoth  = uncurry mapBoth
removeCommon :: Eq a => [a] -> [a] -> ([a],[a])
removeCommon [] bs = ([],bs)
removeCommon as [] = (as,[])
removeCommon (a:as) bs | a `elem` bs = removeCommon as (delete a bs)
                       | otherwise = (a:as',bs')
                      where (as',bs') = removeCommon as bs