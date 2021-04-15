module Poly where

import Data.Char.SScript  
import Data.List as D
import Data.List.Extra 

type Coeff = Int
type Exponent = Int 

-- data Monomial = Mono Coeff Exponent Constant -- Mono n p c = n⋅xᵖ + c 

data Polynomial = PolySum [(Coeff, Exponent)] -- PolySum [(n₁,2), (n₂, 1), (n₃, 0)] = n₁⋅x² + n₂⋅x + n₃
                | PolyFact Polynomial Polynomial -- PolyFact n⋅xᵖ+c P = (n⋅xᵖ + c)⋅P

showExp :: (Coeff, Exponent) -> String 
showExp (co, ex) 
    | ex == 0 = show co 
    | ex == 1 = show co ++ "x"
    | otherwise = show co ++ "x^{" ++ show ex ++ "}"

instance Show Polynomial where
    show (PolySum []) = "0"
    show (PolySum xs) = (formatSS $ intercalate " + " $ map showExp xs)
    show (PolyFact a b) = "(" ++ show a ++ ")(" ++ show b ++ ")"

-- instance Show Monomial where 
--     show (Mono co ex cont) = if cont == 0 then 
--             formatSS $ showExp (co, ex) 
--         else 
--             formatSS $ showExp (co, ex) ++ " + " ++ show cont




normalForm :: Polynomial -> Polynomial
normalForm (PolySum xs) = PolySum $ reverse $ filter ((/=) 0 . fst) $ map combine $ groupSortOn snd xs
    where 
        combine :: [(Coeff, Exponent)] -> (Coeff, Exponent)
        combine xs = mapBoth sum head $ unzip xs
normalForm b = normalForm ( PolySum ( asSumForm b))

asSumForm :: Polynomial -> [(Coeff, Exponent)] 
asSumForm (PolySum a) = a 
asSumForm (PolyFact a b) = concatMap (multiply' b') a' 
    where  
        a' = asSumForm a 
        b' = asSumForm b 
        multiply' ys (co,ex) = map (mapBoth ((*) co) ((+) ex)) ys

add :: Polynomial -> Polynomial -> Polynomial 
add (PolySum a) (PolySum b) = PolySum $ a ++ b
add a b = PolySum ((asSumForm a) ++ (asSumForm b))

multiply :: Polynomial -> Polynomial -> Polynomial 
multiply a b = PolyFact a b

neg :: Polynomial -> Polynomial
neg (PolySum xs) = PolySum $ map (\(a,b) -> (-a, b)) xs
neg (PolyFact a b) = PolyFact (neg a) (neg b)

pow :: Polynomial -> Int -> Polynomial 
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