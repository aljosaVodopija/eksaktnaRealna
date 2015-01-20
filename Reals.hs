{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

{- | We implement real numbers as the completion of dyadic intervals. The whole construction is
   parametrized by an approximate field, an example of which is "Dyadic".
-}

module Reals where

import Data.Ratio
import Staged
import Space
import Dyadic
import Interval


-- | A real number is implemented as a staged dyadic interval @'Interval' q@ where @q@ is the
-- underlying approximate field (in practiec these are dyadic rationals). @'RealNum' q@ can be used
-- to represent not only real numbers but also the elements of the interval domain, including the
-- back-to-front intervals.
type RealNum q = Staged (Interval q)

-- | We implement a very simple show instance for reals which computes the 20th approximation
-- and shows it as an interval, together with a floating point approximation.
instance ApproximateField q => Show (RealNum q) where
   show x = let i = approximate x (prec RoundDown 20)
            in show i ++ " " ++ show (toFloat (midpoint (lower i) (upper i)))

-- | Linear order on real numbers
instance IntervalDomain q => LinearOrder (RealNum q) where
    less = lift2 (\_ -> iless)

-- | It is a bad idea to use Haskell-style inequality @/=@ on reals because it either returns @True@
-- or it diverges. Similarly, using Haskell equality @==@ is bad. Nevertheless, we define @==@ and @/=@
-- because Haskell wants them for numeric types.
instance IntervalDomain q => Eq (RealNum q) where
    x /= y = force $ x `apart` y

-- | Real numbers are an ordered type in the sense of Haskells 'Ord', although a comparison never
-- returns @EQ@ (instead it diverges). This is a fact of life, comparison of reals is not decidable.
instance IntervalDomain q => Ord (RealNum q) where
  compare x y = case force (x `less` y) of
                  True  -> LT
                  False -> GT

-- | The ring structure fo the reals.
instance (ApproximateField q, IntervalDomain q) => Num (RealNum q) where
    x + y = lift2 iadd x y
    x - y = lift2 isub x y
    x * y = lift2 imul x y

    abs x = lift1 iabs x

    signum x = do i <- x
                  s <- get_stage
                  return $ Interval { lower = app_signum s (lower i),
                                      upper = app_signum (anti s) (upper i) }

    fromInteger k = do s <- get_stage
                       return $ Interval { lower = app_fromInteger s k,
                                           upper = app_fromInteger (anti s) k }

-- | Division and reciprocals.
instance (ApproximateField q, IntervalDomain q) => Fractional (RealNum q) where
    x / y = lift2 idiv x y
            
    recip x = lift1 iinv x
                      
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

-- | The Hausdorff property
instance IntervalDomain q => Hausdorff (RealNum q) where
     x `apart` y = (x `less` y) `sor` (y `less` x)

-- | The value @ClosedInterval(a,b)@ represents the closed interval [a,b] as a subspace of the reals.
newtype ClosedInterval q = ClosedInterval (q, q)

-- | Compactness of the closed interval
instance IntervalDomain q => Compact (ClosedInterval q) (RealNum q) where
   forall (ClosedInterval(a,b)) p =
     limit (\s ->
       let r = rounding s
           n = precision s
           test_interval u v = case r of
                                 RoundDown -> Interval {lower = u, upper = v}
                                 RoundUp   -> let w = midpoint u v in Interval {lower = w, upper = w}
           sweep [] = True
           sweep ((k,a,b):lst) = let x = return $ test_interval a b
                                    in case (r, approximate (p x) (prec r k)) of
                                      (RoundDown, False) -> (k < n) &&
                                                            (let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)]))
                                      (RoundDown, True)  -> sweep lst
                                      (RoundUp,   False) -> False
                                      (RoundUp,   True)  -> (k >= n) ||
                                                            (let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)]))
       in sweep [(0,a,b)]
     )

-- | Overtness of reals, open interval (a,b) and closed interval [a,b]
instance IntervalDomain q => Overt (ClosedInterval q) (RealNum q) where
   exists (ClosedInterval (a,b)) p = 
     limit (\s ->
       let r = rounding s
           n = precision s
           test_interval u v = case r of
                                 RoundUp   -> Interval {lower = v, upper = u}
                                 RoundDown -> let w = midpoint u v in Interval {lower = w, upper = w}
           sweep [] = False
           sweep ((k,a,b):lst) = let x = return $ test_interval a b
                                    in case (r, approximate (p x) (prec r k)) of
                                      (RoundDown, False) -> if (k < n) 
                                                                then (let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)]))
                                                            else False
                                      (RoundDown, True)  -> True
                                      (RoundUp,   False) -> sweep lst
                                      (RoundUp,   True)  -> (k >= n) ||
                                                            (let c = midpoint a b in sweep (lst ++ [(k+1,a,c), (k+1,c,b)]))
       in sweep [(0,a,b)]
     )
     
-- | We define the a particular implementation of reals in terms of Dyadic numbers.
-- We need to implement only width. 
instance IntervalDomain Dyadic where
     width Interval{lower=a, upper=b} = b - a 

-- | This is a convenience function which allows us to write @exact 1.3@ as a
-- conversion from floating points to real numbers. There probably is a better way of
-- doing this.
exact :: RealNum Dyadic -> RealNum Dyadic
exact x = x

-- | This function convert elements of type @q@ in elements of type @RealNum q@.
toReal :: IntervalDomain q => q -> RealNum q
toReal x = limit $ \s -> Interval { lower = normalize s x, upper = normalize (anti s) x }
            
-- | Reals form a complete space, which means that every Cauchy sequence of reals has
-- a limit. In the implementation this is manifested by the existence of an operator
-- which computes the limit of a Cauchy sequence. The error bounds for the sequence are
-- given explicitly.
lim :: IntervalDomain q => (Int -> (RealNum q, q)) -> RealNum q
lim x =  
     limit (\s -> 
        let r = rounding s
            n = precision s
            border_lower i j = app_sub s' (lower (approximate (fst $ x i) s')) (snd $ x i)
                where s' = prec_down j 
            border_upper i j = app_add s' (upper (approximate (fst $ x i) s')) (snd $ x i)
                where s' = prec_up j
        in case r of
            RoundDown -> Interval {lower = maximum [border_lower i n | i <- [0..n]], upper = minimum [border_upper i n | i <- [0..n]]}
            RoundUp   -> Interval {lower = minimum [border_upper i n | i <- [0..n]], upper = maximum [border_lower i n | i <- [0..n]]}
     )
     
-- | Reals form an Archimedean field. Topologically speaking, this means that the
-- underlying approximate field @q@ is dense in the reals. Computationally this means
-- that we can compute arbitrarily good @q@-approximations to real numbers. The
-- function 'approx_to x k r' computes an approximation @a@ of type @q@ which is within
-- @2^-k@ of @x@.
approx_to :: IntervalDomain q => RealNum q -> Int -> RoundingMode -> (q, Int)
approx_to x k r = let r10 = toRational' (width (approximate x (prec r 10)))
                      r20 = toRational' (width (approximate x (prec r 20)))
                      n = case r20==0 of
                            True -> 20
                            False -> let a = ceiling (r10^2/r20)
                                     in (ilogb 2 a)+k
                      i = approximate x (prec r n)
                      q = 2/2^k 
                  in case toRational' (width i) < q of
                      True -> (midpoint (lower i) (upper i), n)
                      False -> loop (n+1)
                                where loop m = let i = approximate x (prec r m)
                                               in case toRational' (width i) < q of 
                                                   True -> (midpoint (lower i) (upper i), m)
                                                   False -> loop $ m+1

fac :: Rational -> Rational
fac n = product [1..n]

tI :: Rational -> Integer
tI r = numerator r

ilogb :: Integer -> Integer -> Int
ilogb b n | n < 0      = ilogb b (- n)
          | n < b      = 0
          | otherwise  = (up b n 1) - 1
  where up b n a = if n < (b ^ a)
                      then bin b (quot a 2) a
                      else up b n (2*a)
        bin b lo hi = if (hi - lo) <= 1
                         then hi
                         else let av = quot (lo + hi) 2
                              in if n < (b ^ av)
                                    then bin b lo av
                                    else bin b av hi

-- | Instance floating for reals uses Taylor's series and error bounds. (Missing: Atanh, Atan, Acos, Acosh)
-- Functions (Cos, Sin, Exp, Tan, Cosh, Sinh) makes good approximations in short time for elements inside 
-- the interval (-30,30) and for integers. Log is defined for elements inside (0,2) and Asinh for elements 
-- inside (-1,1).                                      
instance IntervalDomain q => Floating (RealNum q) where

    pi = limit(\s -> 
             let r = rounding s
                 n = precision s
                 border k r'= let k' = toRational k
                                  serie = 3 + 4 * (sum [ (-1)^((tI i)-1)/((2*i)*(2*i+1)*(2*i+2))|i <- [1..k']])
                              in app_fromRational (prec r' k) serie
             in case r of
                 RoundDown -> Interval {lower = (border (2*n) RoundDown), upper = (border (2*n+1) RoundUp)}
                 RoundUp   -> Interval {lower = (border (2*n+1) RoundUp), upper = (border (2*n) RoundDown)}
         )
         
    exp x = limit (\s->
                 let r = rounding s 
                     n = precision s + 4 
                     sig = if r == RoundDown then 1 else (-1) 
                     q1 = toRational' (lower (approximate x (prec r 4)))
                     q2 = toRational' (upper (approximate x (prec r 4)))
                     m = ceiling (maximum [abs q1, abs q2])
                     m' = toRational m
                     v = n+1+(ilogb 2 (3^m)) 
                     u = loop m'
                          where loop p = let m1 = 2^n*(3^m)*m'^((tI p)+1)
                                             m2 = fac (p+1) 
                                         in case m2 >= m1 of
                                             True -> p
                                             False -> loop (p+1)
                     serie t = sum [t^(tI i)/(fac i)|i <- [0..u]]
                     k = maximum [snd (approx_to x v r), n]
                     x1 = toRational' (lower (approximate x (prec r k)))
                     x2 = toRational' (upper (approximate x (prec r k)))
                     remainder = 1/2^(n-1)
                     s' = prec r k
                     part1 = app_fromRational s' ((serie x1) - sig*remainder)
                     part2 = app_fromRational (anti s') ((serie x2) + sig*remainder)
                 in Interval {lower = part1, upper = part2}
             ) 
             
    sinh x = ((exp x) - (exp (-x)))/2

    cosh x = ((exp x) + (exp (-x)))/2
  
    asinh x = limit (\s->
                 let r = rounding s 
                     n = precision s
                     border h k m = let (t, r') = case m of
                                                  -1 -> (toRational' (lower (approximate x (prec_down h))), RoundDown)
                                                  1  -> (toRational' (upper (approximate x (prec_down h))), RoundUp)
                                        h' = toRational h                                             
                                        (serie, remainder) = case (t >= 1,t <= -1) of
                                                               (False, False) -> (sum [(-1)^(tI i)*(fac (2*i))*t^(2*(tI i)+1)/(2^(2*(tI i))*(fac i)^2*(2*i+1))|i <- [0..h']],
                                                                                  abs $ 1/(1-(abs t))*(abs t)^(2*(tI h')+2))
                                                               (True,  False) -> (1,0)
                                                               (False, True)  -> ((-1),0)
                                        part = serie + m*remainder
                                    in app_fromRational (prec r' k) part                                      
                 in case r of 
                     RoundDown -> Interval {lower = maximum [border i (2*n) (-1)| i <- [0,2..(2*n)]], upper = minimum [border i (2*n) 1| i <- [0,2..(2*n)]]}
                     RoundUp   -> Interval {lower = minimum [border i (2*n) 1| i <- [0,2..(2*n)]], upper = maximum [border i (2*n) (-1)| i <- [0,2..(2*n)]]}   
              )

    cos x = limit (\s->
                 let r = rounding s 
                     n = precision s + 4 
                     sig = if r == RoundDown then 1 else (-1) 
                     q1 = toRational' (lower (approximate x (prec r 4)))
                     q2 = toRational' (upper (approximate x (prec r 4)))
                     m = ceiling (maximum [abs q1, abs q2])
                     m' = toRational m
                     v = n + (ilogb 2 (3^m+1))  
                     u = loop m'
                          where loop p = let m1 = 2^n*(3^m+1)*m'^(2*(tI p)+2)
                                             m2 = 2*(fac (2*p+2)) 
                                         in case m2 >= m1 of
                                             True -> p
                                             False -> loop (p+1)
                     serie t = sum [(-1)^(tI i)*t^(2*(tI i))/(fac (2*i))|i <- [0..u]]
                     k = maximum [snd (approx_to x v r), n]
                     x1 = toRational' (lower (approximate x (prec r k)))
                     x2 = toRational' (upper (approximate x (prec r k)))
                     remainder = 1/2^(n-1)
                     s' = prec r k
                     part1 = app_fromRational s' ((serie x1) - sig*remainder)
                     part2 = app_fromRational (anti s') ((serie x2) + sig*remainder)
                 in Interval {lower = part1, upper = part2}
             ) 
             
    sin x = limit (\s->
                 let r = rounding s 
                     n = precision s + 4 
                     sig = if r == RoundDown then 1 else (-1) 
                     q1 = toRational' (lower (approximate x (prec r 4)))
                     q2 = toRational' (upper (approximate x (prec r 4)))
                     m = ceiling (maximum [abs q1, abs q2])
                     m' = toRational m
                     v = n + (ilogb 2 (3^m))
                     u = loop m'
                          where loop p = let m1 = 2^n*(3^m+1)*m'^(2*(tI p)+3)
                                             m2 = 2*(fac (2*p+3)) 
                                         in case m2 >= m1 of
                                             True -> p
                                             False -> loop (p+1)
                     serie t = sum [(-1)^(tI i)*t^(2*(tI i)+1)/(fac (2*i+1))|i <- [0..u]]
                     k = maximum [snd (approx_to x v r), n]
                     x1 = toRational' (lower (approximate x (prec r k)))
                     x2 = toRational' (upper (approximate x (prec r k)))
                     remainder = 1/2^(n-1)
                     s' = prec r k
                     part1 = app_fromRational s' ((serie x1) - sig*remainder)
                     part2 = app_fromRational (anti s') ((serie x2) + sig*remainder)
                 in Interval {lower = part1, upper = part2}
             )    
             
    atanh x = (log (1+x) - log (1-x))/2

    log x = let b = compare x 0
            in case b of 
                LT -> error "Not defined"
                GT -> let b' = compare x 2
                      in case b' of 
                          LT -> let b'' = compare x 1
                                in case b'' of 
                                    LT -> limit (\s->
                                           let r = rounding s 
                                               n = precision s
                                               border h k m = let (t,r') = case m of
                                                                             -1 -> (toRational' (lower (approximate x (prec RoundDown h))),RoundDown)
                                                                             1 -> (toRational' (upper (approximate x (prec RoundDown h))),RoundUp)
                                                                  h' = toRational h                                                
                                                                  serie = -sum [(-1)^(tI i)*(-1+t)^(tI i)/i|i <- [1..h']]
                                                                  remainder = 3^(ceiling (abs t))/2*(-1+t)^(tI h'+1)/(h'+1) 
                                                                  part = serie + m*remainder                                                  
                                                              in app_fromRational (prec r' k) part                                       
                                           in case r of            
                                               RoundDown -> Interval {lower = maximum [border i n (-1)| i <- [1..n]], upper = minimum [border i n 1| i <- [1..n]]}
                                               RoundUp -> Interval {lower = minimum [border i n 1| i <- [1..n]], upper = maximum [border i n (-1)| i <- [1..n]]}                     
                                          )
                                    GT -> limit (\s->
                                           let r = rounding s 
                                               n = precision s
                                               border h k m = let (t,r') = case m of
                                                                             -1 -> (toRational' (lower (approximate x (prec RoundDown h))),RoundDown)
                                                                             1 -> (toRational' (upper (approximate x (prec RoundDown h))),RoundUp)
                                                                  h' = toRational h                                                
                                                                  serie = -sum [(-1)^(tI i)*(-1+t)^(tI i)/i|i <- [1..h']]
                                                                  remainder = (-1+t)^(tI h'+1)/(h'+1) 
                                                                  part = serie + m*remainder                                                  
                                                              in app_fromRational (prec r' k) part                                       
                                           in case r of            
                                               RoundDown -> Interval {lower = maximum [border i n (-1)| i <- [1..n]], upper = minimum [border i n 1| i <- [1..n]]}
                                               RoundUp -> Interval {lower = minimum [border i n 1| i <- [1..n]], upper = maximum [border i n (-1)| i <- [1..n]]}                     
                                          )
                          GT -> error "Not defined"