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
                                 RoundUp -> Interval {lower = v, upper = u}
                                 RoundDown   -> let w = midpoint u v in Interval {lower = w, upper = w}
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
            border_lo i j = app_sub s' (lower (approximate (fst $ x i) s')) (snd $ x i)
                where s' = prec RoundDown j 
            border_up i j = app_add s' (upper (approximate (fst $ x i) s')) (snd $ x i)
                where s' = prec RoundUp j
        in case r of
            RoundDown -> Interval {lower = maximum [border_lo i n | i <- [0..n]], upper = minimum [border_up i n | i <- [0..n]]}
            RoundUp -> Interval {lower = minimum [border_up i n | i <- [0..n]], upper = maximum [border_lo i n | i <- [0..n]]}
     )
     
-- | Reals form an Archimedean field. Topologically speaking, this means that the
-- underlying approximate field @q@ is dense in the reals. Computationally this means
-- that we can compute arbitrarily good @q@-approximations to real numbers. The
-- function 'approx_to x k' computes an approximation @a@ of type @q@ which is within
-- @2^-k@ of @x@.
approx_to :: IntervalDomain q => RealNum q -> Int -> (q,Int)
approx_to x k = loop 0
                where loop n = let i = approximate x (prec RoundDown n)
                                   q = if k > 0 then 1/2^(k-1) else 2
                               in case toRational' (width i) < q of 
                                    True -> (midpoint (lower i) (upper i), n)
                                    False -> loop $ n+10

      {- There are several possibilities for optimization. Here we describe one. Let
       @a_n@ be the midpoint of the interval @approximate x (prec RoundDown n)@ and
       let @r_n@ be its width. We are looking for @n@ such that @r_n < 2^{ -n-1}@.
       One heuristic is to assume that @r_n@'s form a geometric series. From this we
       can look at three terms of the sequence, say @r_10@, @r_20@, and @r_30@ and
       estimate an @n@ which should give @r_n < 2^{ -n-1}@. If the estimate fails,
       we try something else. The drawback is that we might end up over-estimating
       the needed precision @n@. -}

fac :: Rational -> Rational
fac n = product [1..n]

tI :: Rational -> Integer
tI r = numerator r

instance IntervalDomain q => Floating (RealNum q) where
    pi = limit(\s -> 
             let r = rounding s
                 n = precision s
                 border k r'= let k' = toRational k
                                  part_serie = 3 + 4 * (sum [ (-1)^((tI i)-1)/((2*i)*(2*i+1)*(2*i+2))|i <- [1..k']])
                              in app_fromRational (prec r' k) part_serie
             in case r of
                 RoundDown -> Interval {lower = (border (2*n) RoundDown), upper = (border (2*n+1) RoundUp)}
                 RoundUp -> Interval {lower = (border (2*n+1) RoundUp), upper = (border (2*n) RoundDown)}
         )
                
    exp x = (sinh x) + (cosh x) 

    sinh x = limit (\s->
                 let r = rounding s 
                     n = precision s
                     border h k m = let (t,r') = case m of
                                                   -1 -> (toRational' (lower (approximate x (prec RoundDown h))),RoundDown)
                                                   1 -> (toRational' (upper (approximate x (prec RoundDown h))),RoundUp)
                                        h' = toRational h                                                
                                        serie = sum [t^(2*(tI i)+1)/(fac (2*i+1))|i <- [0..h']]
                                        reminder = 3^(ceiling (abs t))/2*t^(2*(tI h')+2)/fac (2*h'+2)  
                                        part = serie + m*reminder                                                  
                                    in app_fromRational (prec r' k) part                                       
                 in case r of            
                     RoundDown -> Interval {lower = maximum [border i (2*n) (-1)| i <- [0,2..(2*n)]], upper = minimum [border i (2*n) 1| i <- [0,2..(2*n)]]}
                     RoundUp -> Interval {lower = minimum [border i (2*n) 1| i <- [0,2..(2*n)]], upper = maximum [border i (2*n) (-1)| i <- [0,2..(2*n)]]}                     
              ) 

    cosh x = limit (\s->
                 let r = rounding s 
                     n = precision s
                     border' h k m m' = let (t,r') = case (m,m') of
                                                       (-1,-1) -> (toRational' (lower (approximate x (prec RoundDown h))),RoundDown)
                                                       (1,1) -> (toRational' (upper (approximate x (prec RoundDown h))),RoundUp)
                                                       (-1,1) -> (toRational' (lower (approximate x (prec RoundDown h))),RoundUp)
                                                       (1,-1) -> (toRational' (upper (approximate x (prec RoundDown h))),RoundDown)
                                            h' = toRational h                                                
                                            serie = sum [t^(2*(tI i))/(fac (2*i))|i <- [0..h']]
                                            reminder = (3^(ceiling (abs t))+1)/2*(abs t)^(2*(tI h')+1)/fac (2*h'+1)  
                                            part = serie + m'*reminder                                                 
                                        in app_fromRational (prec r' k) part
                     border h k m = let q1 = toRational' (lower (approximate x (prec RoundDown h)))
                                        q2 = toRational' (upper (approximate x (prec RoundDown h)))
                                    in case (q1<0, q2>0, m) of
                                        (False, True, _) -> border' h k m m
                                        (True, False, _) -> border' h k (-m) m                                  
                                        (True, True, -1) -> app_fromInteger (prec RoundDown k) 1
                                        (True, True, 1) -> maximum [border' h k (-1) 1, border' h k 1 1]
                                        (False, False, _) -> app_fromInteger (prec RoundDown k) 1                                   
                 in case r of            
                     RoundDown -> Interval {lower = maximum [border i (2*n) (-1)| i <- [0,2..(2*n)]], upper = minimum [border i (2*n) 1| i <- [0,2..(2*n)]]}
                     RoundUp -> Interval {lower = minimum [border i (2*n) 1| i <- [0,2..(2*n)]], upper = maximum [border i (2*n) (-1)| i <- [0,2..(2*n)]]}                     
              ) 
  
    asinh x = limit (\s->
                 let r = rounding s 
                     n = precision s
                     border h k m = let (t,r') = case m of
                                              -1 -> (toRational' (lower (approximate x (prec RoundDown h))),RoundDown)
                                              1 -> (toRational' (upper (approximate x (prec RoundDown h))),RoundUp)
                                        h' = toRational h                                             
                                        (serie,reminder) = case (t >= 1,t <= -1) of
                                                 (False,False) -> (sum [(-1)^(tI i)*(fac (2*i))*t^(2*(tI i)+1)/(2^(2*(tI i))*(fac i)^2*(2*i+1))|i <- [0..h']],
                                                                   abs $ 1/(1-(abs t))*(abs t)^(2*(tI h')+2))
                                                 (True,False) -> (1,0)
                                                 (False,True) -> ((-1),0)
                                        part = serie + m*reminder
                                    in app_fromRational (prec r' k) part                                      
                 in case r of 
                     RoundDown -> Interval {lower = maximum [border i (2*n) (-1)| i <- [0,2..(2*n)]], upper = minimum [border i (2*n) 1| i <- [0,2..(2*n)]]}
                     RoundUp -> Interval {lower = minimum [border i (2*n) 1| i <- [0,2..(2*n)]], upper = maximum [border i (2*n) (-1)| i <- [0,2..(2*n)]]}   
              )

    cos x = limit (\s->
                 let r = rounding s 
                     n = precision s
                     sig = if r == RoundDown then 1 else (-1) 
                     q1 = toRational' (lower (approximate x (prec r 0)))
                     q2 = toRational' (upper (approximate x (prec r 0)))
                     m = ceiling (maximum [abs q1, abs q2])
                     v = loop 1 
                          where loop p = let m1 = (3^m)/2^(p+1) 
                                         in case m1 < 1/2^n of
                                              True -> p
                                              False -> loop (p+1)
                     u = loop 1
                          where loop p = let m1 = 2^n*(3^m+1)*(toRational m)^(2*(tI p)+2)
                                             m2 = 2*(fac (2*p+2)) 
                                         in case p >= (toRational m) && m2 >= m1 of
                                              True -> p
                                              False -> loop (p+1)
                     serie t = sum [(-1)^(tI i)*t^(2*(tI i))/(fac (2*i))|i <- [0..u]]
                     k = maximum [snd (approx_to x v), n]
                     x1 = toRational' (lower (approximate x (prec r k)))
                     x2 = toRational' (upper (approximate x (prec r k)))
                     reminder = if n > 0 then (1/2^(n-1)) else 2
                     s' = prec r k
                     part1 = app_fromRational s' ((serie x1) - sig*reminder)
                     part2 = app_fromRational (anti s') ((serie x2) + sig*reminder)
                 in Interval {lower = part1, upper = part2}
             )    
    sin x = limit (\s->
                 let r = rounding s 
                     n = precision s
                     sig = if r == RoundDown then 1 else (-1) 
                     q1 = toRational' (lower (approximate x (prec r 0)))
                     q2 = toRational' (upper (approximate x (prec r 0)))
                     m = ceiling (maximum [abs q1, abs q2])
                     v = loop 1 
                          where loop p = let m1 = (3^m+1)/2^(p+1) 
                                         in case m1 < 1/2^n of
                                              True -> p
                                              False -> loop (p+1)
                     u = loop 1
                          where loop p = let m1 = 2^n*(3^m+1)*(toRational m)^(2*(tI p)+3)
                                             m2 = 2*(fac (2*p+3)) 
                                         in case p >= (toRational m) && m2 >= m1 of
                                              True -> p
                                              False -> loop (p+1)
                     serie t = sum [(-1)^(tI i)*t^(2*(tI i)+1)/(fac (2*i+1))|i <- [0..u]]
                     k = maximum [snd (approx_to x v), n]
                     x1 = toRational' (lower (approximate x (prec r k)))
                     x2 = toRational' (upper (approximate x (prec r k)))
                     reminder = if n > 0 then (1/2^(n-1)) else 2
                     s' = prec r k
                     part1 = app_fromRational s' ((serie x1) - sig*reminder)
                     part2 = app_fromRational (anti s') ((serie x2) + sig*reminder)
                 in Interval {lower = part1, upper = part2}
             )    
    acosh x = error "Not implemented"
    atanh x = error "Not implemented"
    log x = error "Not implemented" 