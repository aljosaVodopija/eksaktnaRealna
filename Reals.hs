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
        let r = (rounding s == RoundDown)
            n = precision s
            b i j = app_sub s' (lower (approximate (fst $ x i) s')) (snd $ x i)
                where s' = prec RoundDown j	
            c i j = app_add s' (upper (approximate (fst $ x i) s')) (snd $ x i)
                where s' = prec RoundUp j
            k i j = j -- mozne izboljsave
        in case r of
            True -> Interval {lower = maximum [b i (k i n) | i <- [0..n]], upper = minimum [c i (k i n) | i <- [0..n]]}
            False -> Interval {lower = minimum [c i (k i n) | i <- [0..n]], upper = maximum [b i (k i n) | i <- [0..n]]}
     )
	 
-- | Reals form an Archimedean field. Topologically speaking, this means that the
-- underlying approximate field @q@ is dense in the reals. Computationally this means
-- that we can compute arbitrarily good @q@-approximations to real numbers. The
-- function 'approx_to x k' computes an approximation @a@ of type @q@ which is within
-- @2^-k@ of @x@.
approx_to :: IntervalDomain q => RealNum q -> Int -> q
approx_to x k = loop 0
                where loop n = let i = approximate x (prec RoundDown n)
                               in case (toReal (width i)) < 1/2^(k+1) of 
                                    True -> midpoint (lower i) (upper i)
                                    False -> loop $ n+1

      {- There are several possibilities for optimization. Here we describe one. Let
       @a_n@ be the midpoint of the interval @approximate x (prec RoundDown n)@ and
       let @r_n@ be its width. We are looking for @n@ such that @r_n < 2^{ -n-1}@.
       One heuristic is to assume that @r_n@'s form a geometric series. From this we
       can look at three terms of the sequence, say @r_10@, @r_20@, and @r_30@ and
       estimate an @n@ which should give @r_n < 2^{ -n-1}@. If the estimate fails,
       we try something else. The drawback is that we might end up over-estimating
       the needed precision @n@. -}

-- | This function is approx_to for @RealNum Dyadic@. Is faster than normal. 
approx_to' :: RealNum Dyadic -> Int -> Dyadic
approx_to' x k = loop 0
                where loop n = let i = approximate x (prec RoundDown n)
                                   a = Dyadic {mant = 1, expo= -(k+1)}  
				               in case (width i) < a of 
                                    True -> midpoint (lower i) (upper i)
                                    False -> loop $ n+1
      