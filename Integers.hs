{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

{- | Integers (Z) with discrete topology form a LinearOrder, Discrete, Hausdorff, Overt space.
   Compact subspaces are finite sets.
-}

module Integers where

import Staged
import Space
import Searchable
 
-- | Linear order on integers
instance LinearOrder Integer where
	less x y = return $ x < y

-- | The Discrete property
instance Discrete Integer where
	equal x y = return $ x == y

-- | The Hausdorff property
instance Hausdorff Integer where
	apart x y = return $ (x < y || x > y)

-- | Subset of the integers
type SubSet = [Integer]

-- | Compactness of the finite subsets
instance Compact SubSet Integer where
	forall s p = return $ forall' (set s) (force . p)
	
-- | Overtness of any subset
instance Overt SubSet Integer where
	exists s p = return $ exists' (set s) (force . p)
	