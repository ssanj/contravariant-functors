{-# LANGUAGE InstanceSigs        #-}

module Comparison where

import Data.Functor.Contravariant (Contravariant, contramap)
import Model

newtype Comparison a = Comparison { getComparison :: a -> a -> Ordering }

instance Contravariant Comparison where
  contramap :: (b -> a) -> Comparison a -> Comparison b
  contramap bToA (Comparison cmpA1A2) = Comparison $ \b1 b2 ->
    let a1 = bToA b1
        a2 = bToA b2
    in cmpA1A2 a1 a2


-- actual implementation from Data.Functor.Contravariant
-- instance Contravariant Comparison where
--   contramap f g = Comparison $ on (getComparison g) f
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c


intCmp :: Comparison Int
intCmp = Comparison compare

strCmp :: Comparison String
strCmp = contramap length intCmp

personAgeCmp :: Comparison Person
personAgeCmp = contramap age intCmp

fstCmp :: Comparison a -> Comparison (a, b)
fstCmp compA = contramap fst compA

-- fstCmp intCmp