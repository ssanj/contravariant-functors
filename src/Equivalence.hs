{-# LANGUAGE InstanceSigs        #-}

module Equivalence where

import Data.Functor.Contravariant (Contravariant, contramap)
import Model

newtype Equivalence a = Equivalence { getEquivalence :: a -> a -> Bool }

instance Contravariant Equivalence where
  contramap :: (b -> a) -> Equivalence a -> Equivalence b
  contramap bToA (Equivalence eqA1A2) = Equivalence $ \b1 b2 ->
    let a1 = bToA b1
        a2 = bToA b2
    in eqA1A2 a1 a2


-- actual implementation from Data.Functor.Contravariant
-- instance Contravariant Equivalence where
--   contramap f g = Equivalence $ on (getEquivalence g) f

-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c


intEq :: Equivalence Int
intEq = Equivalence (==)

strLengthEq :: Equivalence String
strLengthEq = contramap length intEq

personAgeEq :: Equivalence Person -- equality by age
personAgeEq = contramap age intEq

personNameLengthEq :: Equivalence Person -- equality by length of name
personNameLengthEq = contramap name strLengthEq