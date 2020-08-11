{-# LANGUAGE InstanceSigs        #-}

module Opposite where

import Data.Functor.Contravariant (Contravariant, contramap)
import Model
import qualified Data.Set as S

newtype Op a b = Op { getOp :: b -> a }

instance Contravariant (Op a) where
  contramap :: (c -> b) -> Op a b -> Op a c
  contramap cToB (Op bToA) = Op $ \c ->
    let b = cToB c
    in bToA b

stringsLength :: Op Int [String]
stringsLength = Op $ sum . fmap length

unqiueStringsLength :: Op Int (S.Set String)
unqiueStringsLength = contramap S.toList stringsLength