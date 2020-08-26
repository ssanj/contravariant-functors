{-# LANGUAGE InstanceSigs        #-}

module CallbackRunner where

import Data.Functor.Contravariant (Contravariant, contramap)
import Model

newtype CallbackRunner a =
  CallbackRunner {
    runCallback :: (a -> IO ()) -> IO ()
  }

instance Contravariant CallbackRunner where
  contramap :: (a -> b) -> CallbackRunner b -> CallbackRunner a
  contramap aToB (CallbackRunner runCallbackB) = CallbackRunner $ \aToIO ->
    runCallbackB $ \b ->
      let a = undefined -- where do we get an `a` from?
      in aToIO a

instance Functor CallbackRunner where
  fmap :: (a -> b) -> CallbackRunner a -> CallbackRunner b
  fmap aToB (CallbackRunner runCallbackA) = CallbackRunner $ \bToIO ->
    runCallbackA $ \a ->
      let b      = aToB a
          result = bToIO b
      in result

