{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LogAction where

import Data.Functor.Contravariant

newtype LogAction a = LogAction { unlog :: a -> IO () }

instance Contravariant LogAction where
  contramap :: (b -> a) -> LogAction a -> LogAction b
  -- contramap f logActionA = LogAction $ \b -> unlog logActionA (f b)
  -- simplified version
  contramap f logActionA = LogAction $ unlog logActionA . f


putStrLog :: LogAction String
putStrLog = LogAction putStr

putStrLnLog :: LogAction String
putStrLnLog = LogAction putStrLn

putStringlyLnLog :: (a -> String) -> LogAction a
putStringlyLnLog f = contramap f putStrLnLog

putStrLnInt :: LogAction Int
putStrLnInt = putStringlyLnLog show

data Person = Person { name :: String, age :: Int }

showPerson :: Person -> String
showPerson (Person name age) = "Person(name:" <> name <> ", age: " <> (show age) <> ")"

putStrLnPerson :: LogAction Person
putStrLnPerson = putStringlyLnLog showPerson

showPersonAge :: Person -> String
showPersonAge person =  "age: " <> (show $ age person)

putStrLnPersonAge :: LogAction Person
putStrLnPersonAge = putStringlyLnLog showPersonAge

putStrLnGreeting :: LogAction String
putStrLnGreeting = contramap space . contramap doctor .contramap space . contramap there . contramap space . contramap hello $ putStrLnLog
-- using Law 2
-- putStrLnGreeting = contramap  (hello . space . there . space . doctor . space) $ putStrLnLog

hello :: String -> String
hello = ("Hello" <>)

there :: String -> String
there = ("there" <>)

doctor :: String -> String
doctor = ("Doctor" <>)

space :: String -> String
space = (" " <>)

override :: a -> a -> a
override value = const value

q :: LogAction String -> LogAction String
q = contramap (override "This is Q!!")