module Model
       (
          -- Data types
          Person(..)
       ) where

data Person = Person { name :: String, age :: Int } deriving Show