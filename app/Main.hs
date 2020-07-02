module Main where

import LogAction

main :: IO ()
main = do
  logHelloWorld
  logNumber
  logPerson
  logPersonAge
  logGreeting
  logOverride

logHelloWorld :: IO ()
logHelloWorld = unlog putStrLnLog "Hello World"
-- Hello World

logNumber :: IO ()
logNumber = unlog putStrLnInt 42
-- 42

logPerson :: IO ()
logPerson = unlog putStrLnPerson $ Person "Neelix" 60
-- Person(name:Neelix, age: 60)

logPersonAge :: IO ()
logPersonAge = unlog putStrLnPersonAge $ Person "Tovak" 240
-- age: 240

logGreeting :: IO ()
logGreeting = unlog putStrLnGreeting "Switzer"
-- Hello there Doctor Switzer

logOverride :: IO ()
logOverride = unlog qPutStrLn "Picard J L"
-- This is Q!!