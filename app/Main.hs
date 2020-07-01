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

logNumber :: IO ()
logNumber = unlog putStrLnInt 42

logPerson :: IO ()
logPerson = unlog putStrLnPerson $ Person "Neelix" 60

logPersonAge :: IO ()
logPersonAge = unlog putStrLnPersonAge $ Person "Tovak" 240

logGreeting :: IO ()
logGreeting = unlog putStrLnGreeting "Switzer"

logOverride :: IO ()
logOverride = unlog (q putStrLnLog) $ "Picard J L"