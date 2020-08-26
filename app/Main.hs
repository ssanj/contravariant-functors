module Main where

import LogAction
import Model
import Equivalence
import Comparison
import Opposite

import Data.List (sortBy)
import Data.Set (fromList)

main :: IO ()
main = do
  line "Log"
  logActions
  line "Equivalence"
  equalityActions
  line "Comparison"
  comparisonActions
  line "Opposites"
  oppositeActions

oppositeActions :: IO ()
oppositeActions = do
  let namesList = ["Paris", "Kim", "B'Elanna", "Seven"]
      namesSet  = fromList namesList
  putStrLn $ "namesList length: " <> (show . getOp stringsLength $ namesList)
  putStrLn $ "namesSet length: " <> (show . getOp unqiueStringsLength $ namesSet)

comparisonActions :: IO ()
comparisonActions = do
  cmpInts
  sortInts

sortInts :: IO ()
sortInts = do
  let unsortedNumbers = [3, 5, 1, 4, 2]
      unsortedPeople = [Person "Tuvok1" 240, Person "Janeway" 40, Person "Neelix" 60]
      sortedNumbersAsc = sortBy (getComparison intCmp) unsortedNumbers
      sortedNumbersDsc = sortBy (flip $ getComparison intCmp) unsortedNumbers
      sortedPeopleAgeAsc = sortBy (getComparison personAgeCmp) unsortedPeople
      sortedPeopleAgeDsc = sortBy (flip $ getComparison personAgeCmp) unsortedPeople
  putStrLn $ "unsorted numbers: " <> (show unsortedNumbers)
  putStrLn $ "unsorted people: " <> (show unsortedPeople)
  putStrLn $ "sorted ascending numbers: " <> (show sortedNumbersAsc)
  putStrLn $ "sorted people by ascending age: " <> (show sortedPeopleAgeAsc)
  putStrLn $ "sorted descending numbers:" <> (show sortedNumbersDsc)
  putStrLn $ "sorted people by descending age:" <> (show sortedPeopleAgeDsc)

cmpInts :: IO ()
cmpInts = do
  putStrLn $ "1 cmp 1 ? " <> (show $ getComparison intCmp 1 1)
  -- 1 cmp 1 ? EQ

  putStrLn $ "1 cmp 2 ? " <> (show $ getComparison intCmp 1 2)
  -- 1 cmp 2 ? LT

  putStrLn $ "2 cmp 1 ? " <> (show $ getComparison intCmp 2 1)
  -- 2 cmp 1 ? GT

equalityActions :: IO ()
equalityActions = do
  eqInts
  eqPerson
  eqStr

eqPerson :: IO ()
eqPerson = do
  let t1 = Person "Tuvok1" 240
      t2 = Person "Tuvok2" 340
      t3 = Person "Neelix" 60
      t4 = Person "Janeway" 40
  putStrLn $ "Tuvok1's age == Tuvok2's age ? " <> (show $ getEquivalence personAgeEq t1 t2)
  -- Tuvok1's age == Tuvok2's age ? False

  putStrLn $ "Tuvok1's age == Tuvok1's age ? " <> (show $ getEquivalence personAgeEq t1 t1)
  -- Tuvok1's age == Tuvok1's age ? True

  putStrLn $ "Tuvok2's age == Tuvok2's age ? " <> (show $ getEquivalence personAgeEq t2 t2)
  -- Tuvok2's age == Tuvok2's age ? True

  putStrLn $ "Tuvok1's name length == Tuvok2's name length ? " <> (show $ getEquivalence personNameLengthEq t1 t2)
  -- Tuvok2's name length == Tuvok2's name length ? True

  putStrLn $ "Janeway's name length == Neelix's name length ? " <> (show $ getEquivalence personNameLengthEq t3 t4)
  -- Tuvok2's name length == Neelix's name length ? False

eqInts :: IO ()
eqInts = do
  putStrLn $ "1 == 2 ? " <> (show $ getEquivalence intEq 1 2)
  -- 1 == 2 ? False

  putStrLn $ "1 == 1 ? " <> (show $ getEquivalence intEq 1 1)
  -- 1 == 1 ? True

  putStrLn $ "2 == 2 ? " <> (show $ getEquivalence intEq 2 2)
  -- 2 == 2 ? True

eqStr :: IO ()
eqStr = do
  let s1 = "hello"
      s2 = "goodbye"
  putStrLn $ s1 <> " == " <> s2 <> " ? " <> (show $ getEquivalence strLengthEq s1 s2)
  -- hello == goodbye ? False

  putStrLn $ s1 <> " == " <> s1 <> " ? " <> (show $ getEquivalence strLengthEq s1 s1)
  -- hello == hello ? True

  putStrLn $ s2 <> " == " <> s2 <> " ? " <> (show $ getEquivalence strLengthEq s2 s2)
  -- goodbye == goodbye ? True

logActions :: IO ()
logActions = do
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
logPersonAge = unlog putStrLnPersonAge $ Person "Tuvok" 240
-- age: 240

logGreeting :: IO ()
logGreeting = unlog putStrLnGreeting "Switzer"
-- Hello there Doctor Switzer

logOverride :: IO ()
logOverride = logOverrideWithFunc >> logOverrideWithOp

logOverrideWithFunc :: IO ()
logOverrideWithFunc = unlog qPutStrLn "Picard J L"
-- This is Q!!

logOverrideWithOp :: IO ()
logOverrideWithOp = unlog qPutStrLnOp "Sisko B L"
-- This is Q!!

line :: String -> IO ()
line section = putStrLn "" >> putStrLn section >> (putStrLn $ take 20 $ repeat '-')