{-|
Module      : Tasks.Task7
Description : Bināro meklēšanas koku pārbaude un ievietošanas īpašības.

Uzdevums Nr. 7.

Modulis satur:

1. funkciju 'isSearchTree', kas pārbauda, vai koks ir binārais meklēšanas
   koks (BMK);
2. uzdevumā doto funkciju 'insertTree' un īpašības pārbaudi: ja @t@ ir BMK,
   tad arī @insertTree e t@ ir BMK;
3. eksperimentus ar trim izmainītām 'insertTree' versijām, lai noskaidrotu,
   vai īpašība atklāj, ka tās vairs neveido korektus BMK.

Piezīme par tipu: uzdevumā dots @isSearchTree :: Tree a -> Bool@, bet
elementu salīdzināšanai vajadzīgs ierobežojums @Ord a@, tāpēc tips ir
@isSearchTree :: Ord a => Tree a -> Bool@.

Īpašības tiek pārbaudītas bez ārējām bibliotēkām: ar visiem maziem kokiem
(izsmeļoši) un ar kokiem no pseidogadījuma sarakstiem.

Robežgadījumi, kas tiek pārbaudīti testos:

* tukšs koks, viena mezgla koks;
* vienādas vērtības kreisajā un labajā apakškokā;
* kļūda tiešajā bērnā un kļūda dziļāk kokā (to nepamana pārbaude, kas
  salīdzina tikai vecāku ar bērniem);
* nekorekta lietotāja ievade (burti, decimāldaļskaitļi, tukša ievade).
-}
module Tasks.Task7
  ( Tree (..)
  , isSearchTree
  , isSearchTreeNaive
  , inorder
  , insertTree
  , insertSwapped
  , insertStrict
  , insertLosing
  , fromList
  , propSearchTree
  , propElements
  , counterexample
  , drawTree
  , runTask7
  ) where

import Control.Monad (when)
import Data.List (sort)

import Utils.Input (askYesNo, parseInt, parseIntList, promptUntilValid, withDefault)
import Utils.Testing

{-|
Binārā koka datu tips (no uzdevuma). 'Show' un 'Eq' pievienoti, lai koku
varētu izdrukāt un salīdzināt.
-}
data Tree a = Branch a (Tree a) (Tree a) | Leaf
  deriving (Show, Eq)

{-|
Pārbauda, vai koks ir binārais meklēšanas koks.

BMK nosacījums atbilst 'insertTree' konvencijai (@e <= x@ tiek ievietots
pa kreisi): visas vērtības kreisajā apakškokā ir @<= x@, visas vērtības
labajā apakškokā ir @> x@, un tas pats izpildās katrā mezglā.

Ejot lejup pa koku, funkcija nes līdzi atļauto intervālu: apakšējo robežu
(izslēgta) un augšējo robežu (ieslēgta). Tāpēc tiek pamanīta arī kļūda
dziļāk kokā, ne tikai starp vecāku un tiešo bērnu.

Parametri:

* @tree@ - pārbaudāmais koks.

Atgrieztā vērtība:

* 'True', ja koks ir BMK (tukšs koks arī ir BMK), citādi 'False'.

Sarežģītība:

* Laika sarežģītība: \(O(n)\) - katrs mezgls tiek apskatīts vienu reizi.
-}
isSearchTree :: Ord a => Tree a -> Bool
isSearchTree = within Nothing Nothing
  where
    within _ _ Leaf = True
    within lower upper (Branch x left right) =
         maybe True (< x) lower           -- x ir lielāks par apakšējo robežu
      && maybe True (x <=) upper          -- x nav lielāks par augšējo robežu
      && within lower (Just x) left       -- kreisajā apakškokā vērtības <= x
      && within (Just x) upper right      -- labajā apakškokā vērtības > x

{-|
Vienkāršā versija tieši pēc definīcijas; izmantota tikai 'isSearchTree'
pārbaudei.

Parametri:

* @tree@ - pārbaudāmais koks.

Atgrieztā vērtība:

* tāda pati kā funkcijai 'isSearchTree'.

Sarežģītība:

* Laika sarežģītība: \(O(n^2)\) sliktākajā gadījumā.
-}
isSearchTreeNaive :: Ord a => Tree a -> Bool
isSearchTreeNaive Leaf = True
isSearchTreeNaive (Branch x left right) =
     all (<= x) (inorder left)
  && all (> x) (inorder right)
  && isSearchTreeNaive left
  && isSearchTreeNaive right

{-|
Atgriež koka elementus secībā \"kreisais apakškoks, sakne, labais apakškoks\".
BMK gadījumā šis saraksts ir sakārtots.

Parametri:

* @tree@ - koks.

Atgrieztā vērtība:

* elementu saraksts.
-}
inorder :: Tree a -> [a]
inorder Leaf                  = []
inorder (Branch x left right) = inorder left ++ [x] ++ inorder right

{-|
Ievieto vērtību binārajā meklēšanas kokā (kods no uzdevuma).

Parametri:

* @e@ - ievietojamā vērtība;
* @tree@ - koks.

Atgrieztā vērtība:

* jauns koks ar ievietoto vērtību.
-}
insertTree :: Ord a => a -> Tree a -> Tree a
insertTree e Leaf = Branch e Leaf Leaf
insertTree e (Branch x li re)
  | e <= x = Branch x (insertTree e li) re
  | e > x  = Branch x li (insertTree e re)

{-|
Eksperiments B: apmainīti virzieni (mazākās vērtības tiek liktas pa labi).
-}
insertSwapped :: Ord a => a -> Tree a -> Tree a
insertSwapped e Leaf = Branch e Leaf Leaf
insertSwapped e (Branch x li re)
  | e <= x    = Branch x li (insertSwapped e re)
  | otherwise = Branch x (insertSwapped e li) re

{-|
Eksperiments C: @<@ nevis @<=@ - vienādās vērtības tiek liktas pa labi.
-}
insertStrict :: Ord a => a -> Tree a -> Tree a
insertStrict e Leaf = Branch e Leaf Leaf
insertStrict e (Branch x li re)
  | e < x     = Branch x (insertStrict e li) re
  | otherwise = Branch x li (insertStrict e re)

{-|
Eksperiments D: ejot pa labi, aizmirsta rekursija - labais apakškoks tiek
aizstāts ar jauno mezglu, un tā elementi pazūd.
-}
insertLosing :: Ord a => a -> Tree a -> Tree a
insertLosing e Leaf = Branch e Leaf Leaf
insertLosing e (Branch x li re)
  | e <= x    = Branch x (insertLosing e li) re
  | otherwise = Branch x li (Branch e Leaf Leaf)

{-|
Izveido koku no saraksta, ievietojot elementus pa vienam ar 'insertTree'.

Parametri:

* @xs@ - elementu saraksts.

Atgrieztā vērtība:

* binārais meklēšanas koks.
-}
fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insertTree) Leaf

-- | Ievietošanas funkcijas tips eksperimentiem.
type Insert = Int -> Tree Int -> Tree Int

{-|
Īpašība P1: ja @t@ ir BMK, tad arī @insert e t@ ir BMK.
Ja @t@ nav BMK, implikācija ir patiesa.

Parametri:

* @insert@ - pārbaudāmā ievietošanas funkcija;
* @e@ - ievietojamā vērtība;
* @t@ - koks.

Atgrieztā vērtība:

* 'True', ja īpašība izpildās.
-}
propSearchTree :: Insert -> Int -> Tree Int -> Bool
propSearchTree insert e t = not (isSearchTree t) || isSearchTree (insert e t)

{-|
Īpašība P2: ievietošana nepazaudē un neizdomā elementus - rezultāta
elementi ir vecie elementi un @e@ (sakārtoti).

Parametri un atgrieztā vērtība - kā funkcijai 'propSearchTree'.
-}
propElements :: Insert -> Int -> Tree Int -> Bool
propElements insert e t =
  not (isSearchTree t) || inorder (insert e t) == sort (e : inorder t)

-- | Visi koki ar tieši n mezgliem, kuru vērtības ņemtas no saraksta.
allTrees :: [a] -> Int -> [Tree a]
allTrees _      0 = [Leaf]
allTrees values n =
  [ Branch v l r | k <- [0 .. n - 1]
                 , l <- allTrees values k
                 , r <- allTrees values (n - 1 - k)
                 , v <- values ]

-- | Visi koki ar 0..4 mezgliem un vērtībām 1..3 (1291 koks), sakārtoti
--   pēc izmēra, tāpēc pirmais atrastais pretpiemērs ir mazs.
smallTrees :: [Tree Int]
smallTrees = concatMap (allTrees [1, 2, 3]) [0 .. 4]

-- | Lineārais kongruentais pseidogadījuma skaitļu ģenerators.
nextRandom :: Int -> Int
nextRandom s = (s * 1103515245 + 12345) `mod` 2147483648

-- | k pseidogadījuma saraksti: garums 0..40, vērtības -50..50.
randomLists :: Int -> Int -> [[Int]]
randomLists _    0 = []
randomLists seed k = list : randomLists nextSeed (k - 1)
  where
    s1       = nextRandom seed
    len      = s1 `mod` 41
    numbers  = take len (drop 1 (iterate nextRandom s1))
    list     = [ x `mod` 101 - 50 | x <- numbers ]
    nextSeed = nextRandom (last (s1 : numbers))

-- | Visi testa gadījumi (e, t), kur t ir BMK: visi mazie BMK ar e no 0..4
--   un 300 lieli koki no pseidogadījuma sarakstiem ar piecām e vērtībām.
testCases :: [(Int, Tree Int)]
testCases =
     [ (e, t) | t <- filter isSearchTree smallTrees, e <- [0 .. 4] ]
  ++ [ (e, fromList xs) | xs <- randomLists 2026 300, e <- [-60, -1, 0, 7, 60] ]

{-|
Meklē pretpiemēru īpašībai visos testa gadījumos.

Parametri:

* @property@ - īpašība, kas jāpārbauda pārim (e, t).

Atgrieztā vērtība:

* @Just (e, t)@ - pirmais gadījums, kurā īpašība neizpildās;
* @Nothing@ - ja īpašība izpildās visos gadījumos.
-}
counterexample :: (Int -> Tree Int -> Bool) -> Maybe (Int, Tree Int)
counterexample property =
  case [ (e, t) | (e, t) <- testCases, not (property e t) ] of
    []      -> Nothing
    (c : _) -> Just c

{-|
Sagatavo koku izdrukai \"uz sāniem\": labais apakškoks augšā, kreisais apakšā.

Parametri:

* @tree@ - koks.

Atgrieztā vērtība:

* teksta rindu saraksts.
-}
drawTree :: Show a => Tree a -> [String]
drawTree Leaf = ["    (empty tree)"]
drawTree tree = go "    " tree
  where
    go _ Leaf = []
    go indent (Branch x l r) =
         go (indent ++ "      ") r
      ++ [indent ++ "-- " ++ show x]
      ++ go (indent ++ "      ") l

-- | Koks no uzdevuma piemēra un koks ar kļūdu dziļāk kokā.
exampleTree, brokenTree :: Tree Int
exampleTree = fromList [5, 3, 8, 1, 4, 7, 9, 3]
brokenTree  = Branch 5 (Branch 3 Leaf (Branch 6 Leaf Leaf)) (Branch 8 Leaf Leaf)

-- | Funkcijas 'isSearchTree' testi, ieskaitot robežgadījumus.
functionTests :: [TestResult]
functionTests =
  [ testCase "Empty tree" "Leaf" True (isSearchTree (Leaf :: Tree Int))
  , testCase "Single node" "Branch 5 Leaf Leaf" True
      (isSearchTree (Branch 5 Leaf Leaf :: Tree Int))
  , testCase "Tree built with insertTree" "fromList [5,3,8,1,4,7,9,3]" True
      (isSearchTree exampleTree)
  , testCase "Equal value in the left subtree" "Branch 5 (Branch 5 Leaf Leaf) Leaf" True
      (isSearchTree (Branch 5 (Branch 5 Leaf Leaf) Leaf :: Tree Int))
  , testCase "Equal value in the right subtree" "Branch 5 Leaf (Branch 5 Leaf Leaf)" False
      (isSearchTree (Branch 5 Leaf (Branch 5 Leaf Leaf) :: Tree Int))
  , testCase "Wrong direct child" "Branch 5 (Branch 7 Leaf Leaf) Leaf" False
      (isSearchTree (Branch 5 (Branch 7 Leaf Leaf) Leaf :: Tree Int))
  , testCase "Error deeper in the tree (6 > 5 in the left subtree)" (show brokenTree) False
      (isSearchTree brokenTree)
  , testCase "Same result as the naive version" "all 1291 trees with 0..4 nodes" True
      (all (\t -> isSearchTree t == isSearchTreeNaive t) smallTrees)
  ]

-- | Īpašību pārbaude oriģinālajai un izmainītajām 'insertTree' versijām.
propertyTests :: [TestResult]
propertyTests =
  [ testCase "A) original insertTree: P1 holds" cases True (holds (propSearchTree insertTree))
  , testCase "A) original insertTree: P2 holds" cases True (holds (propElements insertTree))
  , testCase "B) directions swapped: P1 finds the bug" cases True
      (found (propSearchTree insertSwapped))
  , testCase "C) '<' instead of '<=': P1 finds the bug" cases True
      (found (propSearchTree insertStrict))
  , testCase "D) recursion forgotten: P1 does NOT find the bug" cases True
      (holds (propSearchTree insertLosing))
  , testCase "D) recursion forgotten: P2 finds the bug" cases True
      (found (propElements insertLosing))
  ]
  where
    cases    = show (length testCases) ++ " pairs (value, search tree)"
    holds p  = counterexample p == Nothing
    found p  = counterexample p /= Nothing

-- | Izdrukā pretpiemēru izmainītajai versijai.
printCounterexample :: String -> Insert -> (Insert -> Int -> Tree Int -> Bool) -> IO ()
printCounterexample title insert property =
  case counterexample (property insert) of
    Nothing     -> putStrLn (title ++ ": no counterexample")
    Just (e, t) -> do
      let result = insert e t
      putStrLn (title ++ ":")
      putStrLn ("  insert " ++ show e ++ " into " ++ show t)
      putStrLn ("  result: " ++ show result)
      putStrLn ("  elements: " ++ show (inorder result)
                ++ ", expected: " ++ show (sort (e : inorder t))
                ++ ", search tree: " ++ (if isSearchTree result then "yes" else "no"))

{-|
Uzdevuma Nr. 7 izpilde no galvenās izvēlnes: testi, eksperimenti un
interaktīvā daļa.
-}
runTask7 :: IO ()
runTask7 = do
  putStrLn "--- Task 7: Binary Search Trees ---"
  putStrLn ""
  putStrLn "Example tree fromList [5,3,8,1,4,7,9,3] (right subtree on top):"
  mapM_ putStrLn (drawTree exampleTree)
  putStrLn ""
  _ <- runTests "isSearchTree tests (edge cases)" functionTests
  putStrLn "P1: if t is a search tree, then insert e t is a search tree"
  putStrLn "P2: if t is a search tree, then inorder (insert e t) == sort (e : inorder t)"
  putStrLn ""
  _ <- runTests "Property experiments" propertyTests
  putStrLn "=== Counterexamples ==="
  printCounterexample "B) directions swapped, P1" insertSwapped propSearchTree
  printCounterexample "C) '<' instead of '<=', P1" insertStrict propSearchTree
  printCounterexample "D) recursion forgotten, P2" insertLosing propElements
  putStrLn ""
  interactivePart

-- Interaktīvā daļa: lietotājs izveido koku un ievieto vērtību.
interactivePart :: IO ()
interactivePart = do
  putStrLn "=== Try it yourself ==="
  putStrLn "Enter integers to build a tree, e.g. 5, 3, 8, 1."
  putStrLn "Press Enter on an empty line to use the example list."
  loop
  where
    loop = do
      input <- promptUntilValid "Numbers: " (withDefault [5, 3, 8, 1, 4, 7, 9, 3] parseIntList)
      case input of
        Nothing      -> return ()
        Just numbers -> do
          let tree = fromList numbers
          putStrLn "Tree:"
          mapM_ putStrLn (drawTree tree)
          putStrLn ("isSearchTree: " ++ show (isSearchTree tree))
          value <- promptUntilValid "Value to insert: " parseInt
          case value of
            Nothing -> return ()
            Just e  -> do
              let newTree = insertTree e tree
              putStrLn "Tree after insertTree:"
              mapM_ putStrLn (drawTree newTree)
              putStrLn ("isSearchTree: " ++ show (isSearchTree newTree))
              putStrLn ("P1 holds: " ++ show (propSearchTree insertTree e tree)
                        ++ ", P2 holds: " ++ show (propElements insertTree e tree))
              again <- askYesNo "Try another tree? (y/n): "
              when again loop
