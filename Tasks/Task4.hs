{-|
Module      : Tasks.Task4
Description : Saraksta pēdējā elementa atrašana.

Uzdevums Nr. 4 (variants 7-8).

Modulis nodrošina funkciju, kas atgriež saraksta pēdējo elementu.
Funkcija ir polimorfa - tā darbojas ar jebkura tipa sarakstu
(skaitļi, simboli, loģiskās vērtības).

Funkcija atgriež tipu 'Maybe', jo tukšam sarakstam pēdējā elementa
nav (standarta funkcija 'last' šajā gadījumā izraisa izpildes kļūdu).

Robežgadījumi, kas tiek pārbaudīti testos:

* tukšs saraksts, viens elements, divi elementi;
* pēdējais elements atkārtojas, simbolu saraksts;
* garš saraksts (100000 elementu);
* nekorekta lietotāja ievade (burti, trūkstošas iekavas, tukša ievade).
-}
module Tasks.Task4
  ( lastElement
  , lastElementFold
  , lastElementMessage
  , runTask4
  ) where

import Control.Monad (when)

import Utils.Input (askYesNo, parseIntList, promptUntilValid, withDefault)
import Utils.Testing

{-|
Atgriež saraksta pēdējo elementu (rekursīvs risinājums).

Saraksta sākums tiek atmests, līdz paliek viens elements.

Parametri:

* @xs@ - jebkura tipa saraksts.

Atgrieztā vērtība:

* @Just x@ - pēdējais elements, ja saraksts nav tukšs;
* @Nothing@ - ja saraksts ir tukšs.

Sarežģītība:

* Laika sarežģītība: \(O(n)\).
* Papildu atmiņa: \(O(1)\) (astes rekursija).
-}
lastElement :: [a] -> Maybe a
lastElement []       = Nothing
lastElement [x]      = Just x            -- palicis viens elements - tas ir pēdējais
lastElement (_ : xs) = lastElement xs    -- citādi meklē saraksta astē

{-|
Alternatīvs risinājums ar reducēšanu 'foldl': katrs nākamais elements
aizstāj iepriekšējo, tāpēc beigās paliek pēdējais.

Parametri:

* @xs@ - jebkura tipa saraksts.

Atgrieztā vērtība:

* tāda pati kā funkcijai 'lastElement'.

Sarežģītība:

* Laika sarežģītība: \(O(n)\).
-}
lastElementFold :: [a] -> Maybe a
lastElementFold = foldl (\_ x -> Just x) Nothing

{-|
Izveido jēgpilnu teikumu ar rezultātu.

Parametri:

* @numbers@ - veselu skaitļu saraksts.

Atgrieztā vērtība:

* teikums ar pēdējo elementu vai paskaidrojums, ka saraksts ir tukšs.
-}
lastElementMessage :: [Int] -> String
lastElementMessage numbers =
  case lastElement numbers of
    Just x  -> "The last element of the list is " ++ show x ++ "."
    Nothing -> "The list is empty, so it has no last element."

-- | Uzdevuma piemēra saraksts.
exampleList :: [Int]
exampleList = [5, 10, 11, 15, 3, 8]

-- | Funkcijas 'lastElement' testi, ieskaitot robežgadījumus.
functionTests :: [TestResult]
functionTests =
  [ testCase "Normal list" (show exampleList) (Just 8) (lastElement exampleList)
  , testCase "Empty list" "[]" Nothing (lastElement ([] :: [Int]))
  , testCase "Single element" "[42]" (Just 42) (lastElement [42 :: Int])
  , testCase "Two elements" "[1,2]" (Just 2) (lastElement [1, 2 :: Int])
  , testCase "Last element repeats" "[3,7,7]" (Just 7) (lastElement [3, 7, 7 :: Int])
  , testCase "List of characters" "\"Haskell\"" (Just 'l') (lastElement "Haskell")
  , testCase "List of Bool values" "[True,False]" (Just False) (lastElement [True, False])
  , testCase "Long list" "[1..100000]" (Just 100000) (lastElement [1 .. 100000 :: Int])
  , testCase "Same result as foldl version and standard 'last'" "[1..n], n = 1..300" True
      (and [ lastElement xs == Just (last xs) && lastElementFold xs == Just (last xs)
           | n <- [1 .. 300 :: Int], let xs = [1 .. n] ])
  , testCase "foldl version on an empty list" "[]" Nothing (lastElementFold ([] :: [Int]))
  ]

-- | Lietotāja ievades pārbaudes testi (robežgadījumi).
inputTests :: [TestResult]
inputTests =
  [ test "Valid list" "1, 2, 3" (Right [1, 2, 3])
  , test "Empty list in brackets" "[]" (Right [])
  , test "Word instead of a number" "1, two, 3"
      (Left "element 2: 'two' is not a number (letters are not allowed)")
  , test "Number with a letter" "1 2x 3"
      (Left "element 2: '2x' is not a number (letters are not allowed)")
  , test "Missing opening bracket" "1, 2]" (Left "missing opening bracket '['")
  , test "Empty input" "" (Left "empty input - a list of numbers was expected")
  ]
  where
    test name input expected =
      testCaseWith (showEither show) name (showText input) expected (parseIntList input)

{-|
Uzdevuma Nr. 4 izpilde no galvenās izvēlnes: testi un interaktīvā daļa.
-}
runTask4 :: IO ()
runTask4 = do
  putStrLn "--- Task 4: Last Element of a List ---"
  putStrLn ""
  _ <- runTests "Function tests" functionTests
  _ <- runTests "Input validation tests (edge cases)" inputTests
  interactivePart

-- Interaktīvā daļa: lietotājs ievada savu sarakstu.
interactivePart :: IO ()
interactivePart = do
  putStrLn "=== Try it yourself ==="
  putStrLn "Enter integers separated by commas or spaces; [] is an empty list."
  putStrLn "Press Enter on an empty line to use the task example."
  loop
  where
    loop = do
      input <- promptUntilValid "List: " (withDefault exampleList parseIntList)
      case input of
        Nothing      -> return ()
        Just numbers -> do
          putStrLn ("Input list: " ++ show numbers)
          putStrLn (lastElementMessage numbers)
          again <- askYesNo "Try another list? (y/n): "
          when again loop
