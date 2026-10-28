{-|
Module      : Tasks.Task1
Description : Maksimālā skaitļa atrašana veselu skaitļu sarakstā.

Uzdevums Nr. 1 (8. variants, 2. risinājums).

Modulis nodrošina funkciju lielākās vērtības noteikšanai veselu skaitļu
sarakstā. Atbilstoši 2. risinājumam algoritms vienreiz iziet cauri
visiem saraksta elementiem un glabā līdz šim lielāko atrasto vērtību.
Saraksts netiek kārtots.

Funkcija atgriež tipu 'Maybe', lai tukša saraksta gadījumu varētu
apstrādāt bez izņēmuma (standarta funkcija 'maximum' tukšam sarakstam
izraisa izpildes kļūdu).

Robežgadījumi, kas tiek pārbaudīti testos:

* tukšs saraksts, viens elements, visi elementi vienādi;
* tikai negatīvi skaitļi, maksimums atkārtojas;
* maksimums saraksta sākumā vai beigās;
* tipa 'Int' robežvērtības ('minBound', 'maxBound');
* nekorekta lietotāja ievade (burti, decimāldaļskaitļi, tukši elementi,
  pārāk lieli skaitļi, trūkstošas iekavas).
-}
module Tasks.Task1
  ( findMaximum
  , maximumMessage
  , maximumTrace
  , runTask1
  ) where

import Control.Monad (when)

import Utils.Input (askYesNo, parseIntList, promptUntilValid, withDefault)
import Utils.Testing

{-|
Atrod lielāko elementu sarakstā, iterējot cauri visiem elementiem.

Pirmais elements tiek pieņemts par sākotnējo maksimumu. Katrs nākamais
elements tiek salīdzināts ar līdz šim lielāko; ja tas ir lielāks, tas
kļūst par jauno maksimumu.

Parametri:

* @numbers@ - saraksts, kurā meklē maksimumu (elementu tipam jābūt
  salīdzināmam, klase 'Ord').

Atgrieztā vērtība:

* @Just maximumNumber@ - ja sarakstā ir vismaz viens elements;
* @Nothing@ - ja saraksts ir tukšs.

Sarežģītība:

* Laika sarežģītība: \(O(n)\) - katrs elements tiek apskatīts vienu reizi.
* Papildu atmiņa: \(O(1)\).
-}
findMaximum :: Ord a => [a] -> Maybe a
findMaximum []             = Nothing
findMaximum (first : rest) = Just (iterateList first rest)
  where
    -- best - līdz šim lielākais atrastais elements.
    iterateList best []       = best
    iterateList best (x : xs)
      | x > best  = iterateList x xs      -- atrasts jauns maksimums
      | otherwise = iterateList best xs   -- maksimums nemainās

{-|
Izveido jēgpilnu teikumu ar rezultātu (uzdevuma prasība).

Parametri:

* @numbers@ - veselu skaitļu saraksts.

Atgrieztā vērtība:

* @"The maximum number is X."@ - ja saraksts nav tukšs;
* paskaidrojums, ka maksimuma nav - ja saraksts ir tukšs.
-}
maximumMessage :: [Int] -> String
maximumMessage numbers =
  case findMaximum numbers of
    Just maximumNumber -> "The maximum number is " ++ show maximumNumber ++ "."
    Nothing            -> "The list is empty, so there is no maximum number."

{-|
Atkļūdošanas funkcija: apraksta katru iterācijas soli.

Parametri:

* @numbers@ - veselu skaitļu saraksts.

Atgrieztā vērtība:

* rindu saraksts, kur katra rinda parāda apskatīto elementu un
  līdz šim lielāko vērtību pēc šī soļa.
-}
maximumTrace :: [Int] -> [String]
maximumTrace []             = ["  (empty list - nothing to iterate over)"]
maximumTrace (first : rest) = firstStep : steps first (zip [2 ..] rest)
  where
    firstStep = "  Step 1: element " ++ show first ++ " -> current maximum " ++ show first
    steps _ [] = []
    steps best ((stepNumber, x) : more) =
      let newBest = max best x
          note    = if x > best then "  (new maximum)" else ""
      in ("  Step " ++ show (stepNumber :: Int) ++ ": element " ++ show x
          ++ " -> current maximum " ++ show newBest ++ note)
         : steps newBest more

-- | Uzdevuma piemēra saraksts.
exampleList :: [Int]
exampleList = [5, 10, 11, 15, 3, 8]

-- | Funkcijas 'findMaximum' testi, ieskaitot robežgadījumus.
functionTests :: [TestResult]
functionTests =
  [ testCase "Normal list (task example)" (show exampleList) (Just 15) (findMaximum exampleList)
  , testCase "Empty list" "[]" Nothing (findMaximum ([] :: [Int]))
  , testCase "Single element" "[7]" (Just 7) (findMaximum [7 :: Int])
  , testCase "Negative numbers only" "[-10,-3,-25,-1]" (Just (-1))
      (findMaximum [-10, -3, -25, -1 :: Int])
  , testCase "Maximum appears twice" "[4,12,7,12,3]" (Just 12)
      (findMaximum [4, 12, 7, 12, 3 :: Int])
  , testCase "Maximum at the beginning" "[99,1,2,3]" (Just 99) (findMaximum [99, 1, 2, 3 :: Int])
  , testCase "Maximum at the end" "[1,2,3,99]" (Just 99) (findMaximum [1, 2, 3, 99 :: Int])
  , testCase "All elements equal" "[5,5,5]" (Just 5) (findMaximum [5, 5, 5 :: Int])
  , testCase "Int limits" "[minBound, 0, maxBound]" (Just (maxBound :: Int))
      (findMaximum [minBound, 0, maxBound :: Int])
  , testCase "Same result as standard 'maximum'" "200 lists [k, k-1 .. -k] and [-k .. k]" True
      (and [ findMaximum xs == Just (maximum xs)
           | k <- [1 .. 100 :: Int], xs <- [[k, k - 1 .. -k], [-k .. k]] ])
  ]

-- | Lietotāja ievades pārbaudes testi (robežgadījumi).
inputTests :: [TestResult]
inputTests =
  [ test "Numbers separated by commas" "5, 10, 11" (Right [5, 10, 11])
  , test "Numbers separated by spaces" "5 10 11" (Right [5, 10, 11])
  , test "List in brackets" "[5,10,11,15,3,8]" (Right exampleList)
  , test "Empty list in brackets" "[]" (Right [])
  , test "Letter instead of a number" "5, a, 3"
      (Left "element 2: 'a' is not a number (letters are not allowed)")
  , test "Decimal number" "5, 3.5, 2" (Left "element 2: '3.5' is not a whole number")
  , test "Empty element (two commas)" "5,,3" (Left "element 2 is empty")
  , test "Number too large for Int" "1, 99999999999999999999"
      (Left ("element 2: '99999999999999999999' is outside the Int range ("
             ++ show (minBound :: Int) ++ " .. " ++ show (maxBound :: Int) ++ ")"))
  , test "Missing closing bracket" "[5, 10" (Left "missing closing bracket ']'")
  , test "Empty input" "" (Left "empty input - a list of numbers was expected")
  ]
  where
    test name input expected =
      testCaseWith (showEither show) name (showText input) expected (parseIntList input)

{-|
Uzdevuma Nr. 1 izpilde no galvenās izvēlnes: testi un interaktīvā daļa.
-}
runTask1 :: IO ()
runTask1 = do
  putStrLn "--- Task 1: Find Maximum ---"
  putStrLn ""
  _ <- runTests "Function tests" functionTests
  _ <- runTests "Input validation tests (edge cases)" inputTests
  interactivePart

-- Interaktīvā daļa: lietotājs ievada savu sarakstu.
interactivePart :: IO ()
interactivePart = do
  putStrLn "=== Try it yourself ==="
  putStrLn "Enter integers separated by commas or spaces, e.g. 5, 10, 11 or [5,10,11]."
  putStrLn "Press Enter on an empty line to use the task example."
  loop
  where
    loop = do
      input <- promptUntilValid "List: " (withDefault exampleList parseIntList)
      case input of
        Nothing      -> return ()
        Just numbers -> do
          putStrLn ("Input list: " ++ show numbers)
          putStrLn "Iteration steps:"
          mapM_ putStrLn (maximumTrace numbers)
          putStrLn (maximumMessage numbers)
          again <- askYesNo "Try another list? (y/n): "
          when again loop
