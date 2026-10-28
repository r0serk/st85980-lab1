{-|
Module      : Tasks.Task8
Description : Likuma map f . concat = concat . map (map f) pierādījums un pārbaude.

Uzdevums Nr. 8.

Modulis demonstrē un pārbauda likumu par sarakstu funkcijām:

@
map f . concat = concat . map (map f)
@

Pierādījums ir atskaitē un īsumā arī šeit. Programma likumu pārbauda
konkrētos piemēros; pārbaude nav pierādījums, bet apstiprina, ka
pierādītais likums izpildās.

Izmantotās definīcijas:

@
(map.1)    map f []          = []
(map.2)    map f (x : xs)    = f x : map f xs
(concat.1) concat []         = []
(concat.2) concat (xs : xss) = xs ++ concat xss
(++.1)     [] ++ ys          = ys
(++.2)     (x : xs) ++ ys    = x : (xs ++ ys)
@

Palīglemma: @map f (xs ++ ys) = map f xs ++ map f ys@, indukcija pēc @xs@.

* Bāze @xs = []@: abas puses ir @map f ys@ pēc (++.1) un (map.1).
* Solis @xs = x : xs'@: @map f ((x : xs') ++ ys) = f x : map f (xs' ++ ys)@
  pēc (++.2) un (map.2); pēc indukcijas pieņēmuma tas ir
  @f x : (map f xs' ++ map f ys) = map f (x : xs') ++ map f ys@.

Galvenais pierādījums: @map f (concat xss) = concat (map (map f) xss)@,
indukcija pēc @xss@.

* Bāze @xss = []@: abas puses ir @[]@ pēc (concat.1) un (map.1).
* Solis @xss = ys : yss@: kreisā puse ir
  @map f (ys ++ concat yss) = map f ys ++ map f (concat yss)@ (palīglemma),
  un pēc indukcijas pieņēmuma tas ir @map f ys ++ concat (map (map f) yss)@.
  Labā puse pēc (map.2) un (concat.2) ir tieši tas pats.

Robežgadījumi, kas tiek pārbaudīti testos: tukšs ārējais saraksts, tikai
tukši iekšējie saraksti, funkcija, kas maina elementu tipu, bezgalīgs
saraksts (slinkā novērtēšana) un nekorekta lietotāja ievade.
-}
module Tasks.Task8
  ( leftSide
  , rightSide
  , checkLaw
  , checkLemma
  , parseListOfLists
  , runTask8
  ) where

import Control.Monad (when)
import Data.Char (isAlpha)
import Text.Read (readMaybe)

import Utils.Input (askYesNo, promptUntilValid, trim, withDefault)
import Utils.Testing

{-|
Likuma kreisā puse: vispirms saplacina sarakstu, tad pielieto @f@.

Parametri:

* @f@ - funkcija, ko pielieto katram elementam;
* @xss@ - sarakstu saraksts.

Atgrieztā vērtība:

* @map f (concat xss)@.
-}
leftSide :: (a -> b) -> [[a]] -> [b]
leftSide f = map f . concat

{-|
Likuma labā puse: pielieto @f@ katrā apakšsarakstā, tad saplacina.

Parametri:

* @f@ - funkcija, ko pielieto katram elementam;
* @xss@ - sarakstu saraksts.

Atgrieztā vērtība:

* @concat (map (map f) xss)@.
-}
rightSide :: (a -> b) -> [[a]] -> [b]
rightSide f = concat . map (map f)

-- | Visi sarakstu saraksti: ārējais garums 0..3, iekšējie garumi 0..3,
--   elementi 1..3 (1 + 40 + 1600 + 64000 = 65641 gadījums).
allListsOfLists :: [[[Int]]]
allListsOfLists = concatMap outer [0 .. 3]
  where
    inner = concatMap ofLength [0 .. 3]
    ofLength 0 = [[]]
    ofLength k = [ c : r | c <- [1, 2, 3], r <- ofLength (k - 1) ]
    outer 0 = [[]]
    outer k = [ ys : r | ys <- inner, r <- outer (k - 1) ]

{-|
Pārbauda likumu dotajai funkcijai visos 65641 gadījumos.

Parametri:

* @f@ - funkcija no 'Int' uz salīdzināmu tipu.

Atgrieztā vērtība:

* 'True', ja abas likuma puses visos gadījumos sakrīt.
-}
checkLaw :: Eq b => (Int -> b) -> Bool
checkLaw f = all (\xss -> leftSide f xss == rightSide f xss) allListsOfLists

{-|
Pārbauda palīglemmu @map f (xs ++ ys) == map f xs ++ map f ys@ visiem
sarakstu pāriem no iekšējo sarakstu kopas.

Parametri:

* @f@ - funkcija no 'Int' uz salīdzināmu tipu.

Atgrieztā vērtība:

* 'True', ja lemma izpildās visos gadījumos.
-}
checkLemma :: Eq b => (Int -> b) -> Bool
checkLemma f =
  and [ map f (xs ++ ys) == map f xs ++ map f ys
      | [xs, ys] <- filter ((== 2) . length) allListsOfLists ]

{-|
Pārveido lietotāja ievadi sarakstu sarakstā.

Parametri:

* @raw@ - lietotāja ievadītā virkne, piem. @[[1,2],[3],[],[4,5]]@.

Atgrieztā vērtība:

* @Right xss@ - korekts sarakstu saraksts;
* @Left err@ - ja ievade ir tukša, satur burtus, ir nepareizā formātā
  vai satur skaitli ārpus tipa 'Int' diapazona.
-}
parseListOfLists :: String -> Either String [[Int]]
parseListOfLists raw
  | null text = Left "empty input - a list of lists was expected"
  | otherwise =
      case readMaybe text :: Maybe [[Integer]] of
        Nothing
          | any isAlpha text -> Left "letters are not allowed - the elements must be integers"
          | otherwise        -> Left formatError
        Just xss
          | any (any outOfRange) xss -> Left "a number is outside the Int range"
          | otherwise                -> Right (map (map fromInteger) xss)
  where
    text = trim raw
    outOfRange n = n < toInteger (minBound :: Int) || n > toInteger (maxBound :: Int)

-- | Kļūdas paziņojums nepareizam formātam.
formatError :: String
formatError = "invalid format - expected a list of lists, e.g. [[1,2],[3],[],[4,5]]"

-- | Uzdevuma piemērs.
exampleLists :: [[Int]]
exampleLists = [[1, 2], [3], [], [4, 5]]

-- | Likuma testi: Expected = labā puse, Actual = kreisā puse.
lawTests :: [TestResult]
lawTests =
  [ test "Task example" (show exampleLists) exampleLists
  , test "Empty outer list" "[]" []
  , test "Only empty inner lists" "[[],[],[]]" [[], [], []]
  , test "Single inner list" "[[1,2,3]]" [[1, 2, 3]]
  , testCase "Function that changes the type, f = show" "[[1,2],[3]]"
      (rightSide show [[1, 2], [3 :: Int]]) (leftSide show [[1, 2], [3 :: Int]])
  , testCase "Infinite list (first 6 elements)" "repeat [1,2]"
      (take 6 (rightSide (* 10) (repeat [1, 2 :: Int])))
      (take 6 (leftSide (* 10) (repeat [1, 2 :: Int])))
  , testCase "Law holds for 5 functions" "all 65641 lists of lists" True
      (checkLaw (* 10) && checkLaw (+ 1) && checkLaw even && checkLaw show
       && checkLaw (\x -> (x, x * x)))
  , testCase "Lemma map f (xs ++ ys) == map f xs ++ map f ys" "all 1600 pairs (xs, ys)" True
      (checkLemma (* 10) && checkLemma show)
  ]
  where
    test name input xss = testCase (name ++ ", f = (* 10)") input
                            (rightSide (* 10) xss) (leftSide (* 10) xss)

-- | Lietotāja ievades pārbaudes testi (robežgadījumi).
inputTests :: [TestResult]
inputTests =
  [ test "Valid list of lists" "[[1,2],[3],[]]" (Right [[1, 2], [3], []])
  , test "Letter instead of a number" "[[1,a]]"
      (Left "letters are not allowed - the elements must be integers")
  , test "Missing bracket" "[[1,2],[3]" (Left formatError)
  , test "Flat list instead of a list of lists" "[1,2,3]" (Left formatError)
  , test "Empty input" "" (Left "empty input - a list of lists was expected")
  ]
  where
    test name input expected =
      testCaseWith (showEither show) name (showText input) expected (parseListOfLists input)

{-|
Uzdevuma Nr. 8 izpilde no galvenās izvēlnes: testi un interaktīvā daļa.
-}
runTask8 :: IO ()
runTask8 = do
  putStrLn "--- Task 8: map f . concat = concat . map (map f) ---"
  putStrLn ""
  putStrLn "(Expected = right side concat . map (map f), Actual = left side map f . concat)"
  putStrLn ""
  _ <- runTests "Law tests" lawTests
  _ <- runTests "Input validation tests (edge cases)" inputTests
  interactivePart

-- Interaktīvā daļa: lietotājs ievada savu sarakstu sarakstu.
interactivePart :: IO ()
interactivePart = do
  putStrLn "=== Try it yourself (f = (* 10)) ==="
  putStrLn "Enter a list of lists, e.g. [[1,2],[3],[],[4,5]]."
  putStrLn "Press Enter on an empty line to use the task example."
  loop
  where
    f = (* 10) :: Int -> Int
    loop = do
      input <- promptUntilValid "List of lists: " (withDefault exampleLists parseListOfLists)
      case input of
        Nothing  -> return ()
        Just xss -> do
          putStrLn "Left side  (map f . concat) xss:"
          putStrLn ("  concat xss               = " ++ show (concat xss))
          putStrLn ("  map f (concat xss)       = " ++ show (leftSide f xss))
          putStrLn "Right side  (concat . map (map f)) xss:"
          putStrLn ("  map (map f) xss          = " ++ show (map (map f) xss))
          putStrLn ("  concat (map (map f) xss) = " ++ show (rightSide f xss))
          putStrLn ("Both sides are equal: " ++ show (leftSide f xss == rightSide f xss))
          again <- askYesNo "Try another list? (y/n): "
          when again loop
