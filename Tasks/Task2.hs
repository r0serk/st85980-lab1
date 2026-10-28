{-|
Module      : Tasks.Task2
Description : Ievadītā vārda simbolu apgriešana.

Uzdevums Nr. 2 (8. variants, 2. risinājums).

Modulis nodrošina funkciju, kas apgriež vārda simbolus, apstaigājot tos
atgriezeniskā secībā - no pēdējā uz pirmo. Vārds tiek nolasīts no
lietotāja, un pirms apgriešanas ievade tiek pārbaudīta.

Robežgadījumi, kas tiek pārbaudīti testos:

* viena simbola vārds, palindroms, latviešu burti, burti kopā ar cipariem;
* tukša ievade un ievade tikai no atstarpēm;
* vairāki vārdi vienā rindā;
* ievade bez neviena burta (piem. tikai cipari);
* atstarpes pirms un pēc vārda (tiek noņemtas).
-}
module Tasks.Task2
  ( reverseWord
  , reverseWordFast
  , validateWord
  , isPalindrome
  , visitOrder
  , runTask2
  ) where

import Control.Monad (when)
import Data.Char (isAlpha, isSpace, toLower)

import Utils.Input (askYesNo, promptUntilValid, trim)
import Utils.Testing

{-|
Apgriež vārdu, apstaigājot simbolus no pēdējā uz pirmo (2. risinājums).

Indekss @i@ mainās no @n - 1@ līdz @0@, un katrā solī rezultātam tiek
pievienots simbols ar šo indeksu.

Parametri:

* @word@ - apgriežamais vārds.

Atgrieztā vērtība:

* vārds ar simboliem apgrieztā secībā; tukšai virknei - tukša virkne.

Sarežģītība:

* Laika sarežģītība: \(O(n^2)\), jo piekļuve sarakstam pēc indeksa
  (@!!@) ir \(O(i)\). Vārdiem tas nav būtiski; efektīvāka versija
  ir 'reverseWordFast'.
-}
reverseWord :: String -> String
reverseWord word = [ word !! i | i <- [n - 1, n - 2 .. 0] ]
  where
    n = length word

{-|
Apgriež vārdu ar akumulatoru: katrs nākamais simbols tiek likts
rezultāta priekšā, tāpēc pēdējais simbols nonāk pirmajā vietā.

Izmantota rezultātu salīdzināšanai ar 'reverseWord'.

Parametri:

* @word@ - apgriežamais vārds.

Atgrieztā vērtība:

* apgrieztais vārds.

Sarežģītība:

* Laika sarežģītība: \(O(n)\).
-}
reverseWordFast :: String -> String
reverseWordFast = go []
  where
    go accumulator []       = accumulator
    go accumulator (c : cs) = go (c : accumulator) cs

{-|
Pārbauda lietotāja ievadi pirms apgriešanas.

Parametri:

* @raw@ - lietotāja ievadītā virkne.

Atgrieztā vērtība:

* @Right word@ - vārds bez atstarpēm sākumā un beigās;
* @Left err@ - ja ievade ir tukša, satur vairākus vārdus vai
  nesatur nevienu burtu.
-}
validateWord :: String -> Either String String
validateWord raw
  | null word              = Left "empty input - a word was expected"
  | any isSpace word       = Left ("'" ++ word ++ "' contains spaces - please enter a single word")
  | not (any isAlpha word) = Left ("'" ++ word ++ "' contains no letters"
                                   ++ " - a word must contain at least one letter")
  | otherwise              = Right word
  where
    word = trim raw

{-|
Pārbauda, vai vārds ir palindroms (lasās vienādi no abām pusēm),
neatkarīgi no lielajiem un mazajiem burtiem.

Parametri:

* @word@ - pārbaudāmais vārds.

Atgrieztā vērtība:

* 'True', ja vārds ir palindroms, citādi 'False'.
-}
isPalindrome :: String -> Bool
isPalindrome word = lowered == reverseWord lowered
  where
    lowered = map toLower word

{-|
Atkļūdošanas funkcija: parāda, kādā secībā simboli tiek apstaigāti.

Parametri:

* @word@ - vārds.

Atgrieztā vērtība:

* virkne formā @indekss:simbols@, piem. @"2:c 1:b 0:a"@ vārdam @"abc"@.
-}
visitOrder :: String -> String
visitOrder word = unwords [ show i ++ ":" ++ [word !! i] | i <- [n - 1, n - 2 .. 0] ]
  where
    n = length word

-- | Funkcijas 'reverseWord' testi, ieskaitot robežgadījumus.
functionTests :: [TestResult]
functionTests =
  [ test "Normal word" "Haskell" "lleksaH"
  , test "Single character" "a" "a"
  , test "Palindrome" "level" "level"
  , test "Latvian letters" "āboliņš" "šņilobā"
  , test "Letters and digits" "abc123" "321cba"
  , test "Mixed case" "HeLLo" "oLLeH"
  , testCaseWith showText "Reversing twice gives the original" (showText "Programming")
      "Programming" (reverseWord (reverseWord "Programming"))
  , testCase "Same result as reverseWordFast and standard 'reverse'"
      "words of length 0..50" True
      (and [ reverseWord w == reverse w && reverseWordFast w == reverse w
           | k <- [0 .. 50 :: Int], let w = take k (cycle "haskell") ])
  ]
  where
    test name input expected =
      testCaseWith showText name (showText input) expected (reverseWord input)

-- | Lietotāja ievades pārbaudes testi (robežgadījumi).
inputTests :: [TestResult]
inputTests =
  [ test "Valid word" "Haskell" (Right "Haskell")
  , test "Spaces around the word are removed" "  Haskell  " (Right "Haskell")
  , test "Empty input" "" (Left "empty input - a word was expected")
  , test "Only spaces" "     " (Left "empty input - a word was expected")
  , test "Several words" "hello world"
      (Left "'hello world' contains spaces - please enter a single word")
  , test "Digits only" "12345"
      (Left "'12345' contains no letters - a word must contain at least one letter")
  , test "Symbols only" "#!?"
      (Left "'#!?' contains no letters - a word must contain at least one letter")
  ]
  where
    test name input expected =
      testCaseWith (showEither showText) name (showText input) expected (validateWord input)

{-|
Uzdevuma Nr. 2 izpilde no galvenās izvēlnes: testi un interaktīvā daļa.
-}
runTask2 :: IO ()
runTask2 = do
  putStrLn "--- Task 2: Reverse a Word ---"
  putStrLn ""
  _ <- runTests "Function tests" functionTests
  _ <- runTests "Input validation tests (edge cases)" inputTests
  interactivePart

-- Interaktīvā daļa: lietotājs ievada vārdus.
interactivePart :: IO ()
interactivePart = do
  putStrLn "=== Try it yourself ==="
  loop
  where
    loop = do
      input <- promptUntilValid "Enter a word: " validateWord
      case input of
        Nothing   -> return ()
        Just word -> do
          putStrLn ("Visit order (index:character): " ++ visitOrder word)
          putStrLn ("Reversed word: " ++ showText (reverseWord word))
          when (isPalindrome word) (putStrLn "The word is a palindrome.")
          again <- askYesNo "Reverse another word? (y/n): "
          when again loop
