{-|
Module      : Main
Description : Laboratorijas darba Nr. 1 (8. variants) galvenā programma.

Programma apvieno visus astoņus uzdevumus vienā lietotnē ar teksta
izvēlni. Lietotājs izvēlas uzdevuma numuru, programma izpilda attiecīgā
moduļa testus un interaktīvo daļu un pēc tam atgriežas izvēlnē.

Izvēlnes ievade tiek pārbaudīta: burti, decimāldaļskaitļi, tukša ievade
un numuri ārpus diapazona 0-8 netiek pieņemti, un lietotājam tiek lūgts
ievadīt vēlreiz. Ja ievade beidzas (Ctrl+D vai Ctrl+Z), programma
beidz darbu bez kļūdas.

Projekta struktūra:

* @Main.hs@ - galvenā izvēlne (šis modulis);
* @Tasks/Task1.hs@ ... @Tasks/Task8.hs@ - uzdevumu risinājumi;
* @Utils/Input.hs@ - droša ievades nolasīšana un pārbaude;
* @Utils/Testing.hs@ - testu izdruka.

Palaišana no projekta mapes:

@
runghc Main.hs
@

vai kompilējot:

@
ghc Main.hs -o lab1
./lab1
@
-}
module Main (main) where

import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEncoding, stdin, stdout, utf8)

import qualified Tasks.Task1 as Task1
import qualified Tasks.Task2 as Task2
import qualified Tasks.Task3 as Task3
import qualified Tasks.Task4 as Task4
import qualified Tasks.Task5 as Task5
import qualified Tasks.Task6 as Task6
import qualified Tasks.Task7 as Task7
import qualified Tasks.Task8 as Task8
import Utils.Input (parseInt, promptUntilValid, waitForEnter)

{-|
Izvēlnes punkti: numurs, nosaukums un izpildāmā darbība.
-}
menuItems :: [(Int, String, IO ())]
menuItems =
  [ (1, "Task 1: Maximum number in a list",               Task1.runTask1)
  , (2, "Task 2: Reverse a word",                         Task2.runTask2)
  , (3, "Task 3: Capitalize every word",                  Task3.runTask3)
  , (4, "Task 4: Last element of a list",                 Task4.runTask4)
  , (5, "Task 5: Run-length decoding",                    Task5.runTask5)
  , (6, "Task 6: Matrix transposition",                   Task6.runTask6)
  , (7, "Task 7: Binary search trees",                    Task7.runTask7)
  , (8, "Task 8: map f . concat = concat . map (map f)",  Task8.runTask8)
  ]

{-|
Programmas ieejas punkts.

Iestata UTF-8 kodējumu (latviešu burtu ievadei un izvadei) un
izslēdz izvades buferēšanu, lai uzvednes parādītos uzreiz.
-}
main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stdin utf8
  hSetBuffering stdout NoBuffering
  menuLoop

{-|
Galvenās izvēlnes cikls: izdrukā izvēlni, nolasa izvēli un izpilda
uzdevumu, līdz lietotājs izvēlas 0 vai ievade beidzas.
-}
menuLoop :: IO ()
menuLoop = do
  printMenu
  choice <- promptUntilValid "Choose a task (0-8): " parseMenuChoice
  case choice of
    Nothing -> putStrLn "\nInput ended. Goodbye!"
    Just 0  -> putStrLn "Goodbye!"
    Just n  -> do
      putStrLn ""
      runMenuItem n
      waitForEnter
      menuLoop

{-|
Pārbauda izvēlnes ievadi.

Parametri:

* @input@ - lietotāja ievadītā virkne.

Atgrieztā vērtība:

* @Right n@ - ja ievadīts vesels skaitlis no 0 līdz izvēlnes punktu skaitam;
* @Left err@ - ja ievade nav skaitlis vai ir ārpus diapazona.
-}
parseMenuChoice :: String -> Either String Int
parseMenuChoice input = do
  number <- parseInt input
  if number >= 0 && number <= length menuItems
    then Right number
    else Left (show number ++ " is not a menu item (choose a number from 0 to "
               ++ show (length menuItems) ++ ")")

-- Izpilda izvēlēto uzdevumu.
runMenuItem :: Int -> IO ()
runMenuItem n =
  case [ action | (number, _, action) <- menuItems, number == n ] of
    (action : _) -> action
    []           -> return ()

-- Izdrukā galveno izvēlni.
printMenu :: IO ()
printMenu = do
  putStrLn "=================================================="
  putStrLn "  Laboratory work No. 1 - Haskell (variant 8)"
  putStrLn "=================================================="
  mapM_ (\(number, title, _) -> putStrLn ("  " ++ show number ++ ". " ++ title)) menuItems
  putStrLn "  0. Exit"
  putStrLn "--------------------------------------------------"
