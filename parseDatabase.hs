{-|
Module : ParseDatabase
Description : -
The module reads the .xml file and extracts the relevant information to
a .CSV file
Copyright : Santiago Cano, Agustin Rico
This module recognizes and classifies the relevant extracted information of a
.xml file about Proteins (Id, Name, Created Date, Formal Organism, Sequence).
-}
module Main where
import System.IO()
import System.Environment
import Text.Regex.TDFA
import Data.CSV
import Data.List.Split
import Data.List.Utils

{-|
The function getID takes the String readen from the file and extracts
protein´s ID.
-}
getID:: String -> String
getID s = x
  where
     x = s =~ ".*[^ >]" ::String

{-|
The function getName tries to match an expression which contains protein's
name and returns the result of the matching in a String.
-}
getName :: String -> String
getName s = a
  where a = replace "\n" "" s =~ "(<name>)(.|\n)*</name>" ::String

{-|
The function clearName takes the Strings that were returned by getName
and extracts protein´s name only.
-}
clearName :: String -> String
clearName s = a
  where a = s =~ "[^<name>].*[^</name>]"

{-|
The function getDate tries to match an expression which contains protein´s
creation date and returns the result of the matching in a String.
-}
getDate :: String -> String
getDate s = a
  where a = replace "\n" "" s =~ "(<created_date>)(.|\n)*</created_date>"

{-|
The function clearDate takes the Strings returned by getDate and extracts
protein´s creation date only.
-}
clearDate :: String -> String
clearDate s = a
  where a = s =~ "[^<created_date>].*[^/<created_date>]"::String

{-|
The function getOrganism tries to match an expression which contains the name
of the protein's organism and returns the result of the matching in a String.
-}
getOrganism :: String -> String
getOrganism s = a
  where a =  replace "\n" "" s =~ "(<formal>)(.|\n)*</formal>"::String

{-|
The function clearOrganism takes the Strings returned by getOrganism
and extracts protein's organism´s name only.
-}
clearOrganism :: String -> String
clearOrganism s = a
  where a = s =~ "[^<formal>].*[^</formal>]"::String
{-|
The function getSequence tries to match an expression which contains the
sequence of the protein and returns the result of the matching in a String.
-}
getSequence :: String -> String
getSequence s = a
  where
    a = replace "\n" "" s  =~ "(<sequence>)(.|\n)*</sequence>"::String

{-|
The function clearSequence takes the Strings returned by getSequence
and extracts protein´s sequence only.
-}
clearSequence :: String -> String
clearSequence s = a
  where a = s =~ "[^<sequence>](.*)[^</sequence>]"::String
{-|
The function getProtein takes the results of all previous getters and places
it´s concatenation on a List.
-}
getProtein:: String -> [[String]]
getProtein s =  [[a]++[b]++[c]++[d]++[e]]
  where a = getID s::String
        b = clearName(getName s)::String
        c = clearDate(getDate s)::String
        d = clearOrganism(getOrganism s)::String
        e = clearSequence(getSequence s)::String

{-|
The function createList takes as parameter "x" a List which contains
each protein of the xml file, splited by the Main.
"y" represents each element of that list, i.e. the information of each
protein, and "z" will be the result of applying getProtein to each element.
-}
createList:: [String] -> [[String]]
createList x= [ z |  y <- x, z <- getProtein y]

{-|
The main function reads the .xml file and splits the information,in order to be
used by the function createList to then store it in a new file "proteinData.csv"
-}
main:: IO()
main= do
  [nameFile] <- getArgs
  datos <- readFile nameFile
  let a = tail (splitOn "<ProteinEntry id=" datos)::[String]
  writeFile "proteinData.csv" (genCsvFile (createList a))
  return()
