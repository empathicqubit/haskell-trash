module Lib
    ( someFunc
    ) where

import qualified Data.Map as Map
import qualified Data.List as List
import Data.Function ((&))
import Control.Arrow ((>>>))
import qualified Data.Maybe as Maybe
import Criterion.Main as Criterion

data JsonToken = 
    JsonArray [JsonToken]
    | JsonObject (Map.Map String JsonToken)
    | JsonNothing
    | JsonDouble Double
    | JsonString String
    | JsonBool Bool
    | JsonNull
    deriving Show

parse :: String -> JsonToken
parse input = input & parseValue & parseTrailing

parseTrailing :: (JsonToken, String) -> JsonToken
parseTrailing (o, []) = o
parseTrailing (o, input)
    | (input & dropWhile(`List.elem` whitespace)) == [] = o

whitespace = "\t\n\r "
numbers = "-1234567890."
skipChars = "\t\n\r ,"
keywordChars = ['a'..'z']

parseValue :: String -> (JsonToken, String)
parseValue [] = (JsonNothing, [])
parseValue (chr:rest)
    | chr == '{' = rest & parseObject
    | chr == '[' = rest & parseArray
    | chr `List.elem` numbers = chr:rest & parseNumber 
    | chr `List.elem` keywordChars = chr:rest & parseKeyword
    | chr == '"' = chr:rest & parseString
    | chr `List.elem` whitespace = rest & (dropWhile(`List.elem` whitespace)) & parseValue

parseKeyword :: String -> (JsonToken, String)
parseKeyword input = input & span(`List.elem` keywordChars) & tryKeyword

tryKeyword :: (String, String) -> (JsonToken, String)
tryKeyword (keyword, rest)
    | keyword == "true" = (JsonBool True, rest)
    | keyword == "false" = (JsonBool False, rest)
    | keyword == "null" = (JsonNull, rest)

parseNumber :: String -> (JsonToken, String)
parseNumber input = (num & read & JsonDouble, nrest)
    where
        (num, nrest) = input & (span(`List.elem` numbers))

parseString :: String -> (JsonToken, String)
parseString input = (val & JsonString, vrest & tail)
    where (val, vrest) = input & getString 

getString :: String -> (String, String)
getString input = input & tail & span(/= '"')

parseSequence :: String -> Char -> ((a, String) -> (a, String)) -> (a, String) -> (a, String)
parseSequence foundChars termChar worker (o, []) = (o, [])
parseSequence foundChars termChar worker (o, chr:rest)
    | chr `List.elem` skipChars = rest & dropWhile(`List.elem` skipChars) & (o,) & parseSequence foundChars termChar worker
    | chr == termChar = (o, rest)
    | foundChars == [] || chr `List.elem` foundChars = (o, chr:rest) & worker & parseSequence foundChars termChar worker

parseObject :: String -> (JsonToken, String)
parseObject input = (map & JsonObject, rest)
    where (map, rest) = (Map.empty, input) & (parseSequence "\"" '}' parseProperty)

parseArray :: String -> (JsonToken, String)
parseArray input = (arr & reverse & JsonArray, rest)
    where (arr, rest) = ([], input) & (parseSequence "" ']' parseArrayItem)

parseArrayItem :: ([JsonToken], String) -> ([JsonToken], String)
parseArrayItem (o, rest) = (val:final, frest)
    where 
        (val, vrest) = rest & parseValue
        (final, frest) = (o, vrest)

parseProperty :: (Map.Map String JsonToken, String) -> (Map.Map String JsonToken, String)
parseProperty (m, rest) = (newm, frest)
    where 
        newm = m & (Map.insert name val)
        (name, vrest) = rest & getString
        (val, frest) = vrest & dropWhile(/= ':') & tail & parseValue

someFunc :: IO ()
someFunc = getContents >>= (parse >>> show >>> putStrLn)
-- someFunc = defaultMain [ bgroup "main" [ Criterion.bench "main" $ whnf parse " { \"hello\" : \"world \" , \" hungry\" : [ \"grimace \", \" hamburgular\" , 2 ] , \" ronald\" : -334.3987 , \"mcdonald\": true }"]]

-- x@x:~/json-parser/src$ echo '{"hello":"world", "hungry": ["grimace", "hamburgular", 2], "ronald": -334.3987, "mcdonald": true }        ' | stack run
-- JsonObject (fromList [("hello",JsonString "world"),("hungry",JsonArray [JsonString "grimace",JsonString "hamburgular",JsonDouble 2.0]),("mcdonald",JsonBool True),("ronald",JsonDouble (-334.3987))])
