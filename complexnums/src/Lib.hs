module Lib
    ( someFunc
    ) where

import qualified System.IO as SIO
import Data.Function ((&))
import Control.Arrow ((>>>))
import Text.Regex.PCRE ((=~))
import qualified Text.Regex.PCRE as PCRE
import Control.Lens.Operators
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Debug.Trace as Trace

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

prompt :: (String -> Bool) -> String -> IO String
prompt loopCondition text = line >>= breakorcontinue
    where
        breakorcontinue a = if' (loopCondition a) (return a) (prompt loopCondition text)
        line = (text & putStr) >>
            (SIO.stdout & SIO.hFlush) >>
            getLine

complexNumber = "^\\s*(()()([\\-\\+])?\\s*(([0-9\\.]*)i)|([\\-\\+])?\\s*([0-9\\.]+)()(())|([\\-\\+])?\\s*([0-9\\.]+)\\s*([\\-\\+])\\s*(([0-9\\.]*)i))\\s*$"


data ComplexNumber = ComplexNumber {
    complexNumberReal :: Float,
    complexNumberImaginary :: Float
}

instance Show ComplexNumber where
    show (ComplexNumber real imaginary) = [real & show, if' (imaginary >= 0) " + " " ", imaginary & show, "i"] & concat

stringToComplex :: String -> ComplexNumber
stringToComplex input = ComplexNumber realValue imaginaryValue
    where
        realValue = if' (null realDigits) 0 (read realDigits * handleSign realSign)
        imaginaryValue = handleImaginary (handleSign imaginarySign) imaginaryPart imaginaryDigits
        handleSign "+" = 1 :: Float
        handleSign "-" = -1
        handleSign "" = 1
        handleImaginary sign part digits
            | null part = 0
            | null digits = 1 * sign
            | otherwise = read digits * sign
        realSign:realDigits:imaginarySign:imaginaryPart:imaginaryDigits:_ = match & drop 2 & Split.chunksOf 5 & dropWhile(\a -> (a & filter(=="") & length) == 5) & head
        match:_ = input =~ complexNumber :: [[String]]

addComplex :: ComplexNumber -> ComplexNumber -> ComplexNumber
addComplex a b = ComplexNumber (complexNumberReal a + complexNumberReal b) (complexNumberImaginary a + complexNumberImaginary b)

multiplyComplex :: ComplexNumber -> ComplexNumber -> ComplexNumber
multiplyComplex a b = ComplexNumber 
    (complexNumberReal a * complexNumberReal b - complexNumberImaginary a * complexNumberImaginary b) 
    (complexNumberReal a * complexNumberImaginary b + complexNumberReal b * complexNumberImaginary a)

divideComplex :: ComplexNumber -> ComplexNumber -> ComplexNumber
divideComplex num dem = getdem & getfinal
    where
        getfinal newdem = ComplexNumber
            (
                (complexNumberReal num * complexNumberReal dem + complexNumberImaginary num * complexNumberImaginary dem)
                / newdem
            )
            (
                (complexNumberReal dem * complexNumberImaginary num - complexNumberReal num * complexNumberImaginary dem)
                / newdem
            )
        getdem = complexNumberReal dem ** 2 + complexNumberImaginary dem ** 2

someFunc :: IO ()
someFunc = showResults <$> firstNumber <*> secondNumber >>= putStrLn
    where
        showResults f s = 
            [
                "Input: ",
                f & show,
                s & show,
                "Added: ",
                addComplex f s & show, 
                "Multiplied: ", 
                multiplyComplex f s & show,
                "Divided: ", 
                divideComplex f s & show
            ] & List.intercalate "\n"
        firstNumber = ("Enter a complex number in the form of a + bi: " & prompt(=~ complexNumber)) <&> stringToComplex
        secondNumber = ("Enter a second complex number in the form of a + bi: " & prompt(=~ complexNumber)) <&> stringToComplex
