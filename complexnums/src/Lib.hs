module Lib
    ( someFunc
    ) where

import qualified System.IO as SIO
import Data.Function ((&))
import Control.Arrow ((>>>))
import Text.Regex.PCRE ((=~))
import qualified Text.Regex.PCRE as PCRE
import Control.Lens.Operators

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

complexNumber = "^\\s*\\+?(-?\\s*[0-9\\.]+)?\\s*(\\+?((-\\s*)?[0-9\\.]*)i)?\\s*$"

data ComplexNumber = ComplexNumber {
    complexNumberReal :: Float,
    complexNumberImaginary :: Float
}

instance Show ComplexNumber where
    show (ComplexNumber real imaginary) = [real & show, if' (imaginary >= 0) " + " " ", imaginary & show, "i"] & concat

stringToComplex :: String -> ComplexNumber
stringToComplex input = ComplexNumber realValue imaginaryValue
    where
        realValue = if' (null realDigits) 0 (realDigits & read)
        imaginaryValue = 
            if' (null imaginaryDigits) 
                (if' (null imaginaryPart) 
                    0 
                    1
                ) 
                (imaginaryDigits & read)
        _:realDigits:imaginaryPart:imaginaryDigits:_ = match
        match:_ = input =~ complexNumber :: [[String]]

addComplex :: ComplexNumber -> ComplexNumber -> ComplexNumber
addComplex a b = ComplexNumber (complexNumberReal a + complexNumberReal b) (complexNumberImaginary a + complexNumberImaginary b)

multiplyComplex :: ComplexNumber -> ComplexNumber -> ComplexNumber
multiplyComplex a b = ComplexNumber (complexNumberReal a * complexNumberReal b - complexNumberImaginary a * complexNumberImaginary b) (complexNumberReal a * complexNumberImaginary b + complexNumberReal b * complexNumberImaginary a)

someFunc :: IO ()
someFunc = showResults <$> firstNumber <*> secondNumber >>= putStrLn
    where
        showResults f s = 
            [
                addComplex f s & show, 
                "\n", 
                multiplyComplex f s & show
            ] & concat
        firstNumber = ("Enter a complex number in the form of a + bi: " & prompt(=~ complexNumber)) <&> stringToComplex
        secondNumber = ("Enter a second complex number in the form of a + bi: " & prompt(=~ complexNumber)) <&> stringToComplex
