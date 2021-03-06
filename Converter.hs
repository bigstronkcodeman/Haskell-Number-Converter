import System.IO
import Data.Char (toLower, toUpper, intToDigit, digitToInt)
import Data.List (foldl', isPrefixOf)
import Text.Read (readMaybe)


type Prompt = String

data NumberBase = Binary | Octal | Decimal | Hexadecimal
    deriving (Show, Read, Eq, Enum)

-- A mapping from number bases to the corresponding integer
baseNum :: (Integral a) => NumberBase -> a
baseNum Binary = 2
baseNum Octal = 8
baseNum Decimal = 10
baseNum Hexadecimal = 16

-- Given a string and a number base, check to see if that string is
-- a valid representation of a number in the given base
isValidNumber :: String -> NumberBase -> Bool
isValidNumber "" _ = False
isValidNumber s nb = foldl' (\acc d -> acc && (d `elem` validDigs nb)) True s
    where validDigs Binary = "01"
          validDigs Octal = "01234567"
          validDigs Decimal = "0123456789"
          validDigs Hexadecimal = "0123456789abcdefABCDEF"


-- Given a string, return a Maybe NumberBase depending on whether or not
-- a the string represents a valid number in any of the supported number bases
numberBase :: String -> Maybe NumberBase
numberBase ds
    | "0b" `isPrefixOf` ds && isValidNumber numStr Binary = Just Binary
    | "0o" `isPrefixOf` ds && isValidNumber numStr Octal = Just Octal
    | "0x" `isPrefixOf` ds && isValidNumber numStr Hexadecimal = Just Hexadecimal
    | isValidNumber ds Decimal = Just Decimal
    | otherwise = Nothing
    where numStr = drop 2 ds


-- Add proper prefix to a given number string
addPrefix :: NumberBase -> String -> String
addPrefix Binary = ("0b" ++)
addPrefix Octal = ("0o" ++)
addPrefix Decimal = id
addPrefix Hexadecimal = ("0x" ++)


-- Given a string, return a Maybe value that will hold the numerical representation
-- of the string in the proper base, if the string is valid
strToDecimal :: (Integral a) => String -> Maybe a
strToDecimal s = 
    numberBase s >>= \b ->
    let numBuilder acc c = (acc * baseNum b) + (toInteger . digitToInt) c
    in return . fromIntegral . foldl' numBuilder 0 $ modify b
    where modify Decimal = s
          modify _ = drop 2 s


-- Given an integral number and a number base, convert it to the corresponding
-- string representation
numToStr :: (Show a, Integral a) => a -> NumberBase -> String
numToStr x nb =
    let base = baseNum nb
        divSequence = takeWhile (> 0) $ iterate (`div` base) x
        remSequence = map (intToDigit . fromIntegral . (`rem` base)) divSequence
    in dropWhile (== '0') $ reverse remSequence


-- Simple helper for printing a string to stdout then flushing the buffer
-- so that we can get input on the same line as output
putStrFlush :: String -> IO ()
putStrFlush s = putStr s >> hFlush stdout

-- Get a valid number (in any of the supported bases) from the user
getInputNumber :: (Integral a) => Prompt -> IO a
getInputNumber p =
    putStrFlush p >>
    getLine >>= \num ->
    case strToDecimal num of 
        Nothing -> getInputNumber "Invalid number, try again: "
        Just x -> return x
        
-- Get a valid supported number base from the user
getBase :: Prompt -> IO NumberBase
getBase p = 
    putStrFlush p >>
    getLine >>= \inputBase ->
    let base = sanitizer inputBase >>= readMaybe
    in case base of 
        Nothing -> getBase "Invalid base, try again: "
        Just b -> return b
    where sanitizer (x:xs) = Just $ toUpper x : map toLower xs
          sanitizer _ = Nothing

-- Do the stuff
main = 
    getInputNumber "Enter an integer: " >>= \numIn ->
    getBase "Enter a base to convert to: " >>= \baseOut -> 
    (putStrLn . ("Converted number: " ++) . addPrefix baseOut . numToStr numIn) baseOut >>
    main
