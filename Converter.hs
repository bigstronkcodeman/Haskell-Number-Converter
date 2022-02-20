import Data.Char (ord, isNumber, toLower, toUpper, intToDigit, digitToInt)
import System.IO
import Data.List (foldl', isPrefixOf, intercalate)
import Text.Read (readMaybe)


type Prompt = String

data NumberBase = Binary | Octal | Decimal | Hexadecimal
    deriving (Show, Read, Eq, Enum)

baseNum :: (Integral a) => NumberBase -> a
baseNum Binary = 2
baseNum Octal = 8
baseNum Decimal = 10
baseNum Hexadecimal = 16


isValidNumber :: String -> NumberBase -> Bool
isValidNumber "" _ = False
isValidNumber s nb = foldl' (\acc d -> acc && (d `elem` validDigs nb)) True s
    where validDigs Binary = "01"
          validDigs Octal = "01234567"
          validDigs Decimal = "0123456789"
          validDigs Hexadecimal = "0123456789abcdef"


numberBase :: String -> Maybe NumberBase
numberBase ds
    | "0b" `isPrefixOf` ds && isValidNumber numStr Binary = Just Binary
    | "0o" `isPrefixOf` ds && isValidNumber numStr Octal = Just Octal
    | "0x" `isPrefixOf` ds && isValidNumber numStr Hexadecimal = Just Hexadecimal
    | isValidNumber ds Decimal = Just Decimal
    | otherwise = Nothing
    where numStr = drop 2 ds

addPrefix :: NumberBase -> String -> String
addPrefix Binary = ("0b" ++)
addPrefix Octal = ("0o" ++)
addPrefix Decimal = id
addPrefix Hexadecimal = ("0x" ++)

strToDecimal :: (Integral a) => String -> Maybe a
strToDecimal s = 
    numberBase s >>= \b ->
    let numBuilder acc (i,c) = (acc * baseNum b) + (toInteger . digitToInt) c
    in return . fromIntegral . foldl' numBuilder 0 . zip [0..] $ modify b
    where modify Decimal = s
          modify _ = drop 2 s


numToStr :: (Show a, Integral a) => a -> NumberBase -> String
numToStr x nb =
    let base = baseNum nb
        divSequence = takeWhile (> 0) $ iterate (`div` base) x
        divRemPairs = map (\x -> (x, (intToDigit . fromIntegral) $ x `rem` base)) divSequence
    in dropWhile (== '0') $ foldl (\acc (_,r) -> r : acc) "" divRemPairs


putStrFlush :: String -> IO ()
putStrFlush s = putStr s >> hFlush stdout


getInputNumber :: (Integral a) => Prompt -> IO a
getInputNumber p =
    putStrFlush p >>
    getLine >>= \num ->
    case strToDecimal . map toLower $ num of 
        Nothing -> getInputNumber "Invalid number, try again: "
        Just x -> return x
        

getBase :: Prompt -> IO NumberBase
getBase p = 
    putStrFlush p >>
    getLine >>= \inputBase ->
    let upperFirst = toUpper <$> first inputBase
        lowerRest = map toLower <$> rest inputBase
        base = (:) <$> upperFirst <*> lowerRest >>= readMaybe
    in case base of 
        Nothing -> getBase "Invalid base, try again: "
        Just b -> return b
    where first (x:_) = Just x
          first _ = Nothing
          rest (_:xs) = Just xs
          rest _ = Nothing


main = 
    getInputNumber "Enter an integer: " >>= \numIn ->
    getBase "Enter a base to convert to: " >>= \baseOut -> 
    let result = numToStr numIn baseOut
        prefixedResult = addPrefix baseOut result
    in putStrLn ("Converted number: " ++ prefixedResult) >>
       main
