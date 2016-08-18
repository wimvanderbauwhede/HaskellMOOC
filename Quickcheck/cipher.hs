import Data.Char

encipher :: [Char] -> Int -> [Char]
encipher [] shift = []
encipher (c:cs) shift 
  | isUpper(c) = let shiftedChar = ord(c) + shift
                     adjustedChar = if shiftedChar > ord('Z') then
                            shiftedChar-26 else shiftedChar
                 in chr(adjustedChar):(encipher cs shift)
  | isLower(c) = let shiftedChar = ord(c) + shift
                     adjustedChar = if shiftedChar > ord('z') then shiftedChar-26 else shiftedChar
                 in chr(adjustedChar):(encipher cs shift)
  | otherwise = c:(encipher cs shift)
