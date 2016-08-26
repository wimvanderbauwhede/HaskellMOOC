import Data.Char
import Test.QuickCheck


-- fix this with a map function, that enciphers single char at a time

-- cipherchar :: Char -> Int -> Char
-- cipherchar c shift = ord(c) + shift

-- correctcipherchar :: Char -> Int -> Bool -> Char
-- correctcipherchar c shift uppercase = 
--     let shiftedchar = cipherchar c shift
--         limit = if uppercase then 'Z' else 'z'
--     in if shiftedchar < limit then chr((ord(shiftedchar)-26) else shiftedchar

-- mapcipher :: Char -> Int -> Char
-- mapcipher message shift = (map (cipherchar) message) -- do other way round to 
--       -- get currying correct!

encipher :: [Char] -> Int -> [Char]
encipher [] shift = []
encipher (c:cs) shift 
  | isLetter(c)&&isAscii(c) = let shiftedChar = ord(c) + (shift `mod` 26)
                                  adjustedChar = if shiftedChar > ord(limit) then
                                                   shiftedChar-26 else shiftedChar
                                   where limit = (if isUpper(c) then 'Z' else 'z')
                              in chr(adjustedChar):(encipher cs shift)
  | otherwise = c:(encipher cs shift)



wrongcipher :: [Char] -> Int -> [Char]
wrongcipher [] shift = []
wrongcipher (c:cs) shift 
  | isLetter(c)&&isAscii(c)  = let shiftedChar = ord(c) + shift
                               in chr(shiftedChar):(wrongcipher cs shift)
  | otherwise = c:(wrongcipher cs shift)



decipher = \x y-> (wrongcipher x (-y))


