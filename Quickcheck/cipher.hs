-- Caesar's Cipher 
-- Jeremy.Singer@glasgow.ac.uk
-- great example for QuickCheck

import Data.Char


shouldcipher :: Char -> Bool
-- is this a letter to be ciphered?
shouldcipher c = isLetter(c) && isAscii(c)



cipherchar :: Int -> Char -> Char
-- enciphers single char at a time - NO WRAPPING
cipherchar shift c
 | shouldcipher c = chr(ord(c)+shift)
 | otherwise      = c



-- encipher a whole string
cipher :: Int -> [Char] -> [Char]
cipher shift plaintext = map (bettercipherchar shift) plaintext




decipher :: Int -> [Char] -> [Char]
-- inverse of cipher function
decipher shift ciphertext = cipher (-shift) ciphertext



wraparound shift c 
-- should we wrap around the alphabet, if we shift past Z?
 | isLower(c) && ord(c)+shift > ord 'z' = True
 | isUpper(c) && ord(c)+shift > ord 'Z' = True
 | otherwise = False


bettercipherchar :: Int -> Char -> Char
-- implementation of character substitution with wrapping
bettercipherchar shift c
 | shouldcipher c =  chr(ord(c) + adjustedshift)
 | otherwise      = c
 where adjustedshift = let shift' = shift `mod` 26
                       in if (wraparound shift' c)
                          then shift'-26
                          else shift'
