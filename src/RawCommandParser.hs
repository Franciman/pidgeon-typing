module RawCommandParser where

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B

import Data.Word (Word8)

import Data.Char (ord)

import Data.Functor (($>))

-- Utility that helps you write ascii codepoints using char literals
-- but watch out, the character must be in the range 0 .. 127
ascii :: Char -> Word8
ascii = fromIntegral . ord

isControlChar :: Word8 -> Bool
isControlChar c = (0 <= c && c <= 0x1F) || c == 0x7F

isRespSpecial = (== ascii ']')

isListWildcard = let values = map ascii [ '%', '*' ]
                 in (`elem` values)

isQuotedSpecial = let values = map ascii ['"', '\\' ]
                  in (`elem` values)

isAtomSpecial :: Word8 -> Bool
isAtomSpecial c = isSeparator c ||
                  isControlChar c ||
                  isListWildcard c ||
                  isQuotedSpecial c ||
                  isRespSpecial c

    where isSeparator c  = c `elem` map ascii [ '(' , ')' , '{' , ' ' ]

isAtomChar :: Word8 -> Bool
isAtomChar = not . isAtomSpecial

isAStringChar :: Word8 -> Bool
isAStringChar c = isAtomChar c || isRespSpecial c


tag :: A.Parser B.ByteString
tag = A.takeWhile1 (\c -> isAStringChar c && c /= ascii '+')

space :: A.Parser Word8
space = A.word8 0x20

crlf :: A.Parser ()
crlf = A.word8 (ascii '\r') *> A.word8 (ascii '\n') $> ()

-- This datatype represents a semiparsed command.
-- It is the result of parsing only command tag and command name.
-- Command arguments are left unparsed for further processing by specialized parsers
data RawCommand = RawCommand
                { commandTag       :: B.ByteString
                , commandName      :: B.ByteString
                , commandArguments :: B.ByteString
                }

-- Superficially parse a command.
-- This means just parse tag and command name,
-- but leave arguments unparsed for further processing
rawCommand :: A.Parser RawCommand
rawCommand = do
    commandTag <- tag
    space
    commandName <- A.takeWhile1 (/= ascii ' ')
    space
    commandArguments <- B.pack <$> A.manyTill A.anyWord8 crlf
    return $ RawCommand commandTag commandName commandArguments
