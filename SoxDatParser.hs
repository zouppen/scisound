module SoxDatParser where

import Text.Parsec.ByteString.Lazy -- Is OK because no UTF-8 is parsed.
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Combinator

-- |Self-defined instead of (float haskell) because it has to support
-- negative numbers and floats without a dash.
soxNumber = do 
  number <- many1 $ oneOf "-e.0123456789"
  return $ read number

-- |Parses DOS newline, but is compatible with UNIX newline, too.
dosNewline :: Parser Char
dosNewline = do
  optional $ char '\r'
  newline

-- |Doesn't get fooled by newlines and other whitespace characters.
literalSpaces = skipMany $ char ' '

data Sample = Sample { time :: Double
                     , left :: Double
                     , right :: Double
                     } deriving (Show, Ord, Eq)

parseDatFromFile :: FilePath -> IO [Sample]
parseDatFromFile f = do 
  result <- parseFromFile parseDat f
  case result of
    Left a -> fail $ show a
    Right a -> return a

parseDat :: Parser [Sample]
parseDat = do
  string "; Sample Rate "
  skipMany digit
  dosNewline
  string "; Channels 2"
  dosNewline
  manyTill sample eof
  
sample :: Parser Sample
sample = do
  literalSpaces
  time <- soxNumber
  literalSpaces
  left <- soxNumber
  literalSpaces
  right <- soxNumber
  literalSpaces
  dosNewline
  return $ Sample time left right
