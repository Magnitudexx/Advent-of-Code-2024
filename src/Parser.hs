module Parser where

type ParsedFile = [String]

genParser :: (String -> ParsedFile) 
            -> FilePath
            -> IO ParsedFile
genParser parser file = parser <$> readFile file

wordParser :: FilePath -> IO ParsedFile
wordParser  = genParser words

lineParser :: FilePath -> IO ParsedFile
lineParser = genParser lines

toIntList :: ParsedFile -> [Integer]
toIntList  = map read 

