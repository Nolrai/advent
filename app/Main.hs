{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Conduit
import Data.Conduit.Text
import Data.Conduit.List (mapAccum)
import Text.Megaparsec
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
  [inputFile] <- getArgs
  runConduitRes $
    sourceFile inputFile 
      .| decodeUtf8C 
      .| linesBounded 100 
      .| mapWhileC parseDecimal
      .| pairUp
      .| do
        (len :: Int) <- lengthIfC (uncurry (<)) 
        liftIO $ print len

pairUp :: ConduitT Int (Int, Int) (ResourceT IO) ()
pairUp = mapAccum step maxBound >> pure ()
  where
    step :: Int -> Int -> (Int, (Int, Int))
    step new old = (new, (old, new))

parseDecimal :: Text -> Maybe Int
parseDecimal = parseMaybe (L.lexeme C.space L.decimal :: Parsec Void Text Int)