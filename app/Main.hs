{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import Conduit
import Data.List as L
import Data.Conduit.Text
import Data.Conduit.List (mapAccum)
import Text.Megaparsec
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
  [window', inputFile] <- getArgs
  let Just window = parseDecimal (pack window')
  (len :: Int) 
    <- runConduitRes $
      sourceFile inputFile 
        .| decodeUtf8C 
        .| linesBounded 100
        .| mapWhileC parseDecimal
  --      .| slidingAverage window
        .| pairUp
        .| lengthIfC (uncurry (<))
  print len

slidingAverage :: Monad m => Int -> ConduitT Int Int m Int
slidingAverage window =
  mapAccum step [] .| filterC ((== window) . L.length) .| mapC L.sum
  where
    step new l = let l' = new : L.take (window - 1) l in (l', l')

pairUp ::  Monad m => ConduitT Int (Int, Int) m Int
pairUp = mapAccum step maxBound
  where
    step :: Int -> Int -> (Int, (Int, Int))
    step new old = (new, (old, new))

parseDecimal :: Text -> Maybe Int
parseDecimal = parseMaybe (L.lexeme C.space L.decimal :: Parsec Void Text Int)