{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where
import qualified Data.List as P -- P for Prelude
import Conduit
import Data.Conduit.Text
import Data.Conduit.List (mapAccum)
import Text.Megaparsec
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
  [window', inputFile] <- getArgs
  let Just window = parseDecimal (fromString window')
  runConduitRes $
    sourceFile inputFile 
      .| decodeUtf8C 
      .| linesBounded 100 
      .| mapWhileC parseDecimal
      .| slidingWindow window
      .| pairUp
      .| do
        (len :: Int) <- lengthIfC (uncurry (<)) 
        liftIO $ print len

slidingWindow :: Int -> ConduitT Int Int (ResourceT IO) ()
slidingWindow window = getWindow .| step2 .| step3
  where
    getWindow = mapAccum getWindowHelper [] >> pure ()
    step2 = filterC ((== window) . P.length)
    step3 = mapC P.sum
    getWindowHelper :: Int -> [Int] -> ([Int], [Int])
    getWindowHelper new soFar = let r = P.take window (new : soFar) in (r, r)

pairUp :: ConduitT Int (Int, Int) (ResourceT IO) ()
pairUp = mapAccum step maxBound >> pure ()
  where
    step :: Int -> Int -> (Int, (Int, Int))
    step new old = (new, (old, new))

parseDecimal :: Text -> Maybe Int
parseDecimal = parseMaybe (L.lexeme C.space L.decimal :: Parsec Void Text Int)