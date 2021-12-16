{-# LANGUAGE ScopedTypeVariables, DerivingStrategies, FlexibleContexts #-}

module Main (main) where
import qualified Data.List as P -- P for Prelude
import Linear
import Conduit
import Data.Conduit.Text
import Data.Conduit.List (mapAccum)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

main :: IO ()
main = do
  day : inputFile : args <- getArgs 
  case parseDecimal (fromString day) of
    Just 1 -> day1 args inputFile
    Just 2 -> day2 inputFile

day2 :: FilePath -> IO ()
day2 inputFile = 
  runConduitRes $
    sourceFile inputFile 
      .| decodeUtf8C 
      .| linesBounded 100 
      .| mapWhileC parseDay2Command
      .| mapC toXY
      .| sumC >>= (liftIO . print)

toXY :: (Day2Dir, Int) -> V2 Int
toXY (Forward, n) = V2 n 0
toXY (Up, n) = V2 0 (-n)
toXY (Main.Down, n) = V2 0 n

parseMaybe :: M.Parsec Void Text a -> Text -> Maybe a
parseMaybe = M.parseMaybe

data Day2Dir = Forward | Up | Down
  deriving stock (Show, Eq, Enum, Bounded)

parseDay2Command :: Text -> Maybe (Day2Dir, Int)
parseDay2Command = parseMaybe $ (,) <$> enumParser <*> decimal 
  
enumParser :: forall a. (Show a, Enum a, Bounded a) => M.Parsec Void Text a
enumParser = asum $ f `map` [minBound .. maxBound]
  where
    f :: a -> M.Parsec Void Text a
    f x = M.try (L.symbol' C.space (show x) $> x) 

day1 :: [String] -> FilePath -> IO ()
day1 args inputFile = do
  let [window'] = args
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

decimal :: M.Parsec Void Text Int
decimal = L.lexeme C.space L.decimal 

parseDecimal :: Text -> Maybe Int
parseDecimal = parseMaybe decimal