{-# LANGUAGE ScopedTypeVariables, DerivingStrategies, FlexibleContexts, RecordWildCards, OverloadedStrings #-}

module Main (main) where
import qualified Data.List as P -- P for Prelude
import Linear
import Conduit
import Data.Conduit.Text
import Data.Conduit.List (mapAccum)
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Lens ((^.))
import Data.Vector.Unboxed as V
import Linear.V
import Linear.Matrix

main :: IO ()
main = do
  day : inputFile : args <- getArgs 
  case parseDecimal (fromString day) of
    Just 1 -> day1 args inputFile
    Just 2 -> day2 inputFile args
    Just 6 -> day6 inputFile args 

-- Day 6 --

day6 inputFile (generations:_) = do
    let Just generations = parseDecimal 
    putTextLn $ "generations = " <> show generations
    let g = growthMatrix generations
    putTextLn $ "growthMatrix = " <> show g
    case runParser (octalDigit `sepBy` char ',') <$> readFile inputFile of
      Left err -> die $ prettyErrorBundle err
      Right l -> do
        putTextLn $ "input = " <> show l
        let start = sumV . map (b !!) $ l
        putTextLn $ "start = " <> show start
        let sg = (start *! g)
        putTextLn $ "start * g = " <> show sg
        putTextLn $ "total pop = " <> show (sum . toVector $ sg)
        let gs = (g !* start)
        putTextLn $ "g * start = " <> show gs
        putTextLn $ "total pop = " <> show (sum . toVector $ gs)

myBasis :: [V 9]
myBasis = basis

growthMatrix :: Int -> V 9 (V 9)
growthMatrix 0 = identity
growthMatrix 1 = fromVector (generate (\ i -> myBasis ! (i + 1 `mod` 9) + if i == 6 then myBasis ! 0))
growthMatrix n
  | even n = let g = growthMatrix (n/2) in g !*! g
  | odd n = growthMatrix (n - 1) !*! growthMatrix 1

-- Day 2 --

data SubState = SubState {aim :: Int, pos :: V2 Int}

day2 :: FilePath -> [String] -> IO ()
day2 inputFile args = do 
  p <- runConduitRes $
    sourceFile inputFile 
      .| decodeUtf8C 
      .| linesBounded 100 
      .| mapWhileC parseDay2Command
      .| mapC toXY
      .| processing
  putTextLn $ "pos = " <> show p
  putTextLn $ "x * y = " <> show (p ^. _x * p ^. _y)
  where
    processing :: Monad m => ConduitT (V2 Int) Void m (V2 Int) 
    processing = do
      let [part] = args
      case parseDecimal (fromString part) of
        Just 1 -> sumC
        Just 2 -> pos <$> updateC updateSubState startState

startState = SubState {aim = 0, pos = V2 0 0}

updateC :: Monad m => (a -> s -> s) -> s -> ConduitT a Void m s
updateC update start =
  fuse
    (mapAccum (\ new soFar -> let next = update new soFar in (next, next)) start >> pure ())
    (lastDefC start)

updateSubState :: V2 Int -> SubState -> SubState
updateSubState (V2 n a) SubState{..} = SubState {aim = aim + a, pos = pos + V2 n (n * aim)}

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