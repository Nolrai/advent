{-# LANGUAGE ScopedTypeVariables, DerivingStrategies, FlexibleContexts, RecordWildCards, OverloadedStrings, NoImplicitPrelude, TupleSections #-}

module Advent (main, lanternFishDay, lanternFishDay', lanternFishDays, fishToVector, dumbPower, smartPower) where
import Relude as P
import Conduit
import Data.Conduit.Text
import Data.Conduit.List (mapAccum)
import Linear.V2
import qualified Text.Megaparsec as M
import Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Lens ((^.))
import Data.Matrix as X
import Data.Vector (Vector) 
import qualified Data.Vector as V 

main :: IO ()
main = do
  day : inputFile : args <- getArgs 
  case parseDecimal (fromString day) of
    Just 0 -> coinProblem
    Just 1 -> day1 args inputFile
    Just 2 -> day2 inputFile args
    Just 6 -> day6 inputFile args

-- day 6 --

day6 :: FilePath -> [String] -> IO ()
day6 inputFile args = do
  [days', outputFile] <- pure args
  Just days <- pure . parseDecimal . fromString $ days'
  Just start <- parseMaybe (decimal `M.sepBy` char ',') <$> readFileText inputFile
  let firstLine = ("Initial state: " <>) . myShowList $ start
  putTextLn firstLine
  let v = fishToVector start
  putTextLn $ "As vector: " <> show v 
  writeFileText outputFile (firstLine <> "\n")
  let popSize = V.sum $ lanternFishDays days v
  appendFileText outputFile $ "oOn day " <> show days <> " there are " <> show popSize <> " fish.\n" 

lanternFishDay :: [Int] -> [Int]
lanternFishDay l = (8 <$ filter (== 0) l) <> map (\ x -> if x > 0 then x - 1 else 6) l

myShowList :: (Show n, Num n) => [n] -> Text
myShowList = fromString . P.drop 1 . P.reverse . P.drop 1 . show

-- makes a histogram
fishToVector :: [Int] -> Vector Int
fishToVector = foldl' add (V.replicate 9 0) . map toBasis
  where
    add :: Vector Int -> Vector Int -> Vector Int
    add = V.zipWith (+)
    toBasis :: Int -> Vector Int -- take 0 to [1,0,0,0,0,0,0,0,0], take 1 to [0,1,0,0,0,0,0,0,0], ... ect.
    toBasis = (`X.getRow` X.identity 9) . (+1)

lanternFishDay' :: Vector Int -> Vector Int
lanternFishDay' v = getRow 1 (rowVector v * growthMatrix)

lanternFishDays :: Int -> Vector Int -> Vector Int
lanternFishDays n = let g = smartPower growthMatrix n in \ v -> getRow 1 (rowVector v * g)

growthMatrix :: Matrix Int
growthMatrix = X.fromList 9 9 [
    0,0,0,0,0,0,1,0,1, -- breeding fish make a new fish 8 days away from breeding, and then become 6 days away.
    1,0,0,0,0,0,0,0,0, -- otherwise the fish just becomes a day closer
    0,1,0,0,0,0,0,0,0,
    0,0,1,0,0,0,0,0,0,
    0,0,0,1,0,0,0,0,0,
    0,0,0,0,1,0,0,0,0,
    0,0,0,0,0,1,0,0,0,
    0,0,0,0,0,0,1,0,0,
    0,0,0,0,0,0,0,1,0
  ]

smartPower, dumbPower :: Num a => Matrix a -> Int -> Matrix a
dumbPower m 0 = X.identity 9
dumbPower m 1 = m
dumbPower m n = m * dumbPower m (n - 1)

smartPower m 0 = X.identity 9
smartPower m 1 = m
smartPower m n
  | even n = let m' = smartPower m (n `div` 2) in m' * m'
  | otherwise = m * smartPower m (n - 1)

-- day 2 --

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

startState :: SubState
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
toXY (Advent.Down, n) = V2 0 n

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

---

data WhichCoin 
  = Coin0
  | Coin1
  | Coin2
  deriving stock (Ord, Eq, Enum, Bounded, Read, Show)

data CoinResult
  = Heads
  | Tails
  deriving stock (Ord, Eq, Enum, Bounded, Read, Show)

data Strat 
  = End WhichCoin
  | Step WhichCoin Strat Strat
  deriving stock (Ord, Eq, Read, Show)

balencedHight :: Strat -> Maybe Int
balencedHight (End _) = Just 0
balencedHight (Step _ onHeads onTails) = do
  l <- balencedHight onHeads
  r <- balencedHight onTails
  guard (l == r)
  pure l

enumAll :: [WhichCoin]
enumAll = [minBound  .. maxBound]

mkStrat :: Int -> [Strat] 
mkStrat 0 = End <$> enumAll
mkStrat n = Step <$> enumAll <*> mkStrat (n-1) <*> mkStrat (n-1)

getCoin :: WhichCoin -> WhichCoin -> [CoinResult]
getCoin x y =
  if x == y
    then [Heads, Heads, Tails]
    else [Heads, Tails]

scoreStrat :: Int -> Strat -> Maybe (Int, Int)
scoreStrat tests strat = do
  l <- sequenceA $ do
      trickCoin <- enumAll
      scoreStrat' tests strat (getCoin trickCoin)
  Just (length (filter (== True) l), length l)
  
scoreStrat' :: Int -> Strat -> (WhichCoin -> [CoinResult]) -> [Maybe Bool]
scoreStrat' 0 (End guess) coins =
  pure . Just $ length (coins guess) == 2
scoreStrat' tests (Step coinToTest onHeads onTails) coins = do
  result <- coins coinToTest
  scoreStrat' 
    (tests - 1)
    (if result == Heads then onHeads else onTails)
    coins
scoreStrat' _ _ _ = pure Nothing

coinProblem' :: Maybe ([Strat], (Int, Int), [(Strat, (Int, Int))])
coinProblem' = do
  scores <- sequenceA $ [(strat,) <$> scoreStrat 3 strat | strat <- mkStrat 3]
  (_,(_,numCases)) <- viaNonEmpty head scores
  guard (P.all ((== numCases) . snd . snd) scores)
  let sorted = sortOn (fst . snd) scores
  (_, topScore) <- viaNonEmpty head sorted
  let strats = fst <$> P.takeWhile ((== topScore) . snd) sorted
  Just (strats, topScore, sorted)

coinProblem :: IO ()
coinProblem = do
  let Just (strats, score, allStrats) = coinProblem'
  putTextLn $ "number of strategies: " <> show (length (mkStrat 3))
  putTextLn $ "best score: " <> show (fst score) <> " out of " <> show (snd score)
  putTextLn $ "number of best strategies: " <> show (length strats)
  printStrat `P.mapM_` allStrats

printStrat :: (Strat, (Int, Int)) -> IO ()
printStrat s = do
  putTextLn "hit enter to print next strategy"
  _ <- getLine
  putTextLn $ "score: " <> show (snd s)
  putTextLn "strat: "
  print (fst s)
  putTextLn ""
