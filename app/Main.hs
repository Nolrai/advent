module Main (main) where


main :: IO ()
main = do
  runConduitRes $
    sourceFile "input1.txt" .| 
      decodeUtf8C .|
      peekForeverE 
        (do
          v <- lineC readCE
          liftIO $ print len)

