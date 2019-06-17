module Lib
    ( someFunc
    ) where

import System.Console.Haskeline

readEvalPrintLoop :: IO ()
readEvalPrintLoop = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ">> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do outputStrLn $ "Input was: " ++ input
                         loop

someFunc :: IO ()
someFunc = readEvalPrintLoop
