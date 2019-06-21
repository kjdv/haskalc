module Lib
    ( readEvalPrintLoop
    ) where

import Program
import System.Console.Haskeline

readEvalPrintLoop :: IO ()
readEvalPrintLoop = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine ">> "
      case minput of
        Nothing -> return ()
        Just input -> do outputStrLn $ runSingle input
                         loop
