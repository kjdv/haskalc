module Lib
    ( readEvalPrintLoop
    ) where

import           Program
import           System.Console.Haskeline

settings = Settings {
    complete = noCompletion,
    historyFile = Nothing,
    autoAddHistory = True
}

readEvalPrintLoop :: IO ()
readEvalPrintLoop = runInputT settings (loop program)
  where
    loop :: Program -> InputT IO ()
    loop prog = do
      minput <- getInputLine ">>> "
      case minput of
        Nothing -> return ()
        Just input -> do
          let (s, p) = run prog input
          outputStrLn s
          loop p
