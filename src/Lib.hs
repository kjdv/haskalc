module Lib
    ( readEvalPrintLoop
    ) where

import Program
import System.Console.Haskeline

readEvalPrintLoop :: IO ()
readEvalPrintLoop = runInputT defaultSettings (loop program)
  where
    loop :: Program -> InputT IO ()
    loop prog = do
      minput <- getInputLine ">> "
      case minput of
        Nothing -> return ()
        Just input -> do
          let (s, p) = run prog input
          outputStrLn s
          loop p
