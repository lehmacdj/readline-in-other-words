{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Effect
import Control.Effect.Readline

repl :: Eff Readline m => m ()
repl = do
  mline <- getInputLine "> "
  case mline of
    Nothing -> pure ()
    Just line -> outputStrLn line >> repl

main :: IO ()
main = runM $ runReadline defaultSettings $ repl
