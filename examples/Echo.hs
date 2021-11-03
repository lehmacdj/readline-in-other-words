{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Main where

import Control.Effect
import Control.Effect.Readline

repl :: Effs '[Readline, HandleInterrupt] m => m ()
repl = handleInterrupt (outputStrLn "Interrupt!" *> repl) $
  withInterrupt $ do
    mline <- getInputLine "> "
    case mline of
      Nothing -> pure ()
      Just line -> outputStrLn line *> repl

main :: IO ()
main = runM $ runReadline defaultSettings repl
