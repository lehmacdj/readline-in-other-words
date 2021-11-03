# readline-in-other-words
[![GitHub Actions](https://github.com/lehmacdj/readline-in-other-words/actions/workflows/ci.yml/badge.svg)](https://github.com/lehmacdj/readline-in-other-words/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/readline-in-other-words.svg?logo=haskell)](https://hackage.haskell.org/package/readline-in-other-words)

This package provides a few [in-other-words](https://github.com/KingoftheHomeless/in-other-words#readme) effects that collectively provide the full functionality of [haskeline](https://github.com/judah/haskeline#readme). See Haskeline's documentation for additional usage information.

## Example Usage
This is all it takes to write a miniature repl using this library:
```
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
```

This will echo whatever text you write, and catches interrupts announcing that
they occurred and preventing the program from terminating. This example is also
available as the `echo-repl` target in `examples/Echo.hs`.

## Contributions
Bug reports and PRs are welcome.
