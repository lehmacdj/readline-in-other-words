-- | This libraries provides an in-other-words effect that provides interactive
-- command line usage.
--
-- This module provides the following effects:
-- * 'Readline' which offers basic I/O operations on the command line
-- * 'HandleInterrupt' which offers the ability to handle Ctrl-C interrupts
--
-- These effects should cover all of functionality needed except in exceptional
-- circumstances. If needed to tamper with the history functionality provided
-- by haskeline, however, check out "Control.Effect.Readline.History" which
-- provides a 'Control.Effect.Readline.History.ReadlineHistory' effect for
-- doing that.
--
-- In addition to the effects this module exports a carrier 'ReadlineC' and
-- interpreters that can interpret all of the effects in this library.
module Control.Effect.Readline
  ( -- * Effect and Actions

    -- ** Readline
    Readline (..),
    getInputLine,
    getInputLineWithInitial,
    getInputChar,
    getPassword,
    waitForAnyKey,
    outputStr,
    outputStrLn,

    -- ** HandleInterrupt
    HandleInterrupt (..),
    H.Interrupt (..),
    withInterrupt,
    handleInterrupt,
    catchInterrupt,

    -- * Carrier + Threading
    ReadlineC,
    ReadlineThreads,

    -- * Interpreters
    runReadline,
    runReadlineBehavior,
    runReadlineWithPrefs,
    runReadlineBehaviorWithPrefs,

    -- * Re-exports from haskeline

    -- ** Settings
    H.Settings (..),
    H.defaultSettings,
    H.setComplete,

    -- *** Completion
    module System.Console.Haskeline.Completion,

    -- ** Behavior
    H.Behavior,
    H.defaultBehavior,

    -- ** Preferences
    H.Prefs,
    H.readPrefs,
    H.defaultPrefs,
  )
where

import Control.Effect.Readline.Internal
import qualified System.Console.Haskeline as H
import System.Console.Haskeline.Completion
