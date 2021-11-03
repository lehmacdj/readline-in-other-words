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
-- This module exposes threading constraints as orphans for 'H.InputT'. In
-- addition to these orphans it also provides an orphan for
-- 'Control.Monad.Trans.Control.MonadBase and
-- 'Control.Monad.Trans.Control.MonadBaseControl' for versions of haskeline
-- that do not provide it themselves (currently all versions).
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

    -- * Interpreters
    runReadline,
    runReadlineBehavior,
    runReadlineWithPrefs,
    runReadlineBehaviorWithPrefs,
    runReadline',
    runReadlineBehavior',
    runReadlineWithPrefs',
    runReadlineBehaviorWithPrefs',

    -- * Carriers + Threading
    ReadlineC,
    ReadlineInterruptC,
    ReadlineThreads,

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
