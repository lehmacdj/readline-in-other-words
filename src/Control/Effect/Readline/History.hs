-- | Provides low level access to history associated with the readline effect.
-- This effect can be interpreted using any of the interpretations using the
-- 'ReadlineC' carrier.
module Control.Effect.Readline.History
  ( ReadlineHistory (..),
    getHistory,
    putHistory,
    modifyHistory,
    module System.Console.Haskeline.History,
  )
where

import Control.Effect.Readline.Internal
import System.Console.Haskeline.History
