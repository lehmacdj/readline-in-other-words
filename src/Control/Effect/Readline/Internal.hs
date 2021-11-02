{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | __WARNING: the API of this module is not included in the PvP versioning of
-- this package.__
--
-- This module mostly exists to break a cyclic dependency between the Carrier
-- instance and 'ReadlineHistory' which isn't exported from
-- "Control.Effect.Readline".
module Control.Effect.Readline.Internal where

import Control.Effect
import Control.Effect.Carrier
import Control.Effect.Optional
import Control.Effect.Type.Bracket
import Control.Effect.Type.Fix
import Control.Effect.Type.Internal.BaseControl
import Control.Effect.Type.ListenPrim
import Control.Effect.Type.Mask
import Control.Effect.Type.Optional
import Control.Effect.Type.ReaderPrim
import Control.Effect.Type.Regional
import Control.Effect.Type.Split
import Control.Effect.Type.Unlift
import Control.Effect.Type.Unravel
import Control.Effect.Type.WriterPrim
import Control.Monad
import Control.Monad.Reader.Class
import Control.Monad.Trans.Control
import Control.Monad.Writer.Class
import qualified System.Console.Haskeline as H
import qualified System.Console.Haskeline.History as H
import Prelude

-- | For documentation on actions see haskeline's functions with the same name
-- and similar type signatures.
data Readline :: Effect where
  GetInputLine :: String -> Readline m (Maybe String)
  GetInputLineWithInitial :: String -> (String, String) -> Readline m (Maybe String)
  GetInputChar :: String -> Readline m (Maybe Char)
  GetPassword :: Maybe Char -> String -> Readline m (Maybe String)
  WaitForAnyKey :: String -> Readline m Bool
  OutputStr :: String -> Readline m ()

getInputLine :: Eff Readline m => String -> m (Maybe String)
getInputLine = send . GetInputLine

getInputLineWithInitial ::
  Eff Readline m => String -> (String, String) -> m (Maybe String)
getInputLineWithInitial p = send . GetInputLineWithInitial p

getInputChar :: Eff Readline m => String -> m (Maybe Char)
getInputChar = send . GetInputChar

getPassword :: Eff Readline m => Maybe Char -> String -> m (Maybe String)
getPassword m = send . GetPassword m

waitForAnyKey :: Eff Readline m => String -> m Bool
waitForAnyKey = send . WaitForAnyKey

outputStr :: Eff Readline m => String -> m ()
outputStr = send . OutputStr

outputStrLn :: Eff Readline m => String -> m ()
outputStrLn str = outputStr (str <> "\n")

data HandleInterrupt :: Effect where
  WithInterrupt :: m a -> HandleInterrupt m a
  HandleInterrupt :: m a -> m a -> HandleInterrupt m a

-- | Enable interrupt handling within a given action. Within the passed action
-- 'H.Interrupt' will be thrown
withInterrupt :: Eff HandleInterrupt m => m a -> m a
withInterrupt = send . WithInterrupt

-- | Handle an 'H.Interrupt'. When an interrupt occurs in the second argument,
-- the first argument will be called.
handleInterrupt :: Eff HandleInterrupt m => m a -> m a -> m a
handleInterrupt handler = send . HandleInterrupt handler

-- | Specify a continuation that should be called when an 'H.Interrupt' occurs.
--
-- > catchInterrupt = flip handleInterrupt
catchInterrupt :: Eff HandleInterrupt m => m a -> m a -> m a
catchInterrupt = flip handleInterrupt

data ReadlineHistory :: Effect where
  GetHistory :: ReadlineHistory m H.History
  PutHistory :: H.History -> ReadlineHistory m ()

getHistory :: Eff ReadlineHistory m => m H.History
getHistory = send GetHistory

putHistory :: Eff ReadlineHistory m => H.History -> m ()
putHistory = send . PutHistory

modifyHistory :: Eff ReadlineHistory m => (H.History -> H.History) -> m ()
modifyHistory f = getHistory >>= putHistory . f

-- | A pseudo-effect providing the full haskeline functionality in a single
-- effect.
--
-- @'Haskeline'@ should only ever be used inside of 'Eff' and 'Effs'
-- constraints. It is not a real effect! See 'Control.Effect.Bundle'.
type Haskeline = Bundle [Readline, ReadlineHistory, HandleInterrupt]

newtype ReadlineC m a = ReadlineC {unReadlineC :: H.InputT m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadFail,
      MonadIO,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadTrans
    )

instance MonadReader r m => MonadReader r (ReadlineC m) where
  ask = lift ask
  local f action = ReadlineC $
    H.withRunInBase $ \runInBase ->
      local f (runInBase . unReadlineC $ action)

instance MonadWriter w m => MonadWriter w (ReadlineC m) where
  tell = lift . tell
  listen action = ReadlineC $
    H.withRunInBase $ \runInBase ->
      listen (runInBase . unReadlineC $ action)
  pass action = ReadlineC $
    H.withRunInBase $ \runInBase ->
      pass (runInBase . unReadlineC $ action)

instance MonadBase b m => MonadBase b (ReadlineC m) where
  liftBase b = ReadlineC $ lift $ liftBase b

instance MonadBaseControl b m => MonadBaseControl b (ReadlineC m) where
  type StM (ReadlineC m) a = StM m a
  liftBaseWith f =
    ReadlineC $
      H.withRunInBase $ \runInputTInBase ->
        liftBaseWith $ \runMInBase ->
          f (runMInBase . runInputTInBase . unReadlineC)
  restoreM = lift . restoreM

data WithOrHandle a
  = WithInterrupts
  | OnInterruptContinueWith a

instance
  ( Carrier m,
    MonadIO m,
    MonadMask m,
    Threads ReadlineC (Prims m)
  ) =>
  Carrier (ReadlineC m)
  where
  type Derivs (ReadlineC m) = Readline ': ReadlineHistory ': HandleInterrupt ': Derivs m
  type Prims (ReadlineC m) = Optional WithOrHandle ': Prims m
  algPrims = powerAlg (thread @ReadlineC (algPrims @m)) $ \case
    Optionally WithInterrupts a ->
      ReadlineC $ H.withInterrupt (unReadlineC a)
    Optionally (OnInterruptContinueWith c) a ->
      ReadlineC $ H.handleInterrupt (pure c) (unReadlineC a)
{- ORMOLU_DISABLE -}
  reformulate =
    addDeriv
      ( \case
          GetInputLine p -> liftBase $ ReadlineC $ H.getInputLine p
          GetInputLineWithInitial p i -> liftBase $ ReadlineC $ H.getInputLineWithInitial p i
          GetInputChar p -> liftBase $ ReadlineC $ H.getInputChar p
          GetPassword m p -> liftBase $ ReadlineC $ H.getPassword m p
          WaitForAnyKey p -> liftBase $ ReadlineC $ H.waitForAnyKey p
          OutputStr s -> liftBase $ ReadlineC $ H.outputStr s
      ) $
    addDeriv
      ( \case
          GetHistory -> liftBase $ ReadlineC H.getHistory
          PutHistory h -> liftBase $ ReadlineC $ H.putHistory h
      ) $
    weakenReformUnder1 $
    addDeriv
      ( \case
          WithInterrupt a -> join $ optionally WithInterrupts (fmap pure a)
          HandleInterrupt c a ->
            join $ optionally (OnInterruptContinueWith c) (fmap pure a)
      ) $
    addPrim $
    liftReform (reformulate @m)
{- ORMOLU_ENABLE -}

instance ThreadsEff ReadlineC (Unravel p) where
  threadEff alg (Unravel p cataM main) = ReadlineC $
    H.withRunInBase $ \runInBase ->
      alg $ Unravel p (cataM . lift) (runInBase . unReadlineC $ main)

instance ThreadsEff ReadlineC (Regional s) where
  threadEff = threadRegionalViaOptional

instance ThreadsEff ReadlineC Mask where
  threadEff = threadMaskViaClass

instance Functor s => ThreadsEff ReadlineC (Optional s) where
  threadEff = threadOptionalViaBaseControl

instance ThreadsEff ReadlineC (ReaderPrim i) where
  threadEff = threadReaderPrimViaClass

instance Monoid o => ThreadsEff ReadlineC (WriterPrim o) where
  threadEff = threadWriterPrimViaClass

instance ThreadsEff ReadlineC Bracket where
  threadEff = threadBracketViaClass

instance ThreadsEff ReadlineC (Unlift b) where
  threadEff alg (Unlift main) = ReadlineC $
    H.withRunInBase $ \runInBase ->
      alg $ Unlift $ \lower -> main (lower . runInBase . unReadlineC)

instance ThreadsEff ReadlineC Split where
  threadEff alg (Split c m) = ReadlineC $
    H.withRunInBase $ \runInBase ->
      alg $ Split (c . (fmap . fmap) lift) (runInBase . unReadlineC $ m)

instance Monoid o => ThreadsEff ReadlineC (ListenPrim o) where
  threadEff = threadListenPrimViaClass

instance ThreadsEff ReadlineC (BaseControl b) where
  threadEff = threadBaseControlViaClass

instance ThreadsEff ReadlineC Fix where
  threadEff = threadFixViaClass

-- | Threading constraint for 'ReadlineC'.
--
-- 'ReadlineThreads' accepts all the primitive effects
-- (intended to be used as such) offered by in-other-words.
--
-- Most notably, 'ReadlineThreads' accepts @'Control.Effect.Unlift.Unlift' b@.
class Threads ReadlineC p => ReadlineThreads p

instance Threads ReadlineC p => ReadlineThreads p

runReadline ::
  (MonadIO m, MonadMask m) => H.Settings m -> ReadlineC m a -> m a
runReadline settings = H.runInputT settings . unReadlineC

runReadlineBehavior ::
  (MonadIO m, MonadMask m) =>
  H.Behavior ->
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadlineBehavior behavior settings =
  H.runInputTBehavior behavior settings . unReadlineC

runReadlineWithPrefs ::
  (MonadIO m, MonadMask m) =>
  H.Prefs ->
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadlineWithPrefs prefs settings =
  H.runInputTWithPrefs prefs settings . unReadlineC

runReadlineBehaviorWithPrefs ::
  (MonadIO m, MonadMask m) =>
  H.Behavior ->
  H.Prefs ->
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadlineBehaviorWithPrefs behavior prefs settings =
  H.runInputTBehaviorWithPrefs behavior prefs settings . unReadlineC
