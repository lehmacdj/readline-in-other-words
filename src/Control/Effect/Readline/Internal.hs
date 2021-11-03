{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
import Control.Monad.Trans.Control
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

-- -- | A pseudo-effect providing the full haskeline functionality in a single
-- -- effect.
--
-- -- @'Haskeline'@ should only ever be used inside of 'Eff' and 'Effs'
-- -- constraints. It is not a real effect! See 'Control.Effect.Bundle'.
-- type Haskeline = Bundle [Readline, ReadlineHistory, HandleInterrupt]

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

instance MonadBase b m => MonadBase b (ReadlineC m) where
  liftBase b = lift $ liftBase b

instance MonadBaseControl b m => MonadBaseControl b (ReadlineC m) where
  type StM (ReadlineC m) a = StM m a
  liftBaseWith f =
    ReadlineC $
      H.withRunInBase $ \runInputTInBase ->
        liftBaseWith $ \runMInBase ->
          f (runMInBase . runInputTInBase . unReadlineC)
  restoreM = lift . restoreM

instance
  ( Carrier m,
    MonadIO m,
    MonadMask m,
    Threads H.InputT (Prims m)
  ) =>
  Carrier (ReadlineC m)
  where
  type Derivs (ReadlineC m) = Readline ': ReadlineHistory ': Derivs m
  type Prims (ReadlineC m) = Prims m
  algPrims = coerce (thread @H.InputT (algPrims @m))
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
    liftReform $
    reformulate @m
{- ORMOLU_ENABLE -}

newtype ReadlineInterruptC m a = ReadlineInterruptC {unReadlineInterruptC :: H.InputT m a}
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

deriving via
  ReadlineC m
  instance
    MonadBase b m =>
    MonadBase b (ReadlineInterruptC m)

deriving via
  ReadlineC m
  instance
    MonadBaseControl b m =>
    MonadBaseControl b (ReadlineInterruptC m)

-- | Type for denoting which kind of 'Optional' we are inside of.
data WithOrHandle a
  = WithInterrupts
  | OnInterruptContinueWith a

instance
  ( Carrier m,
    MonadIO m,
    MonadMask m,
    Threads H.InputT (Prims m)
  ) =>
  Carrier (ReadlineInterruptC m)
  where
  type Derivs (ReadlineInterruptC m) = HandleInterrupt ': Derivs (ReadlineC m)
  type Prims (ReadlineInterruptC m) = Optional WithOrHandle ': Prims (ReadlineC m)
  algPrims = powerAlg (coerce (algPrims @(ReadlineC m))) $ \case
    Optionally WithInterrupts a ->
      ReadlineInterruptC $ H.withInterrupt (unReadlineInterruptC a)
    Optionally (OnInterruptContinueWith c) a ->
      ReadlineInterruptC $ H.handleInterrupt (pure c) (unReadlineInterruptC a)
{- ORMOLU_DISABLE -}
  reformulate =
    weakenReformUnder1 $
    addDeriv
      ( \case
          WithInterrupt a -> join $ optionally WithInterrupts (fmap pure a)
          HandleInterrupt c a ->
            join $ optionally (OnInterruptContinueWith c) (fmap pure a)
      ) $
    addPrim $
    coerceReform $
    reformulate @(ReadlineC m)
{- ORMOLU_ENABLE -}

-- orphan instances for InputT: MonadBase(Control) and Threads

instance ThreadsEff ReadlineC (Unravel p) where
  threadEff alg (Unravel p cataM main) =
    ReadlineC $
      H.withRunInBase $ \runInBase ->
        alg $ Unravel p (cataM . lift) (runInBase . unReadlineC $ main)

instance ThreadsEff ReadlineC (Regional s) where
  threadEff = threadRegionalViaOptional

instance ThreadsEff ReadlineC Mask where
  threadEff = threadMaskViaClass

instance Functor s => ThreadsEff ReadlineC (Optional s) where
  threadEff = threadOptionalViaBaseControl

instance ThreadsEff ReadlineC (ReaderPrim i) where
  threadEff = threadReaderPrimViaRegional

instance Monoid o => ThreadsEff ReadlineC (WriterPrim o) where
  threadEff = threadWriterPrim $ \alg m -> ReadlineC $
    H.withRunInBase $ \runInBase ->
      alg $ WriterPrimPass $ runInBase . unReadlineC $ m

instance ThreadsEff ReadlineC Bracket where
  threadEff = threadBracketViaClass

instance ThreadsEff ReadlineC (Unlift b) where
  threadEff alg (Unlift main) =
    ReadlineC $
      H.withRunInBase $ \runInBase ->
        alg $ Unlift $ \lower -> main (lower . runInBase . unReadlineC)

instance ThreadsEff ReadlineC Split where
  threadEff alg (Split c m) =
    ReadlineC $
      H.withRunInBase $ \runInBase ->
        alg $ Split (c . (fmap . fmap) lift) (runInBase . unReadlineC $ m)

instance Monoid o => ThreadsEff ReadlineC (ListenPrim o) where
  threadEff = threadListenPrim $ \alg m -> ReadlineC $
    H.withRunInBase $ \runInBase ->
      alg $ ListenPrimListen $ runInBase . unReadlineC $ m

instance ThreadsEff ReadlineC (BaseControl b) where
  threadEff = threadBaseControlViaClass

instance ThreadsEff ReadlineC Fix where
  threadEff = threadFixViaClass

-- | Threading constraint for 'H.InputT'.
--
-- 'ReadlineThreads' accepts all the primitive effects
-- (intended to be used as such) offered by in-other-words.
--
-- Most notably, 'ReadlineThreads' accepts @'Control.Effect.Unlift.Unlift' b@.
class Threads H.InputT p => ReadlineThreads p

instance Threads H.InputT p => ReadlineThreads p

-- | Main interpreter for 'Readline', 'ReadlineHistory', and 'HandleInterrupt'
-- effects. 'H.defaultSettings' exists as a default for settings.
--
-- Example usage:
--
-- > import Control.Effect
-- > import Control.Effect.Readline
-- >
-- > repl :: Effs '[Readline, HandleInterrupt] m => m ()
-- > repl = handleInterrupt (outputStrLn "Interrupt!" *> repl) $
-- >   withInterrupt $ do
-- >     mline <- getInputLine "> "
-- >     case mline of
-- >       Nothing -> pure ()
-- >       Just line -> outputStrLn line *> repl
-- >
-- > main :: IO ()
-- > main = runM $ runReadline defaultSettings repl
runReadline ::
  (MonadIO m, MonadMask m, Carrier m) =>
  H.Settings m ->
  ReadlineInterruptC m a ->
  m a
runReadline settings = H.runInputT settings . unReadlineInterruptC

runReadlineBehavior ::
  (MonadIO m, MonadMask m, Carrier m) =>
  H.Behavior ->
  H.Settings m ->
  ReadlineInterruptC m a ->
  m a
runReadlineBehavior behavior settings =
  H.runInputTBehavior behavior settings . unReadlineInterruptC

runReadlineWithPrefs ::
  (MonadIO m, MonadMask m, Carrier m) =>
  H.Prefs ->
  H.Settings m ->
  ReadlineInterruptC m a ->
  m a
runReadlineWithPrefs prefs settings =
  H.runInputTWithPrefs prefs settings . unReadlineInterruptC

runReadlineBehaviorWithPrefs ::
  (MonadIO m, MonadMask m, Carrier m) =>
  H.Behavior ->
  H.Prefs ->
  H.Settings m ->
  ReadlineInterruptC m a ->
  m a
runReadlineBehaviorWithPrefs behavior prefs settings =
  H.runInputTBehaviorWithPrefs behavior prefs settings . unReadlineInterruptC

-- | Weaker version of 'runReadline' intended for circumstances where the
-- primitive effect 'Optional' can't be threaded. This version is incapable of
-- interpreting 'HandleInterrupt' though.
--
-- Other @'@-ed versions of interpreters are similarly just versions that don't
-- require threading 'Optional'.
runReadline' ::
  (MonadIO m, MonadMask m, Carrier m) =>
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadline' settings = H.runInputT settings . unReadlineC

runReadlineBehavior' ::
  (MonadIO m, MonadMask m, Carrier m) =>
  H.Behavior ->
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadlineBehavior' behavior settings =
  H.runInputTBehavior behavior settings . unReadlineC

runReadlineWithPrefs' ::
  (MonadIO m, MonadMask m, Carrier m) =>
  H.Prefs ->
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadlineWithPrefs' prefs settings =
  H.runInputTWithPrefs prefs settings . unReadlineC

runReadlineBehaviorWithPrefs' ::
  (MonadIO m, MonadMask m, Carrier m) =>
  H.Behavior ->
  H.Prefs ->
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadlineBehaviorWithPrefs' behavior prefs settings =
  H.runInputTBehaviorWithPrefs behavior prefs settings . unReadlineC
