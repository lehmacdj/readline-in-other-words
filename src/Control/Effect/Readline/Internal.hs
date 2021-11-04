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
import Control.Monad.IO.Class
import Control.Monad.Trans.Control hiding (embed)
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

-- Threading constraints + EfflyIO + HelperInputT

-- | newtype that provides MonadIO when Eff (Embed IO) m and otherwise just
-- passes through instances to the base monad
newtype EfflyIO m a = EfflyIO {unEfflyIO :: m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadFail,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadBase b,
      MonadBaseControl b
    )
  deriving (MonadTrans, MonadTransControl) via IdentityT

instance Eff (Embed IO) m => MonadIO (EfflyIO m) where
  liftIO = lift . embed

-- | Version of InputT that we "own" so that we can define new instances on it,
-- in particular threading constraints without creating orphan instances.
newtype ReadlineT m a = ReadlineT {unReadlineT :: H.InputT m a}
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

instance MonadBase b m => MonadBase b (ReadlineT m) where
  liftBase b = lift $ liftBase b

instance MonadBaseControl b m => MonadBaseControl b (ReadlineT m) where
  type StM (ReadlineT m) a = StM m a
  liftBaseWith f =
    ReadlineT $
      H.withRunInBase $ \runInputTInBase ->
        liftBaseWith $ \runMInBase ->
          f (runMInBase . runInputTInBase . unReadlineT)
  restoreM = lift . restoreM

instance ThreadsEff ReadlineT (Unravel p) where
  threadEff alg (Unravel p cataM main) =
    ReadlineT $
      H.withRunInBase $ \runInBase ->
        alg $ Unravel p (cataM . lift) (runInBase . unReadlineT $ main)

instance ThreadsEff ReadlineT (Regional s) where
  threadEff = threadRegionalViaOptional

instance ThreadsEff ReadlineT Mask where
  threadEff = threadMaskViaClass

instance Functor s => ThreadsEff ReadlineT (Optional s) where
  threadEff = threadOptionalViaBaseControl

instance ThreadsEff ReadlineT (ReaderPrim i) where
  threadEff = threadReaderPrimViaRegional

instance Monoid o => ThreadsEff ReadlineT (WriterPrim o) where
  threadEff = threadWriterPrim $ \alg m -> ReadlineT $
    H.withRunInBase $ \runInBase ->
      alg $ WriterPrimPass $ runInBase . unReadlineT $ m

instance ThreadsEff ReadlineT Bracket where
  threadEff = threadBracketViaClass

instance ThreadsEff ReadlineT (Unlift b) where
  threadEff alg (Unlift main) =
    ReadlineT $
      H.withRunInBase $ \runInBase ->
        alg $ Unlift $ \lower -> main (lower . runInBase . unReadlineT)

instance ThreadsEff ReadlineT Split where
  threadEff alg (Split c m) =
    ReadlineT $
      H.withRunInBase $ \runInBase ->
        alg $ Split (c . (fmap . fmap) lift) (runInBase . unReadlineT $ m)

instance Monoid o => ThreadsEff ReadlineT (ListenPrim o) where
  threadEff = threadListenPrim $ \alg m -> ReadlineT $
    H.withRunInBase $ \runInBase ->
      alg $ ListenPrimListen $ runInBase . unReadlineT $ m

instance ThreadsEff ReadlineT (BaseControl b) where
  threadEff = threadBaseControlViaClass

instance ThreadsEff ReadlineT Fix where
  threadEff = threadFixViaClass

-- | Threading constraint for 'H.InputT'.
--
-- 'ReadlineThreads' accepts all the primitive effects
-- (intended to be used as such) offered by in-other-words.
--
-- Most notably, 'ReadlineThreads' accepts @'Control.Effect.Unlift.Unlift' b@.
class Threads ReadlineT p => ReadlineThreads p

instance Threads ReadlineT p => ReadlineThreads p

-- -- | A pseudo-effect providing the full haskeline functionality in a single
-- -- effect.
--
-- -- @'Haskeline'@ should only ever be used inside of 'Eff' and 'Effs'
-- -- constraints. It is not a real effect! See 'Control.Effect.Bundle'.
-- type Haskeline = Bundle [Readline, ReadlineHistory, HandleInterrupt]

newtype ReadlineC m a = ReadlineC {unReadlineC :: ReadlineT (EfflyIO m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadFail,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadBase b,
      MonadBaseControl b
    )
  deriving (MonadTrans) via ReadlineT

deriving newtype instance Eff (Embed IO) m => MonadIO (ReadlineC m)

readlineC :: H.InputT (EfflyIO m) a -> ReadlineC m a
readlineC = coerce

runReadlineC ::
  (H.InputT (EfflyIO m) a -> EfflyIO m a) -> ReadlineC m a -> m a
runReadlineC = coerce

instance
  ( Carrier m,
    Eff (Embed IO) m,
    MonadMask m,
    Threads ReadlineT (Prims m)
  ) =>
  Carrier (ReadlineC m)
  where
  type Derivs (ReadlineC m) = Readline ': ReadlineHistory ': Derivs m
  type Prims (ReadlineC m) = Prims m
  algPrims = coerce (thread @ReadlineT (algPrims @m))
{- ORMOLU_DISABLE -}
  reformulate =
    addDeriv
      ( \case
          GetInputLine p -> liftBase $ readlineC $ H.getInputLine p
          GetInputLineWithInitial p i -> liftBase $ readlineC $ H.getInputLineWithInitial p i
          GetInputChar p -> liftBase $ readlineC $ H.getInputChar p
          GetPassword m p -> liftBase $ readlineC $ H.getPassword m p
          WaitForAnyKey p -> liftBase $ readlineC $ H.waitForAnyKey p
          OutputStr s -> liftBase $ readlineC $ H.outputStr s
      ) $
    addDeriv
      ( \case
          GetHistory -> liftBase $ readlineC H.getHistory
          PutHistory h -> liftBase $ readlineC $ H.putHistory h
      ) $
    liftReform $
    reformulate @m
{- ORMOLU_ENABLE -}

newtype ReadlineInterruptC m a = ReadlineInterruptC {unReadlineInterruptC :: ReadlineT (EfflyIO m) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadFix,
      MonadFail,
      MonadThrow,
      MonadCatch,
      MonadMask,
      MonadBase b,
      MonadBaseControl b
    )
  deriving (MonadTrans) via ReadlineT

deriving newtype instance Eff (Embed IO) m => MonadIO (ReadlineInterruptC m)

readlineInterruptC :: H.InputT (EfflyIO m) a -> ReadlineInterruptC m a
readlineInterruptC = coerce

runReadlineInterruptC ::
  (H.InputT (EfflyIO m) a -> EfflyIO m a) -> ReadlineInterruptC m a -> m a
runReadlineInterruptC = coerce

-- | Type for denoting which kind of 'Optional' we are inside of.
data WithOrHandleInterrupt a
  = WithInterrupts
  | OnInterruptContinueWith a

instance
  ( Carrier m,
    Eff (Embed IO) m,
    MonadMask m,
    Threads ReadlineT (Prims m)
  ) =>
  Carrier (ReadlineInterruptC m)
  where
  type Derivs (ReadlineInterruptC m) = HandleInterrupt ': Derivs (ReadlineC m)
  type Prims (ReadlineInterruptC m) = Optional WithOrHandleInterrupt ': Prims (ReadlineC m)
  algPrims = powerAlg (coerce (algPrims @(ReadlineC m))) $ \case
    Optionally WithInterrupts a ->
      readlineInterruptC $ H.withInterrupt (unReadlineT $ unReadlineInterruptC a)
    Optionally (OnInterruptContinueWith c) a ->
      readlineInterruptC $ H.handleInterrupt (pure c) (unReadlineT $ unReadlineInterruptC a)
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
  (Eff (Embed IO) m, MonadMask m, Carrier m, Threaders '[ReadlineThreads] m p) =>
  H.Settings m ->
  ReadlineInterruptC m a ->
  m a
runReadline settings =
  unEfflyIO . H.runInputT (coerce settings) . unReadlineT . unReadlineInterruptC

runReadlineBehavior ::
  (Eff (Embed IO) m, MonadMask m, Carrier m, Threaders '[ReadlineThreads] m p) =>
  H.Behavior ->
  H.Settings m ->
  ReadlineInterruptC m a ->
  m a
runReadlineBehavior behavior settings =
  runReadlineInterruptC $ H.runInputTBehavior behavior (coerce settings)

runReadlineWithPrefs ::
  (Eff (Embed IO) m, MonadMask m, Carrier m, Threaders '[ReadlineThreads] m p) =>
  H.Prefs ->
  H.Settings m ->
  ReadlineInterruptC m a ->
  m a
runReadlineWithPrefs prefs settings =
  runReadlineInterruptC $ H.runInputTWithPrefs prefs (coerce settings)

runReadlineBehaviorWithPrefs ::
  (Eff (Embed IO) m, MonadMask m, Carrier m, Threaders '[ReadlineThreads] m p) =>
  H.Behavior ->
  H.Prefs ->
  H.Settings m ->
  ReadlineInterruptC m a ->
  m a
runReadlineBehaviorWithPrefs behavior prefs settings =
  runReadlineInterruptC $ H.runInputTBehaviorWithPrefs behavior prefs (coerce settings)

-- | Weaker version of 'runReadline' intended for circumstances where the
-- primitive effect 'Optional' can't be threaded. This version is incapable of
-- interpreting 'HandleInterrupt' though.
--
-- Other @'@-ed versions of interpreters are similarly just versions that don't
-- require threading 'Optional'.
runReadline' ::
  (Eff (Embed IO) m, MonadMask m, Carrier m, Threaders '[ReadlineThreads] m p) =>
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadline' settings = runReadlineC $ H.runInputT (coerce settings)

runReadlineBehavior' ::
  (Eff (Embed IO) m, MonadMask m, Carrier m, Threaders '[ReadlineThreads] m p) =>
  H.Behavior ->
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadlineBehavior' behavior settings =
  runReadlineC $ H.runInputTBehavior behavior (coerce settings)

runReadlineWithPrefs' ::
  (Eff (Embed IO) m, MonadMask m, Carrier m, Threaders '[ReadlineThreads] m p) =>
  H.Prefs ->
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadlineWithPrefs' prefs settings =
  runReadlineC $ H.runInputTWithPrefs prefs (coerce settings)

runReadlineBehaviorWithPrefs' ::
  (Eff (Embed IO) m, MonadMask m, Carrier m, Threaders '[ReadlineThreads] m p) =>
  H.Behavior ->
  H.Prefs ->
  H.Settings m ->
  ReadlineC m a ->
  m a
runReadlineBehaviorWithPrefs' behavior prefs settings =
  runReadlineC $ H.runInputTBehaviorWithPrefs behavior prefs (coerce settings)
