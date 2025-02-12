{-# LANGUAGE CPP #-}
{-|
Module      : KMonad.App.Main
Description : The entry-point to KMonad
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable

-}
module KMonad.MyMain where

import KMonad.Prelude
import KMonad.Prelude.Imports

import KMonad.Args
import KMonad.App.Types
import KMonad.Keyboard
import KMonad.Util
import KMonad.Model


import KMonad.Keyboard.IO
import qualified KMonad.Model.Dispatch as Dp
import qualified KMonad.Model.Hooks    as Hs
import qualified KMonad.Model.Sluice   as Sl
import qualified KMonad.Model.Keymap   as Km

import KMonad.My
-- import qualified KMonad.Prelude.Imports as KPrelude
import qualified System.IO.Unsafe

import Data.List


-- FIXME: This should live somewhere else

#ifdef linux_HOST_OS
import System.Posix.Signals (Handler(Ignore), installHandler, sigCHLD)
#endif


initAppEnv :: HasLogFunc e => AppCfg -> ContT r (RIO e) AppEnv
initAppEnv cfg = do
  -- Get a reference to the logging function
  lgf <- view logFuncL

  -- Wait a bit for the user to release the 'Return' key with which they started KMonad
  threadDelay $ fromIntegral (cfg^.startDelay) * 1000

  -- Acquire the key source and key sink
  snk <- using $ cfg^.keySinkDev
  src <- using $ cfg^.keySourceDev

  launch_ "emitter_proc_rpc" $ do
    liftIO $ launchServer serverMVar

  -- emit e = view keySink >>= flip emitKey e
  pure $ AppEnv
    { _keAppCfg  = undefined
    , _keLogFunc = lgf
    , _keySink   = snk
    , _keySource = src

    , _dispatch  = undefined
    , _inHooks   = undefined
    , _sluice    = undefined

    , _keymap    = undefined
    , _outHooks  = undefined
    , _outVar    = undefined
    }


-- | Run KMonad using the provided configuration
startApp :: HasLogFunc e => AppCfg -> RIO e ()
startApp c = do
#ifdef linux_HOST_OS
  -- Ignore SIGCHLD to avoid zombie processes.
  liftIO . void $ installHandler sigCHLD Ignore Nothing
#endif
  runContT (initAppEnv c) (`runRIO` loop)

loop :: RIO AppEnv ()
loop = forever $ do
  src <- view keySource
  ev <- pull' src
  snk <- (view keySink)
  emitKey snk  ev

-- newtype Key = Key (Maybe KeyEvent)

-- combineKeyEvent :: KeyEvent -> KeyEvent -> KeyEvent
-- combineKeyEvent (KeyEvent Press k) = undefined
-- combineKeyEvent (KeyEvent Release k) = undefined

-- instance Semigroup Key where
--   (<>) (Key a) (Key b) = Key $ combineKeyEvent <$> a <*> b

-- Here we know that the event has occurred
comb :: [KeyEvent] -> KeyEvent -> KeyEvent -> [KeyEvent]
comb list new old = undefined

updateState :: [KeyEvent] -> KeyEvent -> [KeyEvent]
updateState evs k =
  let match = find ((==) k) evs in
  maybe evs (comb evs k) match

keyMap :: MVar [KeyEvent]
keyMap = System.IO.Unsafe.unsafePerformIO $ newEmptyMVar
{-# NOINLINE keyMap #-}

fn :: KeyEvent -> IO KeyEvent
fn = undefined

runServerPull :: (HasAppEnv e, HasLogFunc e, HasAppCfg e) => RIO e ()
runServerPull = do
  mvar <- tryTakeMVar serverMVar
  case mvar of
    Just a -> do
      logInfo $ "Executing remote command!"
      executeServerCmd a
    Nothing -> pure ()

pull' :: (HasAppEnv e, HasLogFunc e, HasAppCfg e) => KeySource -> RIO e KeyEvent
pull' s = awaitKey s >>=
  -- Running the command from the server should always run right before the key is run
  (\a -> runServerPull >> (pure a))
  >>= liftIO . fn

-- | The first command in KMonad
--
-- Get the invocation from the command-line, then do something with it.
main :: IO ()
main = getCmd >>= runCmd

-- | Execute the provided 'Cmd'
--
-- 1. Construct the log-func
-- 2. Parse the config-file
-- 3. Maybe start KMonad
runCmd :: Cmd -> IO ()
runCmd c = do
  hSetBuffering stdout LineBuffering
  o <- logOptionsHandle stdout False <&> setLogMinLevel (c^.logLvl)
  withLogFunc o $ \f -> runRIO f $ do
    cfg <- loadConfig c
    unless (c^.dryRun) $ startApp cfg
