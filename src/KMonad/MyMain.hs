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


import KMonad.Keyboard.Keycode
import KMonad.Keyboard.Types
import KMonad.Keyboard.IO
import qualified KMonad.Model.Dispatch as Dp
import qualified KMonad.Model.Hooks    as Hs
import qualified KMonad.Model.Sluice   as Sl
import qualified KMonad.Model.Keymap   as Km

import KMonad.My
-- import qualified KMonad.Prelude.Imports as KPrelude
import qualified System.IO.Unsafe

import KMonad.MyTypes
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
  sequence $ (emitKey snk) <$> ev

-- newtype Key = Key (Maybe KeyEvent)

-- combineKeyEvent :: KeyEvent -> KeyEvent -> KeyEvent
-- combineKeyEvent (KeyEvent Press k) = undefined
-- combineKeyEvent (KeyEvent Release k) = undefined

-- instance Semigroup Key where
--   (<>) (Key a) (Key b) = Key $ combineKeyEvent <$> a <*> b

-- Here we know that the event has occurred
updateState :: [Keycode] -> KeyEvent -> [Keycode]
updateState list (KeyEvent Release k) = filter ((/=) k) list
updateState list (KeyEvent Press k) = k : list

keyMap :: MVar [Keycode]
keyMap = System.IO.Unsafe.unsafePerformIO $ newEmptyMVar
{-# NOINLINE keyMap #-}

-- If I recieve a release key command, I need to make sure it's sent to be released as output.
fn :: KeyEvent -> IO [KeyEvent]
fn ke@(KeyEvent p k) = do
  cmd <- runServerPull
  m <- takeMVar keyMap
  let m' = updateState m ke
  _ <- putMVar keyMap m'
  if p == Press
  then pure $ (translationLayer m' k)
  else pure []
  
mapKey f (KeyEvent p k) = (KeyEvent p (f k))

globalModifiersLayer :: Keycode -> Keycode
globalModifiersLayer KeyCapsLock = KeyLeftCtrl


-- Turn off and turn back on
aroundNeg context modK a =
  if isJust $ find ((==) modK) context
  then addAround modK a
  else a
    where
      addAround modK a =
        [(KeyEvent Release modK)]
        ++ a
        ++ [(KeyEvent Press modK)]

-- Turn off and turn back on
aroundPos context modK a =
  if isJust $ find ((==) modK) context
  then a
  else addAround modK a
    where
      addAround modK a =
        [(KeyEvent Press modK)]
        ++ a
        ++ [(KeyEvent Release modK)]

tap k = [(KeyEvent Press k), (KeyEvent Release k)]

translationLayer :: [Keycode] -> Keycode -> [KeyEvent]
translationLayer c b | isJust (find ((==) KeyLeftAlt) c) = altTranslationLayer c b
translationLayer c b | isJust (find ((==) KeyLeftCtrl) c) = ctrlTranslationLayer c b
translationLayer c b = tap (carpalxTranslationLayer b)

-- _      @!     @at    @#    @$      @%     @*     @lpar  @rpar  @&     @^     @un    @+     @=
-- _      @1     @2     @3    @4      @5     @6     @7     @8     @9     @0     @-     _
-- _      _      _      _      _      _      _      @å     @ä     @ö     _      _
altTranslationLayer c KeyQ = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key1))
altTranslationLayer c KeyW = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key2))
altTranslationLayer c KeyE = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key3))
altTranslationLayer c KeyR = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key4))
altTranslationLayer c KeyT = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key5))
altTranslationLayer c KeyY = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key6))
altTranslationLayer c KeyU = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key7))
altTranslationLayer c KeyI = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key8))
altTranslationLayer c KeyO = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key9))
altTranslationLayer c KeyP = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap Key0))
altTranslationLayer c KeyLeftBrace = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap KeyMinus))
altTranslationLayer c KeyRightBrace = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (tap KeyEqual))

altTranslationLayer c KeyA = aroundNeg c KeyLeftAlt (tap Key1)
altTranslationLayer c KeyS = aroundNeg c KeyLeftAlt (tap Key2)
altTranslationLayer c KeyD = aroundNeg c KeyLeftAlt (tap Key3)
altTranslationLayer c KeyF = aroundNeg c KeyLeftAlt (tap Key4)
altTranslationLayer c KeyG = aroundNeg c KeyLeftAlt (tap Key5)
altTranslationLayer c KeyH = aroundNeg c KeyLeftAlt (tap Key6)
altTranslationLayer c KeyJ = aroundNeg c KeyLeftAlt (tap Key7)
altTranslationLayer c KeyK = aroundNeg c KeyLeftAlt (tap Key8)
altTranslationLayer c KeyL = aroundNeg c KeyLeftAlt (tap Key9)
altTranslationLayer c KeySemicolon = aroundNeg c KeyLeftAlt (tap Key0)
altTranslationLayer c KeyApostrophe = aroundNeg c KeyLeftAlt (tap KeyMinus)
altTranslationLayer c KeyEnter = aroundNeg c KeyLeftAlt (tap KeyEqual)

altTranslationLayer c KeyM = aroundNeg c KeyLeftAlt (tap KeyEqual)
altTranslationLayer c KeyComma = aroundNeg c KeyLeftAlt (tap KeyEqual)
altTranslationLayer c KeyDot = aroundNeg c KeyLeftAlt (tap KeyEqual)

-- caps      _      _      _      _      _      _      _      _      _      _      _      _      _
--  _      _      _      _      @del   _      _      @bspc  _      _      _      _      _      _
--  _      _      _      _      _      _      _      @ret   _      _      _      _      _
--  _      _      _      _      _      _      _      _      _      _      _      _
--  _   _      _             _                           _      _      _      _
ctrlTranslationLayer c KeyEsc = aroundNeg c KeyLeftCtrl (tap KeyCapsLock)
ctrlTranslationLayer c KeyL = aroundNeg c KeyLeftCtrl (tap KeyDelete)
ctrlTranslationLayer c KeyF = aroundNeg c KeyLeftCtrl (tap KeyBackspace)
ctrlTranslationLayer c KeyA = aroundNeg c KeyLeftCtrl (tap KeyEnter)


carpalxTranslationLayer :: Keycode -> Keycode

-- QWERTY -> Carpalx
-- Top row
-- tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
-- ->
-- tab  q    g    m    l    w    y    f    u    b    ;    [    ]    \
carpalxTranslationLayer KeyQ = KeyQ
carpalxTranslationLayer KeyW = KeyG
carpalxTranslationLayer KeyE = KeyM
carpalxTranslationLayer KeyT = KeyL
carpalxTranslationLayer KeyY = KeyY
carpalxTranslationLayer KeyU = KeyF
carpalxTranslationLayer KeyI = KeyU
carpalxTranslationLayer KeyO = KeyB
carpalxTranslationLayer KeyP = KeySemicolon
carpalxTranslationLayer KeyLeftBrace = KeyLeftBrace
carpalxTranslationLayer KeyRightBrace = KeyRightBrace
carpalxTranslationLayer KeyBackslash = KeyBackslash

-- caps a    s    d    f    g    h    j    k    l    ;    '    ret
-- ->
-- caps d    s    t    n    r    i    a    e    o    h    '    ret
carpalxTranslationLayer KeyA = KeyD
carpalxTranslationLayer KeyS = KeyS
carpalxTranslationLayer KeyD = KeyT
carpalxTranslationLayer KeyF = KeyN
carpalxTranslationLayer KeyG = KeyR
carpalxTranslationLayer KeyH = KeyI
carpalxTranslationLayer KeyJ = KeyA
carpalxTranslationLayer KeyK = KeyE
carpalxTranslationLayer KeyL = KeyO
carpalxTranslationLayer KeySemicolon = KeyH
carpalxTranslationLayer KeyApostrophe = KeyApostrophe

-- lsft z    x    c    v    b    n    m    ,    .    /    rsft
-- ->
-- lsft z    x    c    v    j    k    p    ,    .    /    rsft
carpalxTranslationLayer KeyZ = KeyZ
carpalxTranslationLayer KeyX = KeyX
carpalxTranslationLayer KeyC = KeyC
carpalxTranslationLayer KeyV = KeyV
carpalxTranslationLayer KeyB = KeyJ
carpalxTranslationLayer KeyN = KeyK
carpalxTranslationLayer KeyM = KeyP


runServerPull :: IO (Maybe ServerCmds)
runServerPull = do
  tryTakeMVar serverMVar

pull' :: (HasAppEnv e, HasLogFunc e, HasAppCfg e) => KeySource -> RIO e [KeyEvent]
pull' s = awaitKey s >>=
  liftIO . fn

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
