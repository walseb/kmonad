{-# LANGUAGE CPP #-}
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

import qualified Debug.Trace as Tr 

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
keyMap = System.IO.Unsafe.unsafePerformIO $ newMVar []
{-# NOINLINE keyMap #-}

-- If I recieve a release key command, I need to make sure it's sent to be released as output.
fn :: KeyEvent -> IO [KeyEvent]
fn ke@(KeyEvent p k) = do
  cmd <- runServerPull
  m <- takeMVar keyMap
  let m' = updateState m ke
  _ <- putMVar keyMap m'
  pure $ translationLayer m' k p

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

press k p = Tr.trace ("Tapping: " ++ (show k)) [(KeyEvent p k)]

translationLayer :: [Keycode] -> Keycode -> Switch -> [KeyEvent]
translationLayer c b p | isJust (find ((==) KeyLeftAlt) c) = altTranslationLayer c b p
translationLayer c b p | isJust (find ((==) KeyLeftCtrl) c) = ctrlTranslationLayer c b p
translationLayer c b p = press (carpalxTranslationLayer b) p

-- _      @!     @at    @#    @$      @%     @*     @lpar  @rpar  @&     @^     @un    @+     @=
-- _      @1     @2     @3    @4      @5     @6     @7     @8     @9     @0     @-     _
-- _      _      _      _      _      _      _      @å     @ä     @ö     _      _

altTranslationLayer c KeyQ p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key1 p))
altTranslationLayer c KeyW p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key2 p))
altTranslationLayer c KeyE p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key3 p))
altTranslationLayer c KeyR p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key4 p))
altTranslationLayer c KeyT p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key5 p))
altTranslationLayer c KeyY p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key6 p))
altTranslationLayer c KeyU p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key7 p))
altTranslationLayer c KeyI p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key8 p))
altTranslationLayer c KeyO p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key9 p))
altTranslationLayer c KeyP p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press Key0 p))
altTranslationLayer c KeyLeftBrace p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press KeyMinus p))
altTranslationLayer c KeyRightBrace p = aroundNeg c KeyLeftAlt (aroundPos c KeyLeftShift (press KeyEqual p))

altTranslationLayer c KeyA p = aroundNeg c KeyLeftAlt (press Key1 p)
altTranslationLayer c KeyS p = aroundNeg c KeyLeftAlt (press Key2 p)
altTranslationLayer c KeyD p = aroundNeg c KeyLeftAlt (press Key3 p)
altTranslationLayer c KeyF p = aroundNeg c KeyLeftAlt (press Key4 p)
altTranslationLayer c KeyG p = aroundNeg c KeyLeftAlt (press Key5 p)
altTranslationLayer c KeyH p = aroundNeg c KeyLeftAlt (press Key6 p)
altTranslationLayer c KeyJ p = aroundNeg c KeyLeftAlt (press Key7 p)
altTranslationLayer c KeyK p = aroundNeg c KeyLeftAlt (press Key8 p)
altTranslationLayer c KeyL p = aroundNeg c KeyLeftAlt (press Key9 p)
altTranslationLayer c KeySemicolon p = aroundNeg c KeyLeftAlt (press Key0 p)
altTranslationLayer c KeyApostrophe p = aroundNeg c KeyLeftAlt (press KeyMinus p)
altTranslationLayer c KeyEnter p = aroundNeg c KeyLeftAlt (press KeyEqual p)

altTranslationLayer c KeyM p = aroundNeg c KeyLeftAlt (press KeyEqual p)
altTranslationLayer c KeyComma p = aroundNeg c KeyLeftAlt (press KeyEqual p)
altTranslationLayer c KeyDot p = aroundNeg c KeyLeftAlt (press KeyEqual p)

-- caps      _      _      _      _      _      _      _      _      _      _      _      _      _
--  _      _      _      _      @del   _      _      @bspc  _      _      _      _      _      _
--  _      _      _      _      _      _      _      @ret   _      _      _      _      _
--  _      _      _      _      _      _      _      _      _      _      _      _
--  _   _      _             _                           _      _      _      _
ctrlTranslationLayer c KeyEsc p = aroundNeg c KeyLeftCtrl (press KeyCapsLock p)
ctrlTranslationLayer c KeyL p = aroundNeg c KeyLeftCtrl (press KeyDelete p)
ctrlTranslationLayer c KeyF p = aroundNeg c KeyLeftCtrl (press KeyBackspace p)
ctrlTranslationLayer c KeyA p = aroundNeg c KeyLeftCtrl (press KeyEnter p)



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
