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


-- -- Turn off and turn back on
-- aroundNeg context modK a =
--   if isJust $ find ((==) modK) context
--   then addAround modK a
--   else a
--     where
--       addAround modK a =
--         [(KeyEvent Release modK)]
--         ++ a
--         ++ [(KeyEvent Press modK)]

-- -- Turn off and turn back on
-- aroundPos context modK a =
--   if isJust $ find ((==) modK) context
--   then a
--   else addAround modK a
--     where
--       addAround modK a =
--         [(KeyEvent Press modK)]
--         ++ a
--         ++ [(KeyEvent Release modK)]

press p k = Tr.trace ("Tapping: " ++ (show k)) [(KeyEvent p k)]

keyAlt c =
  if shouldBePressed
  then press Press KeyLeftAlt
  else press Release KeyLeftAlt

    where shouldBePressed = isJust $ find (isJust . altTranslationLayer) c


translationLayer :: [Keycode] -> Keycode -> Switch -> [KeyEvent]
translationLayer c KeyLeftAlt p = press p KeyLeftAlt
translationLayer c KeyCapsLock p = press p KeyLeftCtrl
translationLayer c k p | isJust (find ((==) KeyLeftAlt) c) && isJust (altTranslationLayer k) =
  fromMaybe [] (press p (altTranslationLayer k))
translationLayer c k p | isJust (find ((==) KeyCapsLock) c) = [(press p) <$> (ctrlTranslationLayer k)]
translationLayer c k p = fromMaybe [] $ (press p) <$> (carpalxTranslationLayer k) 

-- _      @!     @at    @#    @$      @%     @*     @lpar  @rpar  @&     @^     @un    @+     @=
-- _      @1     @2     @3    @4      @5     @6     @7     @8     @9     @0     @-     _
-- _      _      _      _      _      _      _      @å     @ä     @ö     _      _

altTranslationLayer KeyQ = Just Key1
altTranslationLayer KeyW = Just Key2
altTranslationLayer KeyE = Just Key3
altTranslationLayer KeyR = Just Key4
altTranslationLayer KeyT = Just Key5
altTranslationLayer KeyY = Just Key6
altTranslationLayer KeyU = Just Key7
altTranslationLayer KeyI = Just Key8
altTranslationLayer KeyO = Just Key9
altTranslationLayer KeyP = Just Key0
altTranslationLayer KeyLeftBrace = Just KeyMinus
altTranslationLayer KeyRightBrace = Just KeyEqual

altTranslationLayer KeyA = Just Key1
altTranslationLayer KeyS = Just Key2
altTranslationLayer KeyD = Just Key3
altTranslationLayer KeyF = Just Key4
altTranslationLayer KeyG = Just Key5
altTranslationLayer KeyH = Just Key6
altTranslationLayer KeyJ = Just Key7
altTranslationLayer KeyK = Just Key8
altTranslationLayer KeyL = Just Key9
altTranslationLayer KeySemicolon = Just Key0
altTranslationLayer KeyApostrophe = Just KeyMinus
altTranslationLayer KeyEnter = Just KeyEqual

-- åäö
altTranslationLayer KeyM = Just KeyEqual
altTranslationLayer KeyComma = Just KeyEqual
altTranslationLayer KeyDot = Just KeyEqual

altTranslationLayer _ = Nothing

-- caps      _      _      _      _      _      _      _      _      _      _      _      _      _
--  _      _      _      _      @del   _      _      @bspc  _      _      _      _      _      _
--  _      _      _      _      _      _      _      @ret   _      _      _      _      _
--  _      _      _      _      _      _      _      _      _      _      _      _
--  _   _      _             _                           _      _      _      _
ctrlTranslationLayer KeyEsc = Just KeyCapsLock
ctrlTranslationLayer KeyL = Just KeyDelete
ctrlTranslationLayer KeyF = Just KeyBackspace
ctrlTranslationLayer KeyA = Just KeyEnter
ctrlTranslationLayer _ = Nothing



-- QWERTY -> Carpalx
-- Top row
-- tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
-- ->
-- tab  q    g    m    l    w    y    f    u    b    ;    [    ]    \
carpalxTranslationLayer KeyQ = Just KeyQ
carpalxTranslationLayer KeyW = Just KeyG
carpalxTranslationLayer KeyE = Just KeyM
carpalxTranslationLayer KeyT = Just KeyL
carpalxTranslationLayer KeyY = Just KeyY
carpalxTranslationLayer KeyU = Just KeyF
carpalxTranslationLayer KeyI = Just KeyU
carpalxTranslationLayer KeyO = Just KeyB
carpalxTranslationLayer KeyP = Just KeySemicolon
carpalxTranslationLayer KeyLeftBrace = Just KeyLeftBrace
carpalxTranslationLayer KeyRightBrace = Just KeyRightBrace
carpalxTranslationLayer KeyBackslash = Just KeyBackslash

-- caps a    s    d    f    g    h    j    k    l    ;    '    ret
-- ->
-- caps d    s    t    n    r    i    a    e    o    h    '    ret
carpalxTranslationLayer KeyCapsLock = Just KeyLeftCtrl
carpalxTranslationLayer KeyA = Just KeyD
carpalxTranslationLayer KeyS = Just KeyS
carpalxTranslationLayer KeyD = Just KeyT
carpalxTranslationLayer KeyF = Just KeyN
carpalxTranslationLayer KeyG = Just KeyR
carpalxTranslationLayer KeyH = Just KeyI
carpalxTranslationLayer KeyJ = Just KeyA
carpalxTranslationLayer KeyK = Just KeyE
carpalxTranslationLayer KeyL = Just KeyO
carpalxTranslationLayer KeySemicolon = Just KeyH
carpalxTranslationLayer KeyApostrophe = Just KeyApostrophe

-- lsft z    x    c    v    b    n    m    ,    .    /    rsft
-- ->
-- lsft z    x    c    v    j    k    p    ,    .    /    rsft
carpalxTranslationLayer KeyZ = Just KeyZ
carpalxTranslationLayer KeyX = Just KeyX
carpalxTranslationLayer KeyC = Just KeyC
carpalxTranslationLayer KeyV = Just KeyV
carpalxTranslationLayer KeyB = Just KeyJ
carpalxTranslationLayer KeyN = Just KeyK
carpalxTranslationLayer KeyM = Just KeyP
carpalxTranslationLayer _ = Nothing


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
