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

-- Here we know that the event has occurred
updateKeymap :: [MyKeyCommand] -> KeyEvent -> ([MyKeyCommand], [KeyEvent])
updateKeymap list (KeyEvent Release k) =
  (fromMaybe list $ (\a -> (filter ((==) a) list)) <$> relevantEntry, fromMaybe [] $ release <$> relevantEntry)

  where
    relevantEntry :: Maybe MyKeyCommand
    relevantEntry = find (\ (MyKeyCommand k' _) -> k == k') list

updateKeymap list (KeyEvent Press k) =
  fromMaybe (list, []) $ (\(new, out) -> (new : list, out)) <$> translationLayer (rawKey <$> list) k

keyMap :: MVar [MyKeyCommand]
keyMap = System.IO.Unsafe.unsafePerformIO $ newMVar []
{-# NOINLINE keyMap #-}


-- If I recieve a release key command, I need to make sure it's sent to be released as output.
fn :: KeyEvent -> IO [KeyEvent]
fn ke@(KeyEvent p k) = do
  cmd <- runServerPull
  m <- takeMVar keyMap
  let (m', outKeys) = updateKeymap m ke
  _ <- putMVar keyMap m'
  let mod = modifierSet (rawKey <$> m')

  pure $ mod ++ outKeys

  where
    undoKey k = (KeyEvent Release k)
    doKey k = (KeyEvent Press k)

mapKey f (KeyEvent p k) = (KeyEvent p (f k))

globalModifiersLayer :: Keycode -> Keycode
globalModifiersLayer KeyCapsLock = KeyLeftCtrl


modifierSet c =
  (keyAlt c) <> (keyCtrl c)
  where
    keyAlt c =
      if shouldBePressed
      then [KeyEvent Press KeyLeftAlt]
      else [KeyEvent Release KeyLeftAlt]
        where shouldBePressed =
                (isJust (find ((==) KeyLeftAlt) c))
                  && (isJust $ find (isJust . altTranslationLayer) c)

    keyCtrl c =
      if shouldBePressed
      then [KeyEvent Press KeyLeftCtrl]
      else [KeyEvent Release KeyLeftCtrl]
        where shouldBePressed =
                (isJust (find ((==) KeyCapsLock) c))
                && (isJust $ find (isJust . ctrlTranslationLayer) c)

-- Keycode that's being pressed (input) + way to undo it.
data MyKeyCommand = MyKeyCommand {
  rawKey :: Keycode
  , release :: [KeyEvent]
  }
  deriving (Eq, Show)

translationLayer :: [Keycode] -> Keycode -> Maybe (MyKeyCommand, [KeyEvent])

translationLayer c k | isJust (find ((==) KeyLeftAlt) c) && isJust (altTranslationLayer k) =
  standardConvert k <$> key
  where
    key = altTranslationLayer k

translationLayer c k | isJust (find ((==) KeyCapsLock) c) && isJust (ctrlTranslationLayer k) =
  standardConvert k <$> key
  where
    key = ctrlTranslationLayer k

translationLayer c k =
  standardConvert k <$> key
  where
    key = carpalxTranslationLayer k

-- k is original raw key
-- k' is modified key
standardConvert k k' =
  (MyKeyCommand k release, activate)
  where
    activate = [(KeyEvent Press k')]
    release = [(KeyEvent Release k')]

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
carpalxTranslationLayer KeyR = Just KeyL
carpalxTranslationLayer KeyT = Just KeyW
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
carpalxTranslationLayer KeyEnter = Just KeyEnter

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
carpalxTranslationLayer KeyComma = Just KeyComma
carpalxTranslationLayer KeyDot = Just KeyDot
carpalxTranslationLayer KeySlash = Just KeySlash
carpalxTranslationLayer KeyRightShift = Just KeyRightShift
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
