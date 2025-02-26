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
-- Key release
updateKeymap :: [MyKeyCommand] -> KeyEvent -> ([MyKeyCommand], Maybe (Switch, MyKeyCommand), [KeyEvent])
updateKeymap list (KeyEvent Release k) =
  (fromMaybe list $ (\a -> (filter ((/=) a) list)) <$> relevantEntry,
    (\a -> (Release, a)) <$> relevantEntry,
    fromMaybe [] $ release <$> relevantEntry)

  where
    relevantEntry :: Maybe MyKeyCommand
    relevantEntry = find (\ (MyKeyCommand k' _ _ _) -> k == k') list

-- Key press
updateKeymap list (KeyEvent Press k) =
  Tr.trace ("New entry: " ++ (show newEntry)) $
    fromMaybe (list, Nothing, []) $ (\new -> (new : list, Just (Press, new), activation new)) <$> newEntry
    where
      newEntry = translationLayer (concat (mods <$> list)) k

keyMap :: MVar [MyKeyCommand]
keyMap = System.IO.Unsafe.unsafePerformIO $ newMVar []
{-# NOINLINE keyMap #-}

-- TODO:
-- Alt needs to engage shift whenever relevant
-- Reduce excess keys being fired by having the modifier functions check what state the modifier key is in. This is an issue since the normal output function will also send out modifier key presses, which might mess this up?

-- If I recieve a release key command, I need to make sure it's sent to be released as output.
fn :: KeyEvent -> IO [KeyEvent]
fn ke = do
  -- "recieved"

  () <- Tr.trace ("Injecting event: " ++ show ke) (pure ())
  cmd <- runServerPull
  m <- takeMVar keyMap

  () <- Tr.trace ("Current keymap: " ++ show m) (pure ())
  let (m', curr, outKeys) = updateKeymap m ke
  () <- Tr.trace ("Current key: " ++ show curr) (pure ())
  () <- Tr.trace ("Updated keymap: " ++ show m') (pure ())
  _ <- putMVar keyMap m'
  let mod = modifierSet m' curr

  () <- Tr.trace ("Outputting events: " ++ show (mod ++ outKeys)) (pure ())
  pure $ mod ++ outKeys

  where
    undoKey k = (KeyEvent Release k)
    doKey k = (KeyEvent Press k)

mapKey f (KeyEvent p k) = (KeyEvent p (f k))

modifierSet c (Just (Release, curr)) =
  -- If the key request is unique, just release it
  (concat (deleteRequirement <$> unique))
    -- If the key request isn't unique, then apply the
    ++ (applyMods <$> nonUnique)
  where
    cMod = (concat $ mods <$> c)
    unique = filter (\a -> not (any (eqMod a) cMod)) (mods curr)

    nonUnique = filter (\a -> any (eqMod a) (mods curr)) cMod 

    deleteRequirement (ModShift Press) = [KeyEvent Release KeyLeftShift]
    deleteRequirement (ModAlt Press) = [KeyEvent Release KeyLeftAlt]
    deleteRequirement (ModCtrl Press) = [KeyEvent Release KeyLeftCtrl]
    deleteRequirement _ = []

    -- nonUnique = modDeleteDuplicates $ filter (not . (`elem` cMod)) (mods curr)

modifierSet _ (Just (Press, curr)) =
  applyMods <$> mods curr

modifierSet _ Nothing =
  []

modDeleteDuplicates c = foldr
                (\a b ->
                  if any (eqMod a) b
                  then b
                  else (a : b))
                []
                c

eqMod (ModShift _) (ModShift _) = True
eqMod (ModAlt _) (ModAlt _) = True
eqMod (ModCtrl _) (ModCtrl _) = True
eqMod _ _ = False


applyMods (ModShift p) = KeyEvent p KeyLeftShift
applyMods (ModAlt p) = KeyEvent p KeyLeftAlt
applyMods (ModCtrl p) = KeyEvent p KeyLeftCtrl

revSwitch Press = Release
revSwitch Release = Press

mapMod f (ModShift p)  = ModShift (f p)
mapMod f (ModAlt p) = ModAlt (f p)
mapMod f (ModCtrl p) = ModCtrl (f p)

data MyModifiersRequested = ModShift Switch | ModAlt Switch | ModCtrl Switch
  deriving (Eq, Show)

-- Keycode that's being pressed (input) + way to undo it.
data MyKeyCommand = MyKeyCommand {
  rawKey :: Keycode
  , activation :: [KeyEvent]
  , release :: [KeyEvent]
  , mods :: [MyModifiersRequested]
  }
  deriving (Eq, Show)

translationLayer :: [MyModifiersRequested] -> Keycode -> Maybe (MyKeyCommand)

translationLayer mod k | any findAlt mod && isJust (altTranslationLayer k) =
  altTranslationLayer k
  where
    findAlt (ModAlt Press) = True
    findAlt _ = False

translationLayer mod k | any findCtrl mod && isJust (ctrlTranslationLayer k) =
  ctrlTranslationLayer k
  where
    findCtrl (ModCtrl Press) = True
    findCtrl _ = False

translationLayer _ k =
  carpalxTranslationLayer k

-- _      @!     @at    @#    @$      @%     @*     @lpar  @rpar  @&     @^     @un    @+     @=
-- _      @1     @2     @3    @4      @5     @6     @7     @8     @9     @0     @-     _
-- _      _      _      _      _      _      _      @å     @ä     @ö     _      _

keyCommand k k' mod =
  MyKeyCommand
    k
    [KeyEvent Press k']
    [KeyEvent Release k']
    mod

keyMod k mod =
  MyKeyCommand
    k
    []
    []
    mod

altTranslationLayer :: Keycode -> Maybe MyKeyCommand
altTranslationLayer k@KeyQ = Just $ keyCommand k Key1 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyW = Just $ keyCommand k Key2 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyE = Just $ keyCommand k Key3 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyR = Just $ keyCommand k Key4 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyT = Just $ keyCommand k Key5 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyY = Just $ keyCommand k Key6 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyU = Just $ keyCommand k Key7 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyI = Just $ keyCommand k Key8 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyO = Just $ keyCommand k Key9 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyP = Just $ keyCommand k Key0 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyLeftBrace = Just $ keyCommand k KeyMinus [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyRightBrace = Just $ keyCommand k KeyEqual [(ModShift Press), (ModAlt Release)]

altTranslationLayer k@KeyA = Just $ keyCommand k Key1 [(ModAlt Release)]
altTranslationLayer k@KeyS = Just $ keyCommand k Key2 [(ModAlt Release)]
altTranslationLayer k@KeyD = Just $ keyCommand k Key3 [(ModAlt Release)]
altTranslationLayer k@KeyF = Just $ keyCommand k Key4 [(ModAlt Release)]
altTranslationLayer k@KeyG = Just $ keyCommand k Key5 [(ModAlt Release)]
altTranslationLayer k@KeyH = Just $ keyCommand k Key6 [(ModAlt Release)]
altTranslationLayer k@KeyJ = Just $ keyCommand k Key7 [(ModAlt Release)]
altTranslationLayer k@KeyK = Just $ keyCommand k Key8 [(ModAlt Release)]
altTranslationLayer k@KeyL = Just $ keyCommand k Key9 [(ModAlt Release)]
altTranslationLayer k@KeySemicolon = Just $ keyCommand k Key0 [(ModAlt Release)]
altTranslationLayer k@KeyApostrophe = Just $ keyCommand k KeyMinus [(ModAlt Release)]
altTranslationLayer k@KeyEnter = Just $ keyCommand k KeyEqual [(ModAlt Release)]

-- åäö
altTranslationLayer k@KeyM = Just $ keyCommand k KeyEqual [(ModAlt Release)]
altTranslationLayer k@KeyComma = Just $ keyCommand k KeyEqual [(ModAlt Release)]
altTranslationLayer k@KeyDot = Just $ keyCommand k KeyEqual [(ModAlt Release)]

altTranslationLayer _ = Nothing

-- caps      _      _      _      _      _      _      _      _      _      _      _      _      _
--  _      _      _      _      @del   _      _      @bspc  _      _      _      _      _      _
--  _      _      _      _      _      _      _      @ret   _      _      _      _      _
--  _      _      _      _      _      _      _      _      _      _      _      _
--  _   _      _             _                           _      _      _      _
ctrlTranslationLayer :: Keycode -> Maybe MyKeyCommand
ctrlTranslationLayer k@KeyEsc = Just $ keyCommand k KeyCapsLock [(ModCtrl Release)]
ctrlTranslationLayer k@KeyL = Just $ keyCommand k KeyDelete [(ModCtrl Release)]
ctrlTranslationLayer k@KeyF = Just $ keyCommand k KeyBackspace [(ModCtrl Release)]
ctrlTranslationLayer k@KeyA = Just $ keyCommand k KeyEnter [(ModCtrl Release)]
ctrlTranslationLayer _ = Nothing



-- QWERTY -> Carpalx
carpalxTranslationLayer :: Keycode -> Maybe MyKeyCommand
carpalxTranslationLayer k@KeyEsc = Just $ keyCommand k KeyEsc []
carpalxTranslationLayer k@KeyHome = Just $ keyCommand k KeyHome []
carpalxTranslationLayer k@KeyEnd = Just $ keyCommand k KeyEnd []
carpalxTranslationLayer k@KeyInsert = Just $ keyCommand k KeyInsert []
carpalxTranslationLayer k@KeyF1 = Just $ keyCommand k KeyF1 []
carpalxTranslationLayer k@KeyF2 = Just $ keyCommand k KeyF2 []
carpalxTranslationLayer k@KeyF3 = Just $ keyCommand k KeyF3 []
carpalxTranslationLayer k@KeyF4 = Just $ keyCommand k KeyF4 []
carpalxTranslationLayer k@KeyF5 = Just $ keyCommand k KeyF5 []
carpalxTranslationLayer k@KeyF6 = Just $ keyCommand k KeyF6 []
carpalxTranslationLayer k@KeyF7 = Just $ keyCommand k KeyF7 []
carpalxTranslationLayer k@KeyF8 = Just $ keyCommand k KeyF8 []
carpalxTranslationLayer k@KeyF9 = Just $ keyCommand k KeyF9 []
carpalxTranslationLayer k@KeyF10 = Just $ keyCommand k KeyF10 []

-- grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
-- ->
-- grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
carpalxTranslationLayer k@KeyGrave = Just $ keyCommand k KeyGrave []
carpalxTranslationLayer k@Key1 = Just $ keyCommand k Key1 []
carpalxTranslationLayer k@Key2 = Just $ keyCommand k Key2 []
carpalxTranslationLayer k@Key3 = Just $ keyCommand k Key3 []
carpalxTranslationLayer k@Key4 = Just $ keyCommand k Key4 []
carpalxTranslationLayer k@Key5 = Just $ keyCommand k Key5 []
carpalxTranslationLayer k@Key6 = Just $ keyCommand k Key6 []
carpalxTranslationLayer k@Key7 = Just $ keyCommand k Key7 []
carpalxTranslationLayer k@Key8 = Just $ keyCommand k Key8 []
carpalxTranslationLayer k@Key9 = Just $ keyCommand k Key9 []
carpalxTranslationLayer k@Key0 = Just $ keyCommand k Key0 []
carpalxTranslationLayer k@KeyMinus = Just $ keyCommand k KeyMinus []
carpalxTranslationLayer k@KeyEqual = Just $ keyCommand k KeyEqual []
carpalxTranslationLayer k@KeyBackspace = Just $ keyCommand k KeyBackspace []


-- Top row
-- tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
-- ->
-- tab  q    g    m    l    w    y    f    u    b    ;    [    ]    \
carpalxTranslationLayer k@KeyTab = Just $ keyCommand k KeyTab []
carpalxTranslationLayer k@KeyQ = Just $ keyCommand k KeyQ []
carpalxTranslationLayer k@KeyW = Just $ keyCommand k KeyG []
carpalxTranslationLayer k@KeyE = Just $ keyCommand k KeyM []
carpalxTranslationLayer k@KeyR = Just $ keyCommand k KeyL []
carpalxTranslationLayer k@KeyT = Just $ keyCommand k KeyW []
carpalxTranslationLayer k@KeyY = Just $ keyCommand k KeyY []
carpalxTranslationLayer k@KeyU = Just $ keyCommand k KeyF []
carpalxTranslationLayer k@KeyI = Just $ keyCommand k KeyU []
carpalxTranslationLayer k@KeyO = Just $ keyCommand k KeyB []
carpalxTranslationLayer k@KeyP = Just $ keyCommand k KeySemicolon []
carpalxTranslationLayer k@KeyLeftBrace = Just $ keyCommand k KeyLeftBrace []
carpalxTranslationLayer k@KeyRightBrace = Just $ keyCommand k KeyRightBrace []
carpalxTranslationLayer k@KeyBackslash = Just $ keyCommand k KeyBackslash []

-- caps a    s    d    f    g    h    j    k    l    ;    '    ret
-- ->
-- caps d    s    t    n    r    i    a    e    o    h    '    ret
carpalxTranslationLayer k@KeyCapsLock = Just $ keyMod k [ModCtrl Press]
carpalxTranslationLayer k@KeyA = Just $ keyCommand k KeyD []
carpalxTranslationLayer k@KeyS = Just $ keyCommand k KeyS []
carpalxTranslationLayer k@KeyD = Just $ keyCommand k KeyT []
carpalxTranslationLayer k@KeyF = Just $ keyCommand k KeyN []
carpalxTranslationLayer k@KeyG = Just $ keyCommand k KeyR []
carpalxTranslationLayer k@KeyH = Just $ keyCommand k KeyI []
carpalxTranslationLayer k@KeyJ = Just $ keyCommand k KeyA []
carpalxTranslationLayer k@KeyK = Just $ keyCommand k KeyE []
carpalxTranslationLayer k@KeyL = Just $ keyCommand k KeyO []
carpalxTranslationLayer k@KeySemicolon = Just $ keyCommand k KeyH []
carpalxTranslationLayer k@KeyApostrophe = Just $ keyCommand k KeyApostrophe []
carpalxTranslationLayer k@KeyEnter = Just $ keyCommand k KeyEnter []

-- lsft z    x    c    v    b    n    m    ,    .    /    rsft
-- ->
-- lsft z    x    c    v    j    k    p    ,    .    /    rsft
carpalxTranslationLayer k@KeyLeftShift = Just $ keyMod k [ModShift Press]
carpalxTranslationLayer k@KeyZ = Just $ keyCommand k KeyZ []
carpalxTranslationLayer k@KeyX = Just $ keyCommand k KeyX []
carpalxTranslationLayer k@KeyC = Just $ keyCommand k KeyC []
carpalxTranslationLayer k@KeyV = Just $ keyCommand k KeyV []
carpalxTranslationLayer k@KeyB = Just $ keyCommand k KeyJ []
carpalxTranslationLayer k@KeyN = Just $ keyCommand k KeyK []
carpalxTranslationLayer k@KeyM = Just $ keyCommand k KeyP []
carpalxTranslationLayer k@KeyComma = Just $ keyCommand k KeyComma []
carpalxTranslationLayer k@KeyDot = Just $ keyCommand k KeyDot []
carpalxTranslationLayer k@KeySlash = Just $ keyCommand k KeySlash []
carpalxTranslationLayer k@KeyRightShift = Just $ keyMod k [ModShift Press]

--   lctl lmet lalt           spc            ralt rmet cmp  rctl
-- ->
--   _    lmet lalt           spc            ralt rmet _  rctl
carpalxTranslationLayer k@KeyLeftCtrl = Nothing
carpalxTranslationLayer k@KeyLeftMeta = Just $ keyCommand k KeyLeftMeta []
carpalxTranslationLayer k@KeyLeftAlt = Just $ keyMod k [ModAlt Press]
carpalxTranslationLayer k@KeySpace = Just $ keyCommand k KeySpace []
carpalxTranslationLayer k@KeyRightAlt = Just $ keyCommand k KeyRightAlt []
carpalxTranslationLayer k@KeyRightMeta = Just $ keyCommand k KeyRightMeta []
carpalxTranslationLayer k@KeyCompose = Nothing
carpalxTranslationLayer k@KeyRightCtrl = Just $ keyCommand k KeyRightCtrl []

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
