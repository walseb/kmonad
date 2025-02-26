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
  (fromMaybe list $ (\a -> (filter ((==) a) list)) <$> relevantEntry,
    (\a -> (Release, a)) <$> relevantEntry,
    fromMaybe [] $ release <$> relevantEntry)

  where
    relevantEntry :: Maybe MyKeyCommand
    relevantEntry = find (\ (MyKeyCommand k' _ _ _) -> k == k') list

-- Key press
updateKeymap list (KeyEvent Press k) =
  fromMaybe (list, Nothing, []) $ (\new -> (new : list, Just (Press, new), activation new)) <$> newEntry
    where newEntry = translationLayer (concat (mods <$> list)) k

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
  let (m', curr, outKeys) = updateKeymap m ke
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
    unique = filter (not . (`elem` cMod)) (mods curr)

    deleteRequirement (ModShift Press) = [KeyEvent Release KeyLeftShift]
    deleteRequirement (ModAlt Press) = [KeyEvent Release KeyLeftAlt]
    deleteRequirement (ModCtrl Press) = [KeyEvent Release KeyLeftCtrl]
    deleteRequirement _ = []

    nonUnique = modDeleteDuplicates $ filter (`elem` (mods curr)) cMod 

modifierSet _ (Just (Press, curr)) =
  applyMods <$> mods curr

modDeleteDuplicates c = foldr
                (\a b ->
                  if any (eqConstructor a) b
                  then b
                  else (a : b))
                []
                c
  where
    eqConstructor (ModShift _) (ModShift _) = True
    eqConstructor (ModAlt _) (ModAlt _) = True
    eqConstructor (ModCtrl _) (ModCtrl _) = True
    eqConstructor _ _ = False


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

keyCommand k mod =
  MyKeyCommand
    k
    [KeyEvent Press k]
    [KeyEvent Release k]
    mod

keyMod k mod =
  MyKeyCommand
    k
    []
    []
    mod

altTranslationLayer :: Keycode -> Maybe MyKeyCommand
altTranslationLayer KeyQ = Just $ keyCommand Key1 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyW = Just $ keyCommand Key2 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyE = Just $ keyCommand Key3 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyR = Just $ keyCommand Key4 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyT = Just $ keyCommand Key5 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyY = Just $ keyCommand Key6 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyU = Just $ keyCommand Key7 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyI = Just $ keyCommand Key8 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyO = Just $ keyCommand Key9 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyP = Just $ keyCommand Key0 [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyLeftBrace = Just $ keyCommand KeyMinus [(ModShift Press), (ModAlt Release)]
altTranslationLayer KeyRightBrace = Just $ keyCommand KeyEqual [(ModShift Press), (ModAlt Release)]

altTranslationLayer KeyA = Just $ keyCommand Key1 [(ModAlt Release)]
altTranslationLayer KeyS = Just $ keyCommand Key2 [(ModAlt Release)]
altTranslationLayer KeyD = Just $ keyCommand Key3 [(ModAlt Release)]
altTranslationLayer KeyF = Just $ keyCommand Key4 [(ModAlt Release)]
altTranslationLayer KeyG = Just $ keyCommand Key5 [(ModAlt Release)]
altTranslationLayer KeyH = Just $ keyCommand Key6 [(ModAlt Release)]
altTranslationLayer KeyJ = Just $ keyCommand Key7 [(ModAlt Release)]
altTranslationLayer KeyK = Just $ keyCommand Key8 [(ModAlt Release)]
altTranslationLayer KeyL = Just $ keyCommand Key9 [(ModAlt Release)]
altTranslationLayer KeySemicolon = Just $ keyCommand Key0 [(ModAlt Release)]
altTranslationLayer KeyApostrophe = Just $ keyCommand KeyMinus [(ModAlt Release)]
altTranslationLayer KeyEnter = Just $ keyCommand KeyEqual [(ModAlt Release)]

-- åäö
altTranslationLayer KeyM = Just $ keyCommand KeyEqual [(ModAlt Release)]
altTranslationLayer KeyComma = Just $ keyCommand KeyEqual [(ModAlt Release)]
altTranslationLayer KeyDot = Just $ keyCommand KeyEqual [(ModAlt Release)]

altTranslationLayer _ = Nothing

-- caps      _      _      _      _      _      _      _      _      _      _      _      _      _
--  _      _      _      _      @del   _      _      @bspc  _      _      _      _      _      _
--  _      _      _      _      _      _      _      @ret   _      _      _      _      _
--  _      _      _      _      _      _      _      _      _      _      _      _
--  _   _      _             _                           _      _      _      _
ctrlTranslationLayer :: Keycode -> Maybe MyKeyCommand
ctrlTranslationLayer KeyEsc = Just $ keyCommand KeyCapsLock [(ModCtrl Release)]
ctrlTranslationLayer KeyL = Just $ keyCommand KeyDelete [(ModCtrl Release)]
ctrlTranslationLayer KeyF = Just $ keyCommand KeyBackspace [(ModCtrl Release)]
ctrlTranslationLayer KeyA = Just $ keyCommand KeyEnter [(ModCtrl Release)]
ctrlTranslationLayer _ = Nothing



-- QWERTY -> Carpalx

-- grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
-- ->
-- grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
carpalxTranslationLayer :: Keycode -> Maybe MyKeyCommand
carpalxTranslationLayer KeyGrave = Just $ keyCommand KeyGrave []
carpalxTranslationLayer Key1 = Just $ keyCommand Key1 []
carpalxTranslationLayer Key2 = Just $ keyCommand Key2 []
carpalxTranslationLayer Key3 = Just $ keyCommand Key3 []
carpalxTranslationLayer Key4 = Just $ keyCommand Key4 []
carpalxTranslationLayer Key5 = Just $ keyCommand Key5 []
carpalxTranslationLayer Key6 = Just $ keyCommand Key6 []
carpalxTranslationLayer Key7 = Just $ keyCommand Key7 []
carpalxTranslationLayer Key8 = Just $ keyCommand Key8 []
carpalxTranslationLayer Key9 = Just $ keyCommand Key9 []
carpalxTranslationLayer Key0 = Just $ keyCommand Key0 []
carpalxTranslationLayer KeyMinus = Just $ keyCommand KeyMinus []
carpalxTranslationLayer KeyEqual = Just $ keyCommand KeyEqual []
carpalxTranslationLayer KeyBackspace = Just $ keyCommand KeyBackspace []


-- Top row
-- tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
-- ->
-- tab  q    g    m    l    w    y    f    u    b    ;    [    ]    \
carpalxTranslationLayer KeyTab = Just $ keyCommand KeyTab []
carpalxTranslationLayer KeyQ = Just $ keyCommand KeyQ []
carpalxTranslationLayer KeyW = Just $ keyCommand KeyG []
carpalxTranslationLayer KeyE = Just $ keyCommand KeyM []
carpalxTranslationLayer KeyR = Just $ keyCommand KeyL []
carpalxTranslationLayer KeyT = Just $ keyCommand KeyW []
carpalxTranslationLayer KeyY = Just $ keyCommand KeyY []
carpalxTranslationLayer KeyU = Just $ keyCommand KeyF []
carpalxTranslationLayer KeyI = Just $ keyCommand KeyU []
carpalxTranslationLayer KeyO = Just $ keyCommand KeyB []
carpalxTranslationLayer KeyP = Just $ keyCommand KeySemicolon []
carpalxTranslationLayer KeyLeftBrace = Just $ keyCommand KeyLeftBrace []
carpalxTranslationLayer KeyRightBrace = Just $ keyCommand KeyRightBrace []
carpalxTranslationLayer KeyBackslash = Just $ keyCommand KeyBackslash []

-- caps a    s    d    f    g    h    j    k    l    ;    '    ret
-- ->
-- caps d    s    t    n    r    i    a    e    o    h    '    ret
carpalxTranslationLayer KeyCapsLock = Just $ keyMod KeyLeftCtrl [ModCtrl Press]
carpalxTranslationLayer KeyA = Just $ keyCommand KeyD []
carpalxTranslationLayer KeyS = Just $ keyCommand KeyS []
carpalxTranslationLayer KeyD = Just $ keyCommand KeyT []
carpalxTranslationLayer KeyF = Just $ keyCommand KeyN []
carpalxTranslationLayer KeyG = Just $ keyCommand KeyR []
carpalxTranslationLayer KeyH = Just $ keyCommand KeyI []
carpalxTranslationLayer KeyJ = Just $ keyCommand KeyA []
carpalxTranslationLayer KeyK = Just $ keyCommand KeyE []
carpalxTranslationLayer KeyL = Just $ keyCommand KeyO []
carpalxTranslationLayer KeySemicolon = Just $ keyCommand KeyH []
carpalxTranslationLayer KeyApostrophe = Just $ keyCommand KeyApostrophe []
carpalxTranslationLayer KeyEnter = Just $ keyCommand KeyEnter []

-- lsft z    x    c    v    b    n    m    ,    .    /    rsft
-- ->
-- lsft z    x    c    v    j    k    p    ,    .    /    rsft
carpalxTranslationLayer KeyLeftShift = Just $ keyMod KeyLeftShift [ModShift Press]
carpalxTranslationLayer KeyZ = Just $ keyCommand KeyZ []
carpalxTranslationLayer KeyX = Just $ keyCommand KeyX []
carpalxTranslationLayer KeyC = Just $ keyCommand KeyC []
carpalxTranslationLayer KeyV = Just $ keyCommand KeyV []
carpalxTranslationLayer KeyB = Just $ keyCommand KeyJ []
carpalxTranslationLayer KeyN = Just $ keyCommand KeyK []
carpalxTranslationLayer KeyM = Just $ keyCommand KeyP []
carpalxTranslationLayer KeyComma = Just $ keyCommand KeyComma []
carpalxTranslationLayer KeyDot = Just $ keyCommand KeyDot []
carpalxTranslationLayer KeySlash = Just $ keyCommand KeySlash []
carpalxTranslationLayer KeyRightShift = Just $ keyMod KeyRightShift [ModShift Press]

--   lctl lmet lalt           spc            ralt rmet cmp  rctl
-- ->
--   _    lmet lalt           spc            ralt rmet _  rctl
carpalxTranslationLayer KeyLeftCtrl = Nothing
carpalxTranslationLayer KeyLeftMeta = Just $ keyCommand KeyLeftMeta []
carpalxTranslationLayer KeyLeftAlt = Just $ keyMod KeyLeftAlt [ModAlt Press]
carpalxTranslationLayer KeySpace = Just $ keyCommand KeySpace []
carpalxTranslationLayer KeyRightAlt = Just $ keyCommand KeyRightAlt []
carpalxTranslationLayer KeyRightMeta = Just $ keyCommand KeyRightMeta []
carpalxTranslationLayer KeyCompose = Nothing
carpalxTranslationLayer KeyRightCtrl = Just $ keyCommand KeyRightCtrl []

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
