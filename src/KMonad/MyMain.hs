{-# LANGUAGE CPP #-}
module KMonad.MyMain where

import KMonad.Prelude
import KMonad.Prelude.Imports

import KMonad.Args
import KMonad.App.Types
import KMonad.Keyboard
import KMonad.Util
import KMonad.Model

import System.Environment

import KMonad.Keyboard.Keycode
import KMonad.Keyboard.Types
import KMonad.Keyboard.IO
import qualified KMonad.Model.Dispatch as Dp
import qualified KMonad.Model.Hooks    as Hs
import qualified KMonad.Model.Sluice   as Sl
import qualified KMonad.Model.Keymap   as Km
import System.Process

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
loop = do
  -- Go ahead and emit capslock release in the initial tick
  snk' <- (view keySink)
  _ <- sequence $ (emitKey snk') <$> [(KeyEvent Release KeyCapsLock)]

  forever $ do
    -- Do we need to re-grab these every tick?
    src <- view keySource
    ev <- pull' src
    snk <- (view keySink)
    sequence $ (emitKey snk) <$> ev

updateKeymap :: [Layer] -> [MyKeyComplete] -> KeyEvent -> ([MyKeyComplete], [KeyEvent])
updateKeymap l list (KeyEvent s k) =
  let ke = KeyEvent s ((carpalxTranslationLayer l . hostnameTranslationLayer l currHostname) k)
  in update l list (k, ke)
  where
    -- Key release
    -- We need to release the original key because
    update :: [Layer] -> [MyKeyComplete] -> (Keycode, KeyEvent) -> ([MyKeyComplete], [KeyEvent])
    update _ list (kOrig, (KeyEvent Release _)) =
      foldr
        (\a@(MyKeyComplete k' a') (b, evs) ->
          if k' == kOrig
          then (b, (modifiers trueContext a') ++ (release a') ++ evs)
          -- Put back if no match
          else ((a : b), evs))
        ([], [])
        list
      -- construct <$> relevantEntries

      where
        trueContext :: [MyKeyCommand]
        trueContext = result <$> (filter (\(MyKeyComplete k _) -> ((k /= kOrig))) list)

        -- relevantEntries :: [MyKeyCommand]
        -- relevantEntries = filter (\ (MyKeyCommand k' _ _ _) -> k == k') list

        -- construct entr@(MyKeyCommand k _ _ _) =
        --   [((\(MyKeyCommand k' _ _ _) -> (not (k == k'))) <$> l,
        --     release entr)]

        modifiers c entr = modifierSet c (Release, entr)

    -- Key press
    update layer list (kOrig, (KeyEvent Press k)) =
      -- Tr.trace ("New entry: " ++ (show newEntry)) $
      foldr (\new@(MyKeyComplete _ new') (old, cmd) -> (new : old, ((modifiers (result <$> old) new') ++ (activation new') ++ cmd))) (list, []) newEntries
        where
          newEntries :: [MyKeyComplete]
          newEntries = (MyKeyComplete kOrig) <$> (translationLayer currHostname layer list (concat ((mods . result) <$> list)) kOrig k)
          modifiers c new = modifierSet c (Press, new)

keyMap :: MVar [MyKeyComplete]
keyMap = System.IO.Unsafe.unsafePerformIO $ newMVar []
{-# NOINLINE keyMap #-}

-- TODO:
-- Alt needs to engage shift whenever relevant
-- Reduce excess keys being fired by having the modifier functions check what state the modifier key is in. This is an issue since the normal output function will also send out modifier key presses, which might mess this up?

parseLayer :: ServerCmd -> Maybe Layer
parseLayer (ServerLayer "emacs") = Just $ Emacs
parseLayer (ServerLayer "EXWM") = Just $ EXWM
parseLayer (ServerLayer "EXWMFirefox") = Just $ EXWMFirefox
parseLayer (ServerLayer "raw") = Just $ Raw
parseLayer (ServerLayer "steno") = Just $ Steno
parseLayer _ = Nothing

-- If I recieve a release key command, I need to make sure it's sent to be released as output.
fn :: KeyEvent -> IO [KeyEvent]
fn ev = do
  -- "recieved"

  -- () <- Tr.trace ("Injecting event: " ++ show ke) (pure ())
  cmd <- runServerPull
  m <- takeMVar keyMap

  -- () <- Tr.trace ("Current keymap: " ++ show m) (pure ())
  -- let (m', curr, outKeys) = updateKeymap' m ke
  let (m', outKeys) = updateKeymap cmd m ev
  -- () <- Tr.trace ("Current key: " ++ show curr) (pure ())
  -- () <- Tr.trace ("Updated keymap: " ++ show m') (pure ())
  _ <- putMVar keyMap m'

  -- () <- Tr.trace ("Outputting events: " ++ show (mod ++ outKeys)) (pure ())
  pure $ outKeys

  where
    maybeToList = maybe [] (\a -> [a])
    undoKey k = (KeyEvent Release k)
    doKey k = (KeyEvent Press k)

mapKey f (KeyEvent p k) = (KeyEvent p (f k))

modifierSet c (Release, curr) =
  -- If the key request is unique, just release it
  (concat (deleteRequirement <$> unique))
    -- If the key request isn't unique, then apply the
    ++ (applyMods <$> nonUnique)
  where
    cMod = concat $ mods <$> c
    unique = filter (\a -> not (any (eqMod a) cMod)) (mods curr)

    nonUnique = filter (\a -> any (eqMod a) (mods curr)) cMod

    deleteRequirement (ModShift Press) = [KeyEvent Release KeyLeftShift]
    deleteRequirement (ModAlt Press) = [KeyEvent Release KeyLeftAlt]
    deleteRequirement (ModCtrl Press) = [KeyEvent Release KeyLeftCtrl]
    deleteRequirement (ModRAlt Press) = [KeyEvent Release KeyRightAlt]
    deleteRequirement _ = []

    -- nonUnique = modDeleteDuplicates $ filter (not . (`elem` cMod)) (mods curr)

modifierSet _ (Press, curr) =
  applyMods <$> mods curr

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
eqMod (ModRAlt _) (ModRAlt _) = True
eqMod _ _ = False


applyMods (ModShift p) = KeyEvent p KeyLeftShift
applyMods (ModAlt p) = KeyEvent p KeyLeftAlt
applyMods (ModCtrl p) = KeyEvent p KeyLeftCtrl
applyMods (ModRAlt p) = KeyEvent p KeyRightAlt

revSwitch Press = Release
revSwitch Release = Press

mapMod f (ModShift p)  = ModShift (f p)
mapMod f (ModAlt p) = ModAlt (f p)
mapMod f (ModCtrl p) = ModCtrl (f p)
mapMod f (ModRAlt p) = ModRAlt (f p)

data MyModifiersRequested = ModShift Switch | ModAlt Switch | ModCtrl Switch | ModRAlt Switch
  deriving (Eq, Show)

data MyKeyComplete = MyKeyComplete
  {
    origKey :: Keycode
    , result :: MyKeyCommand
  }

-- Keycode that's being pressed (input) + way to undo it.
data MyKeyCommand = MyKeyCommand {
  rawKey :: Keycode
  , activation :: [KeyEvent]
  , release :: [KeyEvent]
  , mods :: [MyModifiersRequested]
  }
  deriving (Eq, Show)

data Layer =
  Emacs
  | EXWM
  | Raw
  | Steno
  | EXWMFirefox
  deriving (Eq, Show)

currHostname :: String
currHostname = System.IO.Unsafe.unsafePerformIO (getEnv "HOSTNAME")
-- currHostname = System.IO.Unsafe.unsafePerformIO (readProcess "hostname" [] "")
-- currHostname = "thinkpad-t480"
{-# NOINLINE currHostname #-}

findLayerRawOrSteno Steno = True
findLayerRawOrSteno Raw = True
findLayerRawOrSteno _ = False

findLayerRaw Raw = True
findLayerRaw _ = False

findLayerSteno Steno = True
findLayerSteno _ = False

translationLayer :: String -> [Layer] -> [MyKeyComplete] -> [MyModifiersRequested] -> Keycode -> Keycode -> [MyKeyCommand]
translationLayer hostname layer last mod kOrig k =
  fromMaybe [] $ translationLayer' layer mod kOrig k

  where
    findAlt (ModAlt Press) = True
    findAlt _ = False
    findCtrl (ModCtrl Press) = True
    findCtrl _ = False

    findLayerEXWM EXWM = True
    findLayerEXWM _ = False

    findLayerEmacs Emacs = True
    findLayerEmacs _ = False

    translationLayer' _layer _ _kOrig k@KeyLeftAlt = Just $ list $ keyMod k [ModAlt Press]
    translationLayer' _layer _ _kOrig k@KeyRightShift = Just $ list $ keyMod k [ModShift Press]
    translationLayer' _layer _ _kOrig k@KeyLeftShift = Just $ list $ keyMod k [ModShift Press]
    translationLayer' _layer _ _kOrig k@KeyCapsLock = Just $ list $ keyMod k [ModCtrl Press]
    translationLayer' _layer _ _kOrig k@KeyRightCtrl = Just $ list $ keyMod k [ModCtrl Press]

    -- Notice that this steno layer absorbs any key that's not the exit key
    translationLayer' layer _ kOrig k | any findLayerSteno layer =
      Just $ fromMaybe (list $ keyCommand k k []) (stenoLayer last mod hostname kOrig k)

    -- Shortcircut if in raw
    translationLayer' layer _ _kOrig k | any findLayerRaw layer = Just $ list $ keyCommand k k []

    translationLayer' _layer mod _kOrig k | any findCtrl mod && any findAlt mod && isJust (altCtrlTranslationLayer k) =
      altCtrlTranslationLayer k

    translationLayer' _layer mod _kOrig k | any findAlt mod && isJust (altTranslationLayer k) =
      altTranslationLayer k

    translationLayer' _layer mod _kOrig k | any findCtrl mod && isJust (ctrlTranslationLayer k) =
      ctrlTranslationLayer k

    translationLayer' layer _mod _kOrig k | (any findLayerEmacs layer || any findLayerEXWM layer) && isJust (rootTranslationLayer k) =
      rootTranslationLayer k

    translationLayer' layer mod _kOrig k | any findCtrl mod && (any findLayerEmacs layer || any findLayerEXWM layer) && isJust (rootCtrlTranslationLayer k) =
      rootCtrlTranslationLayer k

    translationLayer' layer mod _kOrig k | any findCtrl mod && any findLayerEXWM layer && isJust (exwmCtrlTranslationLayer k) =
      exwmCtrlTranslationLayer k

    translationLayer' _layer _ _kOrig k = Just $ list $ keyCommand k k []

-- _      @!     @at    @#    @$      @%     @*     @lpar  @rpar  @&     @^     @un    @+     @=
-- _      @1     @2     @3    @4      @5     @6     @7     @8     @9     @0     @-     _
-- _      _      _      _      _      _      _      @å     @ä     @ö     _      _
keyCommand k k' mod =
  MyKeyCommand
    k
    [KeyEvent Press k']
    [KeyEvent Release k']
    mod

keyCommandTap k k' mod =
  MyKeyCommand
    k
    [(KeyEvent Press k'), (KeyEvent Release k')]
    []
    mod

keyMod k mod =
  MyKeyCommand
    k
    []
    []
    mod

list a = [a]

altTranslationLayer :: Keycode -> Maybe [MyKeyCommand]
altTranslationLayer k@KeyQ = Just $ list $ keyCommand k Key1 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyG = Just $ list $ keyCommand k Key2 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyM = Just $ list $ keyCommand k Key3 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyL = Just $ list $ keyCommand k Key4 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyW = Just $ list $ keyCommand k Key5 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyY = Just $ list $ keyCommand k Key8 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyF = Just $ list $ keyCommand k Key9 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyU = Just $ list $ keyCommand k Key0 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyB = Just $ list $ keyCommand k Key7 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeySemicolon = Just $ list $ keyCommand k Key6 [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyLeftBrace = Just $ list $ keyCommand k KeyMinus [(ModShift Press), (ModAlt Release)]
altTranslationLayer k@KeyRightBrace = Just $ list $ keyCommand k KeyEqual [(ModShift Press), (ModAlt Release)]

altTranslationLayer k@KeyD = Just $ list $ keyCommand k Key1 [(ModAlt Release)]
altTranslationLayer k@KeyS = Just $ list $ keyCommand k Key2 [(ModAlt Release)]
altTranslationLayer k@KeyT = Just $ list $ keyCommand k Key3 [(ModAlt Release)]
altTranslationLayer k@KeyN = Just $ list $ keyCommand k Key4 [(ModAlt Release)]
altTranslationLayer k@KeyR = Just $ list $ keyCommand k Key5 [(ModAlt Release)]
altTranslationLayer k@KeyI = Just $ list $ keyCommand k Key6 [(ModAlt Release)]
altTranslationLayer k@KeyA = Just $ list $ keyCommand k Key7 [(ModAlt Release)]
altTranslationLayer k@KeyE = Just $ list $ keyCommand k Key8 [(ModAlt Release)]
altTranslationLayer k@KeyO = Just $ list $ keyCommand k Key9 [(ModAlt Release)]
altTranslationLayer k@KeyH = Just $ list $ keyCommand k Key0 [(ModAlt Release)]
altTranslationLayer k@KeyApostrophe = Just $ list $ keyCommand k KeyMinus [(ModAlt Release)]
altTranslationLayer k@KeyEnter = Just $ list $ keyCommand k KeyEqual [(ModAlt Release)]

-- åäö
-- If you ever have any problems with the changed xcompose binds, try using the default ones by simply adding to your activation a tap. So that the first keyCommand taps the key, and the second holds.
altTranslationLayer k@KeyP = Just $ [(keyCommandTap k KeyA [(ModAlt Release), (ModRAlt Press)]), (keyCommand k KeyA [(ModAlt Release), (ModShift Release), (ModRAlt Press)])]
altTranslationLayer k@KeyComma = Just $ [(keyCommandTap k KeyA [(ModAlt Release), (ModRAlt Press)]), (keyCommand k KeyApostrophe [(ModAlt Release), (ModShift Press), (ModRAlt Press)])]
altTranslationLayer k@KeyDot = Just $ [(keyCommandTap k KeyO [(ModAlt Release), (ModRAlt Press)]), (keyCommand k KeyApostrophe [(ModAlt Release), (ModShift Press), (ModRAlt Press)])]

altTranslationLayer _ = Nothing

altCtrlTranslationLayer :: Keycode -> Maybe [MyKeyCommand]
altCtrlTranslationLayer k@KeyF = Just $ list $ keyCommand k KeyBackspace [(ModAlt Release), (ModCtrl Press)]
altCtrlTranslationLayer k@KeyL = Just $ list $ keyCommand k KeyDelete [(ModAlt Release), (ModCtrl Press)]
altCtrlTranslationLayer _ = Nothing

-- Key pressed without any modifier
rootTranslationLayer :: Keycode -> Maybe [MyKeyCommand]
rootTranslationLayer k@KeyTab = Just $ list $ keyCommand k KeyF12 []
rootTranslationLayer _ = Nothing

rootCtrlTranslationLayer :: Keycode -> Maybe [MyKeyCommand]
rootCtrlTranslationLayer k@KeyT = Just $ list $ keyCommand k KeyTab [(ModCtrl Release)]
rootCtrlTranslationLayer _ = Nothing

exwmCtrlTranslationLayer :: Keycode -> Maybe [MyKeyCommand]
exwmCtrlTranslationLayer _ = Nothing

stenoLayer :: [MyKeyComplete] -> [MyModifiersRequested] -> String -> Keycode -> Keycode -> Maybe [MyKeyCommand]
-- Handle pressing C-e. K here would be E in a Carpalx layout
stenoLayer last _mod "desktop" k@KeyK _ =
  if length last == 1 && (any ((==) KeyCapsLock) (origKey <$> last))
  then Just $ list $ keyCommand k KeyE [(ModCtrl Press)]
  else Nothing
-- For other machines that don't rebind ctrl, if Ctrl is pressed down and you press what would be 'e' in Carpalx, exit state
stenoLayer last mod _ k@KeyK _ =
  -- KeyN is where the ctrl key would be if it hadn't been rebound by the steno map
  if length last == 1 && (any ((==) (ModCtrl Press)) mod)
  then Just $ list $ keyCommand k KeyE [(ModCtrl Press)]
  else Nothing
stenoLayer _ _ _ _ _ = Nothing

-- caps      _      _      _      _      _      _      _      _      _      _      _      _      _
--  _      _      _      _      @del   _      _      @bspc  _      _      _      _      _      _
--  _      _      _      _      _      _      _      @ret   _      _      _      _      _
--  _      _      _      _      _      _      _      _      _      _      _      _
--  _   _      _             _                           _      _      _      _
ctrlTranslationLayer :: Keycode -> Maybe [MyKeyCommand]
ctrlTranslationLayer k@KeyGrave = Just $ list $ keyCommand k KeyCapsLock [(ModCtrl Release)]
ctrlTranslationLayer k@KeyL = Just $ list $ keyCommand k KeyDelete [(ModCtrl Release)]
ctrlTranslationLayer k@KeyF = Just $ list $ keyCommand k KeyBackspace [(ModCtrl Release)]
ctrlTranslationLayer k@KeyA = Just $ list $ keyCommand k KeyEnter [(ModCtrl Release)]
-- Might as well handle this here as C-a is rebound
ctrlTranslationLayer k@KeyM = Just $ list $ keyCommand k KeyA [(ModCtrl Press)]
ctrlTranslationLayer _ = Nothing

hostnameTranslationLayer :: [Layer] -> String -> Keycode -> Keycode
hostnameTranslationLayer _ "thinkpad-t480" KeyBackslash = KeyEnter
hostnameTranslationLayer _ "thinkpad-t480" KeyEnter = KeyBackslash
-- Steno on ergodox
hostnameTranslationLayer l "desktop" KeyLeftShift | any findLayerSteno l = KeyC
hostnameTranslationLayer l "desktop" KeySpace | any findLayerSteno l = KeyV
hostnameTranslationLayer l "desktop" KeyCapsLock | any findLayerSteno l = KeyN
hostnameTranslationLayer l "desktop" KeyLeftAlt | any findLayerSteno l = KeyM
hostnameTranslationLayer _ _ a = a

-- QWERTY -> Carpalx
carpalxTranslationLayer :: [Layer] -> Keycode -> Keycode
carpalxTranslationLayer l k | any findLayerRawOrSteno l = k
carpalxTranslationLayer _ KeyEsc = KeyEsc
carpalxTranslationLayer _ KeyHome = KeyHome
carpalxTranslationLayer _ KeyEnd = KeyEnd
carpalxTranslationLayer _ KeyInsert = KeyInsert
carpalxTranslationLayer _ KeyF1 = KeyF1
carpalxTranslationLayer _ KeyF2 = KeyF2
carpalxTranslationLayer _ KeyF3 = KeyF3
carpalxTranslationLayer _ KeyF4 = KeyF4
carpalxTranslationLayer _ KeyF5 = KeyF5
carpalxTranslationLayer _ KeyF6 = KeyF6
carpalxTranslationLayer _ KeyF7 = KeyF7
carpalxTranslationLayer _ KeyF8 = KeyF8
carpalxTranslationLayer _ KeyF9 = KeyF9
carpalxTranslationLayer _ KeyF10 = KeyF10

-- grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
-- ->
-- grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
carpalxTranslationLayer _ KeyGrave = KeyGrave
carpalxTranslationLayer _ Key1 = Key1
carpalxTranslationLayer _ Key2 = Key2
carpalxTranslationLayer _ Key3 = Key3
carpalxTranslationLayer _ Key4 = Key4
carpalxTranslationLayer _ Key5 = Key5
carpalxTranslationLayer _ Key6 = Key6
carpalxTranslationLayer _ Key7 = Key7
carpalxTranslationLayer _ Key8 = Key8
carpalxTranslationLayer _ Key9 = Key9
carpalxTranslationLayer _ Key0 = Key0
carpalxTranslationLayer _ KeyMinus = KeyMinus
carpalxTranslationLayer _ KeyEqual = KeyEqual
carpalxTranslationLayer _ KeyBackspace = KeyGrave -- Ignore key

-- Top row
-- tab  q    w    e    r    t    y    u    i    o    p    [    ]    \
-- ->
-- tab  q    g    m    l    w    y    f    u    b    ;    [    ]    \
carpalxTranslationLayer _ KeyQ = KeyQ
carpalxTranslationLayer _ KeyW = KeyG
carpalxTranslationLayer _ KeyE = KeyM
carpalxTranslationLayer _ KeyR = KeyL
carpalxTranslationLayer _ KeyT = KeyW
carpalxTranslationLayer _ KeyY = KeyY
carpalxTranslationLayer _ KeyU = KeyF
carpalxTranslationLayer _ KeyI = KeyU
carpalxTranslationLayer _ KeyO = KeyB
carpalxTranslationLayer _ KeyP = KeySemicolon
carpalxTranslationLayer _ KeyLeftBrace = KeyLeftBrace
carpalxTranslationLayer _ KeyRightBrace = KeyRightBrace
carpalxTranslationLayer _ KeyBackslash = KeyBackslash

-- caps a    s    d    f    g    h    j    k    l    ;    '    ret
-- ->
-- caps d    s    t    n    r    i    a    e    o    h    '    ret
carpalxTranslationLayer _ KeyA = KeyD
carpalxTranslationLayer _ KeyS = KeyS
carpalxTranslationLayer _ KeyD = KeyT
carpalxTranslationLayer _ KeyF = KeyN
carpalxTranslationLayer _ KeyG = KeyR
carpalxTranslationLayer _ KeyH = KeyI
carpalxTranslationLayer _ KeyJ = KeyA
carpalxTranslationLayer _ KeyK = KeyE
carpalxTranslationLayer _ KeyL = KeyO
carpalxTranslationLayer _ KeySemicolon = KeyH
carpalxTranslationLayer _ KeyApostrophe = KeyApostrophe
carpalxTranslationLayer _ KeyEnter = KeyEnter

-- lsft z    x    c    v    b    n    m    ,    .    /    rsft
-- ->
-- lsft z    x    c    v    j    k    p    ,    .    /    rsft
carpalxTranslationLayer _ KeyZ = KeyZ
carpalxTranslationLayer _ KeyX = KeyX
carpalxTranslationLayer _ KeyC = KeyC
carpalxTranslationLayer _ KeyV = KeyV
carpalxTranslationLayer _ KeyB = KeyJ
carpalxTranslationLayer _ KeyN = KeyK
carpalxTranslationLayer _ KeyM = KeyP
carpalxTranslationLayer _ KeyComma = KeyComma
carpalxTranslationLayer _ KeyDot = KeyDot
carpalxTranslationLayer _ KeySlash = KeySlash

--   lctl lmet lalt           spc            ralt rmet cmp  rctl
-- ->
--   _    lmet lalt           spc            ralt rmet _  rctl
-- carpalxTranslationLayer KeyLeftCtrl = Nothing
-- carpalxTranslationLayer KeyLeftMeta = KeyLeftMeta
-- carpalxTranslationLayer KeySpace = KeySpace
-- carpalxTranslationLayer KeyRightAlt = KeyRightAlt
-- carpalxTranslationLayer KeyRightMeta = KeyRightMeta
-- carpalxTranslationLayer k@KeyCompose = Nothing

carpalxTranslationLayer _ k = k

lastServerResponse :: MVar [Layer]
lastServerResponse = System.IO.Unsafe.unsafePerformIO $ newMVar [Emacs]
{-# NOINLINE lastServerResponse #-}

runServerPull :: IO [Layer]
runServerPull = do
  last <- takeMVar lastServerResponse
  resp <- tryReadMVar serverMVar
  let
    resp' :: Maybe [Layer]
    resp' = catMaybes <$> ((fmap . fmap) parseLayer resp)
    out' = fromMaybe last resp'
  putMVar lastServerResponse out'
  pure out'

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
