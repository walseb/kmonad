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

import GHC.IO.Exception (ExitCode (ExitFailure))
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
import Data.Maybe (maybeToList)


import qualified Control.Concurrent
import qualified Control.Exception


-- FIXME: This should live somewhere else

#ifdef linux_HOST_OS
import qualified System.Posix.Signals as Sig

#endif


headSafe :: [a] -> Maybe a
headSafe []     = Nothing
headSafe (a:_) = Just a

initAppEnv :: HasLogFunc e => AppCfg -> ContT r (RIO e) AppEnv
initAppEnv cfg = do
  -- Get a reference to the logging function
  lgf <- view logFuncL

  -- Wait a bit for the user to release the 'Return' key with which they started KMonad
  threadDelay $ fromIntegral (cfg^.startDelay) * 1000

  -- Acquire the key source and key sink
  snk <- using $ cfg^.keySinkDev
  src <- using $ cfg^.keySourceDev

  id <- liftIO $ Control.Concurrent.forkIO (launchServer serverMVar)

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
-- #ifdef linux_HOST_OS
--   -- Ignore SIGCHLD to avoid zombie processes.
--   liftIO . void $ Sig.installHandler Sig.sigCHLD Sig.Ignore Nothing
-- #endif
  runContT (initAppEnv c) (`runRIO` loop)

loop :: RIO AppEnv ()
loop = do
  -- Go ahead and emit capslock release in the initial tick
  -- snk' <- (view keySink)
  -- _ <- sequence $ (emitKey snk') <$> [(KeyEvent Release KeyCapsLock)]

  forever $ do
    -- Do we need to re-grab these every tick?
    src <- view keySource
    ev <- pull' src
    snk <- (view keySink)
    sequence $ (emitKey snk) <$> ev

updateKeymap :: [Layer] -> [(RootKeycode, RootInput)] -> KeyEvent -> ([(RootKeycode, RootInput)], [KeyEvent])
updateKeymap l list (KeyEvent s k) =
  let (KeyEvent _ k') = KeyEvent s ((carpalxTranslationLayer l) k)
      ke' = KeyEvent s ((hostnameTranslationLayer l currHostname k) k')
  in update l list (k, ke')
  where
    -- Key release
    -- We need to release the original key because
    update :: [Layer] -> [(RootKeycode, RootInput)] -> (Keycode, KeyEvent) -> ([(RootKeycode, RootInput)], [KeyEvent])
    update _ list (kOrig, (KeyEvent Release _)) =
      (noCurrent, effects)

      where
        (current, noCurrent) = mapSplit (\(a, _) -> a == kOrig) list
        effects = concat $ (\a@(_, a') ->
                                  -- Since this is the oldest key, run its actions first
                                  -- First release key
                                  (fromMaybe [] (maybeGetRelease a'))
                                  -- Then release modifiers
                                  ++ modifierSet (snd <$> noCurrent) oldModState (Release, snd a) Nothing)
                           <$> current

        oldModState = concat $ modifier <$> (listOnlyMods noCurrent)

        mapSplit f as = foldr (\b (bs, bs') -> if f b then (b:bs, bs') else (bs, b:bs')) ([], []) as

        maybeGetRelease (MyKeyCommand (KeyCommand _ _ rel _)) = Just rel
        maybeGetRelease _ = Nothing

        listOnlyMods l = catMaybes $ removeKeys <$> l
          where
            removeKeys (_, (MyModifier a)) = Just a
            removeKeys (_, (MyKeyCommand _)) = Nothing

    -- Key press
    update layer list (kOrig, (KeyEvent Press k)) =
      -- Tr.trace ("New entry: " ++ (show newEntry)) $
      let (news, ev) =
            -- This fold is very tricky. If you make any changes, try 'åäö' keys.
            foldr (\new@(_, new') (old, cmd) ->
              (new : old,
                -- Tr.trace ("=============\nKey list length: " ++ (show (length newEntries)) ++ "\nKey list contents:" ++ (show newEntries) ++ "\nThis was last key: " ++ (show (headSafe (oldList' old))) ++ "\nKey pressed: " ++ (show new') ++ "\n=============")
                cmd
                -- First press the modifiers
                ++ (modifierSet (snd <$> (oldList' old)) (concat (modifier <$> listOnlyMods (oldList' old))) (Press, new') (snd <$> (headSafe (oldList' old)))) -- The issue stems from this.

                -- Then press the key, if it's a key
                ++ (concat (maybeToList (activation <$> (removeMod new'))))

                ))
              ([], [])
              (reverse newEntries)
        in ((reverse news) ++ list, ev)
        where
          oldList' n = n ++ list
          newEntries :: [(RootKeycode, RootInput)]
          newEntries = ((,) kOrig) <$> (translationLayer currHostname layer list (concat (modifier <$> (listOnlyMods list))) kOrig k)

          listOnlyMods l = catMaybes $ removeKeys <$> l
            where
              removeKeys (_, (MyModifier a)) = Just a
              removeKeys (_, (MyKeyCommand _)) = Nothing

          removeMod (MyKeyCommand a) = Just a
          removeMod (MyModifier _) = Nothing

keyMap :: MVar [(RootKeycode, RootInput)]
keyMap = System.IO.Unsafe.unsafePerformIO $ newMVar []
{-# NOINLINE keyMap #-}

parseLayer :: ServerCmd -> Maybe Layer
parseLayer (ServerLayer "PlainModifiers") = Just $ PlainModifiers
parseLayer (ServerLayer "RTS") = Just $ RTS
parseLayer (ServerLayer "FPS") = Just $ FPS
parseLayer (ServerLayer "emacs") = Just $ Emacs
parseLayer (ServerLayer "EXWM") = Just $ EXWM
parseLayer (ServerLayer "EXWMFirefox") = Just $ EXWMFirefox
parseLayer (ServerLayer "raw") = Just $ Raw
parseLayer (ServerLayer "steno") = Just $ Steno
parseLayer _ = Nothing

-- If I recieve a release key command, I need to make sure it's sent to be released as output.
fn :: KeyEvent -> IO [KeyEvent]
fn ev = do
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

mapKey f (KeyEvent p k) = (KeyEvent p (f k))

-- The non-global mods need to be thrown out as soon as a new key not requiring them is pressed
modifierSet :: [RootInput] -> [MyModifiersRequested] -> (Switch, RootInput) -> Maybe RootInput -> [KeyEvent]

modifierSet _ oldGlobalMods (Press, (MyModifier (Modifier _ mods))) (Just (MyKeyCommand (KeyCommand _ _ _ mods'))) =
  fromTargetGivenContext (mergeMods mods oldGlobalMods) (mergeMods mods' oldGlobalMods)

modifierSet _ oldGlobalMods (Press, (MyModifier (Modifier _ mods))) _ =
  fromTargetGivenContext (mergeMods mods oldGlobalMods) oldGlobalMods

modifierSet _ oldGlobalMods (Press, (MyKeyCommand (KeyCommand _ _ _ mods))) (Just (MyKeyCommand (KeyCommand _ _ _ mods'))) =
  -- Tr.trace ("\nMod key target: " ++ (show (mergeMods mods oldGlobalMods)) ++ "\nMod key source: " ++ show (mergeMods mods' oldGlobalMods) ++ "\nMod key tally: " ++ (show (fromTargetGivenContext (mergeMods mods oldGlobalMods) (mergeMods mods' oldGlobalMods))) ++ "\n") $
    fromTargetGivenContext (mergeMods mods oldGlobalMods) (mergeMods mods' oldGlobalMods)

modifierSet _ oldGlobalMods (Press, (MyKeyCommand (KeyCommand _ _ _ mods))) _ =
  -- Tr.trace ("\nMod gen target: " ++ (show (mergeMods mods oldGlobalMods)) ++ "\nMod gen source: " ++ (show oldGlobalMods) ++ "Mod gen tally: " ++ (show (fromTargetGivenContext (mergeMods mods oldGlobalMods) oldGlobalMods)) ++ "\n") $
    fromTargetGivenContext (mergeMods mods oldGlobalMods) oldGlobalMods

-- TODO: Account for last key and resume its context
modifierSet c _ (Release, (MyModifier (Modifier _ m))) _ =
  -- If a mod is released, we only need to change the modifiers being held if the mod released isn't required anywhere in the context.
  -- If it is required for any reason anywhere in the (key, not mod) context, then simply do nothing. Those other keys will release the mod eventually
  join $ applyMods <$> isFoundAmongContext
  where
    isFoundAmongContext = if isJust (find (\a -> elem a m) (concat (mods <$> (findRootKey c))))
      then []
      else catMaybes (disableMod <$> m)

modifierSet c oldGlobalMods (Release, key@(MyKeyCommand (KeyCommand _ _ _ mods))) _ =
  fromTargetGivenContext
    (mergeMods (lToRootKey cNoKey) oldGlobalMods)
    (mergeMods mods oldGlobalMods)
  where
    cNoKey = filter ((/=) key) c

lToRootKey c = (fromMaybe [] (headSafe (mods <$> (findRootKey c))))

findRootKey as = catMaybes $ rootToKey <$> as

rootToKey (MyKeyCommand a) = Just a
rootToKey (MyModifier _) = Nothing



mergeMods :: [MyModifiersRequested] -> [MyModifiersRequested] -> [MyModifiersRequested]
mergeMods major minor =
  modDeleteDuplicates $ minor ++ major

  where
    modDeleteDuplicates c = foldr
                    (\a b ->
                      if any (eqModAbstract a) b
                      then b
                      else (a : b))
                    []
                    c

-- Finds and removes orphaned mods
-- Apply only the keys in targetMods that don't exist EXACTLYL in oldMods
fromTargetGivenContext :: [MyModifiersRequested] -> [MyModifiersRequested] -> [KeyEvent]
fromTargetGivenContext targetMods oldMods =
  -- Handles changes in same symbols
  -- [M Release]
  -- [M Press]
  (join (applyMods <$> (inBothAndConflicting targetMods oldMods)))
    -- Handles releases of removed keys
    -- []
    -- [M Press]
    -- Release keys that don't exist in targetMods
    ++ (join (applyMods <$> (catMaybes (disableMod <$> orphanedKeys))))
    -- Handles presses of new keys
    -- [M Press]
    -- []
    ++ (join (applyMods <$> (filter onlyIfPress newKeys)))
    -- ++ (applyMods <$> modDeleteAbsDuplicatesFrom targetMods oldMods)
    where
      newKeys = modNotIn targetMods oldMods
      orphanedKeys = modNotIn oldMods targetMods

      inBothAndConflicting c source = filter (\a -> (any (isJust . (conflictingModSwitch a)) source)) c

      -- If you have two modifiers, what needs to be changed?
      -- The first mod has presedence.
      conflictingModSwitch k k' | eqModAbstract k k' && (getSwitch k) /= (getSwitch k') = Just k
      conflictingModSwitch _ _ = Nothing

      modNotIn c source = filter (\a -> not (any (eqModAbstract a) source)) c

onlyIfPress a = mapModSwitch ((==) Press) a

disableMod (ModShift Press) = Just (ModShift Release)
disableMod (ModAlt Press) = Just (ModAlt Release)
disableMod (ModCtrl Press) = Just (ModCtrl Release)
disableMod (ModRAlt Press) = Just (ModRAlt Release)
disableMod (ModFKeys Press) = Just (ModFKeys Release)

disableMod (ModShift Release) = Nothing
disableMod (ModAlt Release) = Nothing
disableMod (ModCtrl Release) = Nothing
disableMod (ModRAlt Release) = Nothing
disableMod (ModFKeys Release) = Nothing

eqModAbstract (ModShift _) (ModShift _) = True
eqModAbstract (ModAlt _) (ModAlt _) = True
eqModAbstract (ModCtrl _) (ModCtrl _) = True
eqModAbstract (ModRAlt _) (ModRAlt _) = True
eqModAbstract (ModFKeys _) (ModFKeys _) = True
eqModAbstract _ _ = False

applyMods (ModShift p) = [KeyEvent p KeyLeftShift]
applyMods (ModAlt p) = [KeyEvent p KeyLeftAlt]
applyMods (ModCtrl p) = [KeyEvent p KeyLeftCtrl]
applyMods (ModRAlt p) = [KeyEvent p KeyRightAlt]
applyMods (ModFKeys p) = []

revSwitch Press = Release
revSwitch Release = Press

getSwitch :: MyModifiersRequested -> Switch
getSwitch m = mapModSwitch id m

mapMod f (ModShift p)  = ModShift (f p)
mapMod f (ModAlt p) = ModAlt (f p)
mapMod f (ModCtrl p) = ModCtrl (f p)
mapMod f (ModRAlt p) = ModRAlt (f p)
mapMod f (ModFKeys p) = ModFKeys (f p)

mapModSwitch f (ModShift p) = (f p)
mapModSwitch f (ModAlt p) = (f p)
mapModSwitch f (ModCtrl p) = (f p)
mapModSwitch f (ModRAlt p) = (f p)
mapModSwitch f (ModFKeys p) = (f p)

data MyModifiersRequested = ModShift Switch | ModAlt Switch | ModCtrl Switch | ModRAlt Switch | ModFKeys Switch
  deriving (Eq, Show)

type RootKeycode = Keycode

-- Keycode that's being pressed (input) + way to undo it.
data RootInput = MyKeyCommand KeyCommand | MyModifier Modifier
  deriving (Eq, Show)

data KeyCommand = KeyCommand {
  rawKey :: Keycode
  , activation :: [KeyEvent]
  , release :: [KeyEvent]
  , mods :: [MyModifiersRequested]
  }
  deriving (Eq, Show)

data Modifier = Modifier {
  modRawKey :: Keycode
  , modifier :: [MyModifiersRequested]
  }
  deriving (Eq, Show)

data Layer =
  Emacs
  | EXWM
  | Raw
  | Steno
  | EXWMFirefox
  | RTS
  | FPS
  | PlainModifiers
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

translationLayer :: String -> [Layer] -> [(RootKeycode, RootInput)] -> [MyModifiersRequested] -> Keycode -> Keycode -> [RootInput]
translationLayer hostname layer last mod kOrig k =
  fromMaybe [] $ translationLayer' layer mod kOrig k

  where
    findModFKeys (ModFKeys Press) = True
    findModFKeys _ = False

    findAlt (ModAlt Press) = True
    findAlt _ = False
    findCtrl (ModCtrl Press) = True
    findCtrl _ = False

    findLayerEXWM EXWM = True
    findLayerEXWM _ = False

    findLayerEmacs Emacs = True
    findLayerEmacs _ = False

    findLayerRts RTS = True
    findLayerRts _ = False

    findLayerFps FPS = True
    findLayerFps _ = False

    findLayerPlainModifiers PlainModifiers = True
    findLayerPlainModifiers _ = False

    translationLayer' _layer _ _kOrig k@KeyLeftAlt = Just $ list $ keyMod k [ModAlt Press]
    translationLayer' _layer _ _kOrig k@KeyRightShift = Just $ list $ keyMod k [ModShift Press]
    translationLayer' _layer _ _kOrig k@KeyLeftShift = Just $ list $ keyMod k [ModShift Press]

    translationLayer' _layer _ _kOrig k@KeyCapsLock = Just $ list $ keyMod k [ModCtrl Press]
    -- For left hand ctrl on split keyboard
    translationLayer' _layer _ _kOrig k@KeyLeftCtrl = Just $ list $ keyMod k [ModCtrl Press]

    translationLayer' _layer _ _kOrig k@KeyRightCtrl = Just $ list $ keyMod k [ModCtrl Press]

    -- Notice that this steno layer absorbs any key that's not the exit key
    translationLayer' layer _ kOrig k | any findLayerSteno layer =
      Just $ fromMaybe (list $ keyCommand k k []) (stenoLayer last mod hostname kOrig k)

    -- Shortcircut if in raw
    translationLayer' layer _ _kOrig k | any findLayerRaw layer = Just $ list $ keyCommand k k []

    translationLayer' _layer mod _kOrig k | any findCtrl mod && any findAlt mod && isJust (altCtrlTranslationLayer k) =
      altCtrlTranslationLayer k

    translationLayer' _layer mod _kOrig k | (not (any findLayerPlainModifiers layer)) && (any findAlt mod && isJust (altTranslationLayer k)) =
      altTranslationLayer k

    translationLayer' _layer mod _kOrig k | any findModFKeys mod && isJust (modFKeysTranslationLayer k) =
      modFKeysTranslationLayer k

    translationLayer' _layer mod _kOrig k | any findCtrl mod && isJust (ctrlTranslationLayer k) =
      ctrlTranslationLayer k

    translationLayer' layer _mod _kOrig k | any findLayerRts layer && isJust (rtsTranslationLayer k) =
      rtsTranslationLayer k

    translationLayer' layer _mod _kOrig k | any findLayerFps layer && isJust (fpsTranslationLayer k) =
      fpsTranslationLayer k

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
  MyKeyCommand $ KeyCommand
    k
    [KeyEvent Press k']
    [KeyEvent Release k']
    mod

keyCommandTap k k' mod =
  MyKeyCommand $ KeyCommand
    k
    [(KeyEvent Press k'), (KeyEvent Release k')]
    []
    mod

keyMod k mod =
  MyModifier $ Modifier k mod

list a = [a]

modFKeysTranslationLayer :: Keycode -> Maybe [RootInput]
modFKeysTranslationLayer k@KeyD = Just $ list $ keyCommand k KeyF1 []
modFKeysTranslationLayer k@KeyS = Just $ list $ keyCommand k KeyF2 []
modFKeysTranslationLayer k@KeyT = Just $ list $ keyCommand k KeyF3 []
modFKeysTranslationLayer k@KeyN = Just $ list $ keyCommand k KeyF4 []
modFKeysTranslationLayer k@KeyR = Just $ list $ keyCommand k KeyF5 []
modFKeysTranslationLayer k@KeyI = Just $ list $ keyCommand k KeyF6 []
modFKeysTranslationLayer k@KeyA = Just $ list $ keyCommand k KeyF7 []
modFKeysTranslationLayer k@KeyE = Just $ list $ keyCommand k KeyF8 []
modFKeysTranslationLayer k@KeyO = Just $ list $ keyCommand k KeyF9 []
modFKeysTranslationLayer k@KeyH = Just $ list $ keyCommand k KeyF10 []
modFKeysTranslationLayer k@KeyApostrophe = Just $ list $ keyCommand k KeyF11 []
modFKeysTranslationLayer k@KeyRightBrace = Just $ list $ keyCommand k KeyF12 []
modFKeysTranslationLayer _ = Nothing

altTranslationLayer :: Keycode -> Maybe [RootInput]
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

-- If you ever have any problems with the changed xcompose binds, try using the default ones by simply adding to your activation a tap. So that the first keyCommand taps the key, and the second holds.
-- https://www.x.org/releases/current/doc/libX11/i18n/compose/en_US.UTF-8.html
-- å == oa
altTranslationLayer k@KeyP = Just $ [
  (keyCommandTap k KeyO [(ModAlt Release), (ModRAlt Press), (ModShift Release)]),
  (keyCommand    k KeyA [(ModAlt Release), (ModRAlt Press)])
  ]

altTranslationLayer k@KeyComma = Just $ [
  (keyCommandTap k KeyA       [(ModAlt Release), (ModRAlt Press)]),
  (keyCommand k KeyApostrophe [(ModAlt Release), (ModRAlt Press), (ModShift Press)])
  ]
altTranslationLayer k@KeyDot = Just $ [
  (keyCommandTap k KeyO       [(ModAlt Release), (ModRAlt Press)]),
  (keyCommand k KeyApostrophe [(ModAlt Release), (ModRAlt Press), (ModShift Press)])
  ]

altTranslationLayer _ = Nothing

altCtrlTranslationLayer :: Keycode -> Maybe [RootInput]
altCtrlTranslationLayer k@KeyF = Just $ list $ keyCommand k KeyBackspace [(ModAlt Release), (ModCtrl Press)]
altCtrlTranslationLayer k@KeyL = Just $ list $ keyCommand k KeyDelete [(ModAlt Release), (ModCtrl Press)]
-- Alt tab (for windows compatibility. Alt + Ctrl + T shouldn't do anything useful anyways. Shift + Ctrl + T is how you do tab back.)
altCtrlTranslationLayer k@KeyT = Just $ list $ keyCommand k KeyTab [(ModAlt Press), (ModCtrl Release)]
altCtrlTranslationLayer _ = Nothing

rtsTranslationLayer :: Keycode -> Maybe [RootInput]
rtsTranslationLayer k@KeyM = Just $ list $ keyCommand k KeyUp []
rtsTranslationLayer k@KeyT = Just $ list $ keyCommand k KeyDown []
rtsTranslationLayer k@KeyS = Just $ list $ keyCommand k KeyLeft []
rtsTranslationLayer k@KeyN = Just $ list $ keyCommand k KeyRight []
rtsTranslationLayer _ = Nothing

fpsTranslationLayer :: Keycode -> Maybe [RootInput]
fpsTranslationLayer k@KeyM = Just $ list $ keyCommand k KeyW []
fpsTranslationLayer k@KeyT = Just $ list $ keyCommand k KeyS []
fpsTranslationLayer k@KeyS = Just $ list $ keyCommand k KeyA []
fpsTranslationLayer k@KeyN = Just $ list $ keyCommand k KeyD []

-- Rebind overlaps so that they can be used when binding
fpsTranslationLayer k@KeyW = Just $ list $ keyCommand k KeyM []
fpsTranslationLayer k@KeyS = Just $ list $ keyCommand k KeyT []
fpsTranslationLayer k@KeyA = Just $ list $ keyCommand k KeyS []
fpsTranslationLayer k@KeyD = Just $ list $ keyCommand k KeyN []
fpsTranslationLayer _ = Nothing

-- Key pressed without any modifier
rootTranslationLayer :: Keycode -> Maybe [RootInput]
rootTranslationLayer k@KeyTab = Just $ list $ keyCommand k KeyF12 []
rootTranslationLayer _ = Nothing

rootCtrlTranslationLayer :: Keycode -> Maybe [RootInput]
rootCtrlTranslationLayer k@KeyT = Just $ list $ keyCommand k KeyTab [(ModCtrl Release)]
rootCtrlTranslationLayer _ = Nothing

exwmCtrlTranslationLayer :: Keycode -> Maybe [RootInput]
exwmCtrlTranslationLayer _ = Nothing

stenoLayer :: [(RootKeycode, RootInput)] -> [MyModifiersRequested] -> String -> Keycode -> Keycode -> Maybe [RootInput]
-- Handle pressing C-e. K here would be E in a Carpalx layout
stenoLayer last _mod "desktop" k@KeyK _ =
  if length last == 1 && (any ((==) KeyCapsLock) (fst <$> last))
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
ctrlTranslationLayer :: Keycode -> Maybe [RootInput]
ctrlTranslationLayer k@KeyGrave = Just $ list $ keyCommand k KeyCapsLock [(ModCtrl Release)]
ctrlTranslationLayer k@KeyL = Just $ list $ keyCommand k KeyDelete [(ModCtrl Release)]
ctrlTranslationLayer k@KeyF = Just $ list $ keyCommand k KeyBackspace [(ModCtrl Release)]
ctrlTranslationLayer k@KeyA = Just $ list $ keyCommand k KeyEnter [(ModCtrl Release)]
-- Might as well handle this here as C-a is rebound
ctrlTranslationLayer k@KeyM = Just $ list $ keyCommand k KeyA [(ModCtrl Press)]
ctrlTranslationLayer _ = Nothing

-- First keycode is the old key, without carpalx applied
hostnameTranslationLayer :: [Layer] -> String -> Keycode -> Keycode -> Keycode
hostnameTranslationLayer _ "thinkpad-t480" _ KeyBackslash = KeyEnter
hostnameTranslationLayer _ "thinkpad-t480" _ KeyEnter = KeyBackslash

-- Mouse
-- hostnameTranslationLayer _ "thinkpad-t480" _ KeyGrave = BtnLeft
-- hostnameTranslationLayer _ "thinkpad-t480" _ Key1 = BtnRight
-- Left side
hostnameTranslationLayer _ "thinkpad-t480" _ KeyGrave = BtnLeft
hostnameTranslationLayer _ "thinkpad-t480" _ Key1 = BtnRight
-- Right side
hostnameTranslationLayer _ "thinkpad-t480" _ KeyRightAlt = BtnLeft
hostnameTranslationLayer _ "thinkpad-t480" _ KeyPrint = BtnRight
hostnameTranslationLayer _ "thinkpad-t480" KeyPrint _ = BtnRight
hostnameTranslationLayer _ "thinkpad-t480" _ KeyRightCtrl = BtnRight

-- Steno on ergodox
hostnameTranslationLayer l "desktop" _ KeyLeftShift | any findLayerSteno l = KeyC
hostnameTranslationLayer l "desktop" _ KeySpace | any findLayerSteno l = KeyV
hostnameTranslationLayer l "desktop" _ KeyCapsLock | any findLayerSteno l = KeyN
hostnameTranslationLayer l "desktop" _ KeyLeftAlt | any findLayerSteno l = KeyM

-- Move all buttons one step to the left. This is because Ergodox doesn't have the far right side of keys properly aligned as they are bound in Plover
hostnameTranslationLayer l "desktop" KeyY _ | any findLayerSteno l = KeyU
hostnameTranslationLayer l "desktop" KeyU _ | any findLayerSteno l = KeyI
hostnameTranslationLayer l "desktop" KeyI _ | any findLayerSteno l = KeyO
hostnameTranslationLayer l "desktop" KeyO _ | any findLayerSteno l = KeyP
hostnameTranslationLayer l "desktop" KeyP _ | any findLayerSteno l = KeyLeftBrace

hostnameTranslationLayer l "desktop" KeyH _ | any findLayerSteno l = KeyJ
hostnameTranslationLayer l "desktop" KeyJ _ | any findLayerSteno l = KeyK
hostnameTranslationLayer l "desktop" KeyK _ | any findLayerSteno l = KeyL
hostnameTranslationLayer l "desktop" KeyL _ | any findLayerSteno l = KeySemicolon
hostnameTranslationLayer l "desktop" KeySemicolon _ | any findLayerSteno l = KeyApostrophe

-- hostnameTranslationLayer l "desktop" KeyBackslash | any findLayerSteno l = KeyLeftBrace
-- hostnameTranslationLayer l "desktop" KeyRightBrace | any findLayerSteno l = KeyApostrophe
hostnameTranslationLayer _ _ _ a = a

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
