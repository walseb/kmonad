{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module KMonad.My (server, serverMVar, executeServerCmd) where

import Control.Monad (void, when)
import Control.Monad.Catch (catch, SomeException)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Map.Strict (Map)
import Network.Socket (Socket)
import Streamly.Data.Fold (Fold)
import System.Random (randomIO)

import Control.Concurrent (forkIO)

import qualified Streamly.Data.Array as Array
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Parser as Parser
import qualified Streamly.Data.Stream.Prelude as Stream
import qualified Streamly.Network.Inet.TCP as TCP
import qualified Streamly.Network.Socket as Socket
import qualified Streamly.Unicode.Stream as Unicode

import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.List (find)
import Data.Vector ( Vector, fromList, snoc, toList )
import Control.Concurrent.MVar
import Prelude

import qualified Streamly.Internal.Data.Time.Clock as Clock
    (getTime, Clock(..))
import qualified Debug.Trace as Tr
import KMonad.Model.Action (MonadKIO (..))

import System.IO.Unsafe (unsafePerformIO)
import KMonad.Model.Action
import KMonad.Util
import KMonad.Keyboard.Types
import KMonad.Prelude (Text (..))
import qualified KMonad.Prelude as KPrelude
import qualified KMonad.Prelude.Imports as KPrelude
import KMonad.Prelude.Imports ((<|>), ($>))
import qualified KMonad.Parsing as Par
import qualified Text.Megaparsec.Char.Lexer as L
import KMonad.MyTypes
import KMonad.MyParse

-- import KMonad.Prelude

------------------------------------------------------------------------------
-- Utility functions
------------------------------------------------------------------------------

sendValue :: Show a => Socket -> a -> IO ()
sendValue sk x =
      Stream.fromList (show x ++ "\n")
    & Unicode.encodeLatin1
    & Stream.fold (Array.writeN 60)
    >>= Socket.putChunk sk

------------------------------------------------------------------------------
-- Command Handlers
------------------------------------------------------------------------------

-- shutdown :: MVar ServerCmds -> Socket -> IO ()
-- shutdown shutdownMvar sk = do
--   pure $ ServerEmit

random :: Socket -> IO ()
random sk = (randomIO :: IO Int) >>= sendValue sk

def :: String -> Socket -> IO ()
def str sk = sendValue sk ("Unknown command: " ++ str)

-- emit       :: KeyEvent -> m ()
-- -- | Pause the current thread for n milliseconds
-- pause      :: Milliseconds -> m ()
-- -- | Pause or unpause event processing
-- hold       :: Bool -> m ()
-- -- | Register a callback hook
-- register   :: HookLocation -> Hook m -> m ()
-- -- | Run a layer-stack manipulation
-- layerOp    :: LayerOp -> m ()
-- -- | Insert an event in the input queue
-- inject     :: KeyEvent -> m ()
-- -- | Run a shell-command
-- shellCmd   :: Text -> m ()
commands :: MVar ServerCmds -> String -> IO (Fold IO Socket ())
commands uMvar cmd = do
  return (Fold.drainMapM (\_sock -> update cmd))
  where
    update msg = do
      tryTakeMVar uMvar
      putMVar uMvar (parseServerCmd (KPrelude.pack msg))

executeServerCmd (ServerEmit a) = emit a
executeServerCmd (ServerPause a) = pause a
executeServerCmd (ServerHold a) = hold a
executeServerCmd (ServerLayerOp a) = layerOp a
executeServerCmd (ServerInject a) = inject a
executeServerCmd (ServerShellCmd a) = shellCmd a
executeServerCmd ServerNull = pure ()

{-# INLINE demuxKvToMap #-}
demuxKvToMap :: (Monad m, Ord k) => (k -> m (Fold m a b)) -> Fold m (k, a) (Map k b)
demuxKvToMap f = Fold.demuxToMap fst (\(k, _) -> fmap (Fold.lmap snd) (f k))

demux :: MVar ServerCmds -> Fold IO (String, Socket) ()
demux uMvar = void (demuxKvToMap (commands uMvar))

------------------------------------------------------------------------------
-- Parse and handle commands on a socket
------------------------------------------------------------------------------

handler :: MVar ServerCmds -> Socket -> IO ()
handler uMvar sk = do
    Socket.read sk        -- Stream IO Word8
      & Unicode.decodeLatin1  -- Stream IO Char
      & Stream.parseMany word -- Stream IO String
      & Stream.catRights
      & fmap (, sk)           -- Stream IO (String, Socket)
      & Stream.fold (demux uMvar)     -- IO () + Exceptions
      & discard               -- IO ()

    where

    word = Parser.wordBy (== '\n') Fold.toList
    discard action = void action `catch` (\(_ :: SomeException) -> return ())

------------------------------------------------------------------------------
-- Accept connecttions and handle connected sockets
------------------------------------------------------------------------------

server :: MVar ServerCmds -> IO ()
server uMvar =
      TCP.accept 9199                                -- Stream IO Socket
    & Stream.parMapM Prelude.id (Socket.forSocketM (handler uMvar))  -- Stream IO ()
    & Stream.fold Fold.drain                         -- IO ()

serverMVar :: MVar ServerCmds
serverMVar = unsafePerformIO $ KPrelude.newEmptyMVar
{-# NOINLINE serverMVar #-}


launchServer :: MVar ServerCmds -> IO ()
launchServer mvar = do
  -- Currently, errors still go through. Wait in case the port isn't bound
  KPrelude.threadDelay 2000000
  () <- KPrelude.catch
              (server mvar)
              (\e -> do let err = show (e :: KPrelude.IOException)
                        -- We can't print in the server thread
                        Tr.trace ("Error: " ++ err) (pure ())
                        return ())
  -- Wait until port is avaliable again
  KPrelude.threadDelay 2000000
  launchServer mvar
