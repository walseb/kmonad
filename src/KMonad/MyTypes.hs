module KMonad.MyTypes where

import KMonad.Util
import KMonad.Keyboard.Types
import KMonad.Prelude
import KMonad.Model.Action

data ServerCmds =
  ServerEmit KeyEvent
  | ServerPause Milliseconds
  | ServerHold Bool
  | ServerLayerOp LayerOp
  | ServerInject KeyEvent
  | ServerShellCmd Text
  | ServerNull
  deriving (Show) 

instance Show LayerOp where
  show (PushLayer a) = "(PushLayer " ++ show a ++ ")"
  show (PopLayer a) = "(PopLayer " ++ show a ++ ")"
  show (SetBaseLayer a) = "(SetBaseLayer " ++ show a ++ ")"

