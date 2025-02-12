module KMonad.MyTypes where

import KMonad.Util
import KMonad.Keyboard.Types
import KMonad.Prelude
import KMonad.Model.Action

data ServerCmds =
  ServerKey KeyEvent
  | ServerLayer Text
  | ServerNull
  deriving (Show) 
