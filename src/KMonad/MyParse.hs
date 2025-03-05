module KMonad.MyParse (parseServerCmd) where


import KMonad.Parsing
import KMonad.Model.Action
import qualified Text.Megaparsec.Char.Lexer as L
import KMonad.Prelude.Imports
import KMonad.MyTypes
import qualified KMonad.Prelude.Imports as Tr
import qualified KMonad.Prelude.Imports as KPrelude

data ServerCmd =
  ServerKey KeyEvent
  | ServerLayer Text
  | ServerNull
  deriving (Show) 

parseQuotedString :: Parser Text
parseQuotedString = do
  _ <- char '"'
  str <- manyTill L.charLiteral (char '"')
  return (pack str)

cmds :: Parser [ServerCmd]
cmds = -- ("ShellCmd "  *> (ServerShellCmd <$> parseQuotedString))
   -- <|>
   ("SetLayer " *> (ServerLayer <$> parseQuotedString))

parseServerCmd t = case runParser cmds "" t  of
  Left  e -> Tr.trace (KPrelude.pack ("Parsinq error: " ++ (show e))) ServerNull
  Right x -> Tr.trace (KPrelude.pack ("Parsed: " ++ (show x))) x
