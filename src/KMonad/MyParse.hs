module KMonad.MyParse (parseServerCmd) where


import KMonad.Parsing
import KMonad.Model.Action
import qualified Text.Megaparsec.Char.Lexer as L
import KMonad.Prelude.Imports
import KMonad.MyTypes
import qualified KMonad.Prelude.Imports as Tr
import qualified KMonad.Prelude.Imports as KPrelude


parseQuotedString :: Parser Text
parseQuotedString = do
  char '"'
  str <- manyTill L.charLiteral (char '"')
  return (pack str)

cmds :: Parser ServerCmds
cmds = ("ShellCmd "  *> (ServerShellCmd <$> parseQuotedString))
   <|> ("LayerOp (PushLayer " *> ((ServerLayerOp . PushLayer) <$> parseQuotedString))
   <|> ("LayerOp (PopLayer " *> ((ServerLayerOp . PopLayer) <$> parseQuotedString))
   <|> ("LayerOp (SetBaseLayer " *> ((ServerLayerOp . SetBaseLayer) <$> parseQuotedString))

parseServerCmd t = case runParser cmds "" t  of
  Left  e -> error (show e)
  Right x -> Tr.trace (KPrelude.pack ("Parsed command: " ++ (show x))) x
