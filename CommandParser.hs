module CommandParser
  ( Command (..),
    parseCommand,
    commandHelp,
  )
where

data Command
  = CmdAdd String String Int String
  | CmdRemove String Int
  | CmdUpdate String Int
  | CmdList
  | CmdReport
  | CmdHelp
  | CmdExit
  | CmdInvalid String
  deriving (Show, Eq)

parseCommand :: String -> Command
parseCommand input =
  case words input of
    ("add" : itemId : nome : qtd : categoria : _) ->
      case readMaybeInt qtd of
        Just n -> CmdAdd itemId nome n categoria
        Nothing -> CmdInvalid "Quantidade inválida."
    ("remove" : itemId : qtd : _) ->
      case readMaybeInt qtd of
        Just n -> CmdRemove itemId n
        Nothing -> CmdInvalid "Quantidade inválida."
    ("update" : itemId : novaQtd : _) ->
      case readMaybeInt novaQtd of
        Just n -> CmdUpdate itemId n
        Nothing -> CmdInvalid "Quantidade inválida."
    ["list"] -> CmdList
    ["report"] -> CmdReport
    ["help"] -> CmdHelp
    ["exit"] -> CmdExit
    [] -> CmdHelp
    _ -> CmdInvalid "Comando não reconhecido."

commandHelp :: String
commandHelp =
  unlines
    [ "Comandos disponíveis:",
      " add <id> <nome> <quantidade> <categoria>",
      " remove <id> <quantidade>",
      " update <id> <novaQuantidade>",
      " list",
      " report",
      " help",
      " exit",
      "",
      "Observação: utilize nomes/categorias sem espaço (ex.: Teclado_Mecanico)."
    ]

readMaybeInt :: String -> Maybe Int
readMaybeInt txt =
  case reads txt of
    [(n, "")] -> Just n
    _ -> Nothing
