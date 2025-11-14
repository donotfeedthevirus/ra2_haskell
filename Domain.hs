module Domain
  ( Item (..),
    Inventario,
    AcaoLog (..),
    StatusLog (..),
    LogEntry (..),
    ResultadoOperacao,
    emptyInventory,
    readInventory,
    readLogs,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)

data Item = Item
  { itemID :: String,
    nome :: String,
    quantidade :: Int,
    categoria :: String
  }
  deriving (Show, Read, Eq)

type Inventario = Map String Item

data AcaoLog
  = Add
  | Remove
  | Update
  | Listar
  | Reportar
  | QueryFail
  deriving (Show, Read, Eq)

data StatusLog
  = Sucesso
  | Falha String
  deriving (Show, Read, Eq)

data LogEntry = LogEntry
  { logTimestamp :: UTCTime,
    logAcao :: AcaoLog,
    logDetalhes :: String,
    logStatus :: StatusLog
  }
  deriving (Show, Read, Eq)

type ResultadoOperacao = (Inventario, LogEntry)

emptyInventory :: Inventario
emptyInventory = Map.empty

readInventory :: String -> Either String Inventario
readInventory txt =
  case reads txt of
    [(inv, "")] -> Right inv
    _ -> Left "Inventario.dat inválido (falha na desserialização)."

readLogs :: String -> Either String [LogEntry]
readLogs txt = traverse parseLine (filter (not . null) (lines txt))
  where
    parseLine linha =
      case reads linha of
        [(entry, "")] -> Right entry
        _ -> Left "Auditoria.log inválido (falha na desserialização)."

