module Persistence
  ( loadInventory,
    loadLogs,
    persistInventory,
    appendAudit,
    seedInventory,
  )
where

import Control.Exception (IOException, catch)
import qualified Data.Map as Map
import Domain
import System.Directory (doesFileExist)

inventoryPath :: FilePath
inventoryPath = "Inventario.dat"

auditPath :: FilePath
auditPath = "Auditoria.log"

loadInventory :: IO Inventario
loadInventory = do
  exists <- doesFileExist inventoryPath
  if not exists
    then return emptyInventory
    else do
      txt <- safeRead inventoryPath
      case readInventory txt of
        Right inv -> return inv
        Left _ -> return emptyInventory

loadLogs :: IO [LogEntry]
loadLogs = do
  exists <- doesFileExist auditPath
  if not exists
    then return []
    else do
      txt <- safeRead auditPath
      case readLogs txt of
        Right ls -> return ls
        Left _ -> return []

persistInventory :: Inventario -> IO ()
persistInventory inv = writeFile inventoryPath (show inv)

appendAudit :: LogEntry -> IO ()
appendAudit entry = appendFile auditPath (show entry ++ "\n")

seedInventory :: Inventario
seedInventory = Map.fromList (map attachId itens)
  where
    attachId item = (itemID item, item)
    itens =
      [ Item "item01" "Teclado" 10 "Periferico",
        Item "item02" "Mouse" 15 "Periferico",
        Item "item03" "Monitor" 8 "Video",
        Item "item04" "Notebook" 5 "Computador",
        Item "item05" "Cadeira" 12 "Escritorio",
        Item "item06" "Mesa" 7 "Escritorio",
        Item "item07" "Headset" 9 "Audio",
        Item "item08" "Webcam" 6 "Video",
        Item "item09" "HD Externo" 11 "Armazenamento",
        Item "item10" "Pen Drive" 20 "Armazenamento"
      ]

safeRead :: FilePath -> IO String
safeRead path = readFile path `catch` handle
  where
    handle :: IOException -> IO String
    handle _ = return ""
