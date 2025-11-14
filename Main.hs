module Main where

import CommandParser
import qualified Data.Map as Map
import Data.Time (UTCTime, getCurrentTime)
import Domain
import Logic
import Persistence
import Reports
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  putStrLn "=== Sistema de Inventário RA2 (docs/ENUNCIADO.md) ==="
  baseInv <- loadInventory
  baseLogs <- loadLogs
  (inv0, logs0) <- seedIfNeeded baseInv baseLogs
  putStrLn "Digite 'help' para ver os comandos."
  loop inv0 logs0

loop :: Inventario -> [LogEntry] -> IO ()
loop inv logs = do
  putStr "\n> "
  hFlush stdout
  line <- getLine
  case parseCommand line of
    CmdAdd itemId nome qtd cat -> runAdd itemId nome qtd cat inv logs
    CmdRemove itemId qtd -> runRemove itemId qtd inv logs
    CmdUpdate itemId novaQtd -> runUpdate itemId novaQtd inv logs
    CmdList -> runList inv logs
    CmdReport -> runReport inv logs
    CmdHelp -> putStrLn commandHelp >> loop inv logs
    CmdExit -> putStrLn "Saindo..." >> return ()
    CmdInvalid msg -> putStrLn ("Erro: " ++ msg) >> loop inv logs

runAdd :: String -> String -> Int -> String -> Inventario -> [LogEntry] -> IO ()
runAdd itemId nome qtd cat inv logs = do
  now <- getCurrentTime
  case addItem now itemId nome qtd cat inv of
    Left err -> do
      entry <- logFalha now ("add item:" ++ itemId) err
      putStrLn ("Erro: " ++ err)
      loop inv (logs ++ [entry])
    Right (inv', entry) -> do
      persistInventory inv'
      appendAudit entry
      putStrLn "Item adicionado."
      loop inv' (logs ++ [entry])

runRemove :: String -> Int -> Inventario -> [LogEntry] -> IO ()
runRemove itemId qtd inv logs = do
  now <- getCurrentTime
  case removeItem now itemId qtd inv of
    Left err -> do
      entry <- logFalha now ("remove item:" ++ itemId) err
      putStrLn ("Erro: " ++ err)
      loop inv (logs ++ [entry])
    Right (inv', entry) -> do
      persistInventory inv'
      appendAudit entry
      putStrLn "Itens removidos."
      loop inv' (logs ++ [entry])

runUpdate :: String -> Int -> Inventario -> [LogEntry] -> IO ()
runUpdate itemId novaQtd inv logs = do
  now <- getCurrentTime
  case updateQty now itemId novaQtd inv of
    Left err -> do
      entry <- logFalha now ("update item:" ++ itemId) err
      putStrLn ("Erro: " ++ err)
      loop inv (logs ++ [entry])
    Right (inv', entry) -> do
      persistInventory inv'
      appendAudit entry
      putStrLn "Quantidade atualizada."
      loop inv' (logs ++ [entry])

runList :: Inventario -> [LogEntry] -> IO ()
runList inv logs = do
  now <- getCurrentTime
  let (inv', entry) = listItems now inv
  appendAudit entry
  putStrLn "--- Itens cadastrados ---"
  mapM_ printItem (Map.elems inv')
  loop inv' (logs ++ [entry])

runReport :: Inventario -> [LogEntry] -> IO ()
runReport inv logs = do
  now <- getCurrentTime
  let entry = LogEntry now Reportar "report geral" Sucesso
  appendAudit entry
  putStrLn "--- Relatório ---"
  putStrLn ("Itens cadastrados: " ++ show (Map.size inv))
  putStrLn "Erros registrados:"
  mapM_ (putStrLn . formatLog) (logsDeErro logs)
  case itemMaisMovimentado logs of
    Just itemId -> putStrLn ("Item mais movimentado: " ++ itemId)
    Nothing -> putStrLn "Nenhum movimento ainda."
  putStr "ID para histórico (Enter para pular): "
  hFlush stdout
  alvo <- getLine
  if null alvo
    then loop inv (logs ++ [entry])
    else do
      let hist = historicoPorItem alvo logs
      if null hist
        then putStrLn "Sem registros para esse item."
        else mapM_ (putStrLn . formatLog) hist
      loop inv (logs ++ [entry])

logFalha :: UTCTime -> String -> String -> IO LogEntry
logFalha now detalhes msg = do
  let entry = LogEntry now QueryFail (detalhes ++ " erro:" ++ msg) (Falha msg)
  appendAudit entry
  return entry

printItem :: Item -> IO ()
printItem item =
  putStrLn (itemID item ++ " | " ++ nome item ++ " | qtd:" ++ show (quantidade item) ++ " | cat:" ++ categoria item)

formatLog :: LogEntry -> String
formatLog entry =
  show (logTimestamp entry) ++ " - " ++ show (logAcao entry) ++ " - " ++ logDetalhes entry ++ " - " ++ show (logStatus entry)

seedIfNeeded :: Inventario -> [LogEntry] -> IO (Inventario, [LogEntry])
seedIfNeeded inv logs =
  if Map.null inv
    then do
      let seeded = seedInventory
      persistInventory seeded
      now <- getCurrentTime
      let entry = LogEntry now Update "seed inicial" Sucesso
      appendAudit entry
      putStrLn "Inventário inicial criado com 10 itens."
      return (seeded, logs ++ [entry])
    else return (inv, logs)
