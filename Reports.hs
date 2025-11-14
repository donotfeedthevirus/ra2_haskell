module Reports
  ( historicoPorItem,
    logsDeErro,
    itemMaisMovimentado,
  )
where

import Data.List (isPrefixOf, maximumBy)
import qualified Data.Map as Map
import Data.Ord (comparing)
import Domain

historicoPorItem :: String -> [LogEntry] -> [LogEntry]
historicoPorItem alvo =
  filter (\entry -> extractItem entry == Just alvo)

logsDeErro :: [LogEntry] -> [LogEntry]
logsDeErro = filter ehErro

itemMaisMovimentado :: [LogEntry] -> Maybe String
itemMaisMovimentado entries =
  let counts = foldl conta Map.empty entries
   in if Map.null counts
        then Nothing
        else Just (fst (maximumBy (comparing snd) (Map.toList counts)))
  where
    conta acc entry =
      case extractItem entry of
        Just ident
          | logAcao entry `elem` [Add, Remove, Update] ->
              Map.insertWith (+) ident 1 acc
        _ -> acc

extractItem :: LogEntry -> Maybe String
extractItem entry = busca (words (logDetalhes entry))
  where
    busca [] = Nothing
    busca (w : ws)
      | "item:" `isPrefixOf` w = Just (drop 5 w)
      | otherwise = busca ws

ehErro :: LogEntry -> Bool
ehErro entry =
  case logStatus entry of
    Sucesso -> False
    Falha _ -> True
