module Logic
  ( addItem,
    removeItem,
    updateQty,
    listItems,
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Domain

addItem :: UTCTime -> String -> String -> Int -> String -> Inventario -> Either String ResultadoOperacao
addItem now newId newNome qty cat inv
  | null newId = Left "ID do item não pode ser vazio."
  | qty <= 0 = Left "Quantidade deve ser positiva."
  | Map.member newId inv = Left "Já existe um item com esse ID."
  | otherwise =
      let item = Item newId newNome qty cat
          inv' = Map.insert newId item inv
          detalhes = "add item:" ++ newId ++ " qty:" ++ show qty
          entry = LogEntry now Add detalhes Sucesso
       in Right (inv', entry)

removeItem :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
removeItem now targetId qtd inv = do
  item <- maybe (Left "Item inexistente.") Right (Map.lookup targetId inv)
  if qtd <= 0
    then Left "Quantidade deve ser positiva."
    else
      if qtd > quantidade item
        then Left "Estoque insuficiente."
        else
          let novaQtd = quantidade item - qtd
              inv' =
                if novaQtd == 0
                  then Map.delete targetId inv
                  else Map.insert targetId (item {quantidade = novaQtd}) inv
              detalhes = "remove item:" ++ targetId ++ " qty:" ++ show qtd
              entry = LogEntry now Remove detalhes Sucesso
           in Right (inv', entry)

updateQty :: UTCTime -> String -> Int -> Inventario -> Either String ResultadoOperacao
updateQty now targetId novaQtd inv = do
  item <- maybe (Left "Item inexistente.") Right (Map.lookup targetId inv)
  if novaQtd < 0
    then Left "Quantidade final não pode ser negativa."
    else
      let atual = item {quantidade = novaQtd}
          inv' = Map.insert targetId atual inv
          detalhes = "update item:" ++ targetId ++ " qty:" ++ show novaQtd
          entry = LogEntry now Update detalhes Sucesso
       in Right (inv', entry)

listItems :: UTCTime -> Inventario -> ResultadoOperacao
listItems now inv =
  let detalhes = "list count:" ++ show (Map.size inv)
      entry = LogEntry now Listar detalhes Sucesso
   in (inv, entry)
