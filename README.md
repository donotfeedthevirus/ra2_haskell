# ra2_haskell

Implementação de referência em Haskell para o trabalho da disciplina de Programação Funcional descrito em `docs/ENUNCIADO.md`.

## Identificação

- Instituição: PUCPR
- Disciplina: Programação Lógica e Funcional
- Professor: Frank Alcantara
- Integrantes: Gustavo Muniz, Vinicius Marcon
- Ambiente Online: https://onlinegdb.com/TxcsRFxwJW

## Como compilar e executar

No terminal (GHC local):

```bash
ghc --make Main.hs
./Main
```

No Online GDB

1. Acesse o link do
2. Compile com `ghc --make Main.hs` (ou use o botão “Run” do ambiente).
3. Execute `./Main`.

Os arquivos `Inventario.dat` e `Auditoria.log` (na raiz do projeto) são atualizados conforme cada operação. Se estiver executando pela primeira vez, o sistema cria automaticamente 10 itens de exemplo.

## Comandos disponíveis

Para esse trabalho decidimos fazer comandos para interagir com a aplicação, cada comando pode receber parâmetros (marcados por `<parâmetro>`).

| Comando                                    | Descrição                                                                                         |
| ------------------------------------------ | ------------------------------------------------------------------------------------------------- |
| `add <id> <nome> <quantidade> <categoria>` | Adiciona novo item (nomes/categorias sem espaço).                                                 |
| `remove <id> <quantidade>`                 | Remove quantidade específica de um item existente.                                                |
| `update <id> <novaQuantidade>`             | Ajusta a quantidade de um item.                                                                   |
| `list`                                     | Lista todos os itens carregados.                                                                  |
| `report`                                   | Executa análises sobre `Auditoria.log` (logs de erro, item mais movimentado, histórico por item). |
| `help`                                     | Mostra o resumo dos comandos.                                                                     |
| `exit`                                     | Finaliza o programa.                                                                              |

## Cenários de teste

1. **Persistência de Estado**
   - Passos: iniciar o programa (arquivos vazios), adicionar `teclado_extra`, `mouse_extra` e `monitor_extra`, executar `list`, sair e iniciar novamente com `list`.
   - Resultado observado: `Inventario.dat` foi criado com 13 itens, `Auditoria.log` registrou as operações e, após reiniciar, o comando `list` exibiu os 13 itens persistidos.
2. **Erro de Lógica (Estoque Insuficiente)**
   - Passos: com o item `monitor_extra` em estoque (=1), executar `remove monitor_extra 15`.
   - Resultado observado: o programa exibiu `Erro: Estoque insuficiente.`, `Inventario.dat` permaneceu com quantidade 1 para `monitor_extra` e `Auditoria.log` recebeu uma linha `QueryFail ... Falha "Estoque insuficiente."`.
3. **Relatório de Erros**
   - Passos: executar `report`, fornecer `monitor_extra` como ID e observar a saída.
   - Resultado observado: o relatório listou o erro de estoque insuficiente em “Erros registrados” e no histórico do item, além de informar o item mais movimentado.
