# Guia: Executando Testes QuickCheck com GHCi

## 📋 Pré-requisitos

Certifique-se de ter o QuickCheck instalado:

```bash
cabal update
cabal install --lib QuickCheck
```

## 🚀 Como Executar os Testes

### 1. Iniciar o GHCi

Navegue até o diretório onde está o arquivo e carregue-o no GHCi:

```bash
cd /caminho/para/seu/diretorio
ghci haskell_quickcheck_solutions.hs
```

Você verá algo como:

```
GHCi, version 9.6.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling QuickCheckExercises ( haskell_quickcheck_solutions.hs, interpreted )
Ok, one module loaded.
*QuickCheckExercises>
```

### 2. Executar Todos os Testes

Para rodar todos os testes de uma vez:

```haskell
*QuickCheckExercises> main
```

**Saída esperada:**
```
=== Testando propriedades BSTree ===
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.

=== Testando propriedades Set ===
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.
+++ OK, passed 100 tests.

=== Testando propriedades Expr ===
+++ OK, passed 100 tests.
...
```

### 3. Testar Propriedades Individuais

Você pode testar cada propriedade separadamente:

#### **Questão 1 - BSTree:**
```haskell
*QuickCheckExercises> quickCheck prop_BSTOrdering
*QuickCheckExercises> quickCheck prop_BSTInsertMember
*QuickCheckExercises> quickCheck prop_BSTInsertSize
```

#### **Questão 2 - Set:**
```haskell
*QuickCheckExercises> quickCheck prop_SetUnionIdempotent
*QuickCheckExercises> quickCheck prop_SetUnionCommutative
*QuickCheckExercises> quickCheck prop_SetUnionIdentity
*QuickCheckExercises> quickCheck prop_SetInterEmpty
*QuickCheckExercises> quickCheck prop_SetDeMorgan
```

#### **Questão 3 - Expressões:**
```haskell
*QuickCheckExercises> quickCheck prop_ExprAddCommutative
*QuickCheckExercises> quickCheck prop_ExprAddAssociative
*QuickCheckExercises> quickCheck prop_ExprMulCommutative
*QuickCheckExercises> quickCheck prop_ExprMulAssociative
*QuickCheckExercises> quickCheck prop_ExprDistributive
*QuickCheckExercises> quickCheck prop_ExprDoubleNeg
```

#### **Questão 4 - Tree:**
```haskell
*QuickCheckExercises> quickCheck prop_TreeMirrorTwice
*QuickCheckExercises> quickCheck prop_TreeMirrorCollapse
```

#### **Questão 5 - Derivadas e Simplificação:**
```haskell
*QuickCheckExercises> quickCheck prop_DiffNoNewVars
*QuickCheckExercises> quickCheck prop_SimplifyCorrect
```

### 4. Aumentar o Número de Testes

Por padrão, QuickCheck executa 100 testes. Para executar mais:

```haskell
*QuickCheckExercises> quickCheckWith stdArgs { maxSuccess = 1000 } prop_BSTOrdering
```

### 5. Ver Exemplos Gerados

Para ver os valores sendo testados:

```haskell
*QuickCheckExercises> verboseCheck prop_BSTOrdering
```

Isso mostrará cada caso de teste gerado.

### 6. Testar com Exemplos Específicos

Você pode testar propriedades com valores específicos:

```haskell
*QuickCheckExercises> prop_BSTInsertMember 42 (insertTree 10 (insertTree 5 nil))
True

*QuickCheckExercises> prop_ExprAddCommutative (Const 5) (Const 10)
True
```

## 🔄 Recarregar Código Modificado

Se você modificar o arquivo, recarregue no GHCi:

```haskell
*QuickCheckExercises> :reload
```

Ou use o atalho:

```haskell
*QuickCheckExercises> :r
```

## 🛠️ Comandos Úteis do GHCi

| Comando | Descrição |
|---------|-----------|
| `:type expr` ou `:t expr` | Mostra o tipo de uma expressão |
| `:info nome` ou `:i nome` | Mostra informações sobre uma função/tipo |
| `:load arquivo` ou `:l arquivo` | Carrega um arquivo |
| `:reload` ou `:r` | Recarrega o arquivo atual |
| `:quit` ou `:q` | Sai do GHCi |
| `:help` ou `:?` | Mostra ajuda |
| `:browse` | Lista todas as definições do módulo |

## 📊 Exemplos de Uso Avançado

### Gerar e Ver Valores Aleatórios

```haskell
*QuickCheckExercises> sample (arbitrary :: Gen (BSTree Int))
*QuickCheckExercises> sample (arbitrary :: Gen (Set Int))
*QuickCheckExercises> sample (arbitrary :: Gen Expr)
```

### Testar uma Propriedade Específica com Seed

Para reproduzir um teste com seed específico:

```haskell
*QuickCheckExercises> quickCheckWith (stdArgs { replay = Just (mkQCGen 12345, 0) }) prop_BSTOrdering
```

### Verificar Tipos

```haskell
*QuickCheckExercises> :t prop_SetUnionCommutative
prop_SetUnionCommutative :: Set Int -> Set Int -> Bool

*QuickCheckExercises> :t genExpr
genExpr :: Int -> Gen Expr
```

## 🐛 Solução de Problemas

### Erro: "Variable not in scope"

Certifique-se de que você carregou o arquivo corretamente:

```haskell
:load haskell_quickcheck_solutions.hs
```

### Erro: "Could not find module 'Test.QuickCheck'"

Instale o QuickCheck:

```bash
cabal install --lib QuickCheck
```

### GHCi está lento

Se os testes estiverem lentos, compile com otimizações:

```bash
ghc -O2 haskell_quickcheck_solutions.hs
./haskell_quickcheck_solutions
```

## 💡 Dicas

1. **Use Tab para autocompletar** nomes de funções no GHCi
2. **Use `:browse`** para ver todas as funções disponíveis
3. **Use `verboseCheck`** quando uma propriedade falhar para ver os casos de teste
4. **Pressione Ctrl+C** para interromper testes longos
5. **Use `:set +s`** para ver estatísticas de tempo e memória

## 📝 Resumo das Questões Implementadas

- **Questão 1:** 3 propriedades para BSTree (ordenação, inserção/busca, tamanho)
- **Questão 2:** 5 propriedades matemáticas para Set (idempotência, comutatividade, identidade, interseção vazia, De Morgan)
- **Questão 3:** Gerador de expressões + 6 propriedades algébricas (comutatividade, associatividade, distributividade)
- **Questão 4:** Funções collapse/mirror + 2 propriedades para Tree
- **Questão 5:** Diferenciação + simplificação com verificação de correção

---

**Pronto!** Agora você pode testar todas as propriedades QuickCheck de forma interativa e eficiente! 🎉
