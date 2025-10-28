{-# LANGUAGE FlexibleInstances #-}
module QuickCheckExercises where

import Test.QuickCheck
import Control.Monad (liftM, liftM2, liftM3)

-- ============================================================================
-- MÓDULO BSTree (implementação básica)
-- ============================================================================

data BSTree a = Nil | Node a (BSTree a) (BSTree a)
  deriving (Show, Eq)

nil :: BSTree a
nil = Nil

isNil :: BSTree a -> Bool
isNil Nil = True
isNil _ = False

node :: a -> BSTree a -> BSTree a -> BSTree a
node = Node

sing :: a -> BSTree a
sing x = Node x Nil Nil

elemInTree :: Ord a => a -> BSTree a -> Bool
elemInTree _ Nil = False
elemInTree x (Node y left right)
  | x == y = True
  | x < y = elemInTree x left
  | otherwise = elemInTree x right

insertTree :: Ord a => a -> BSTree a -> BSTree a
insertTree x Nil = Node x Nil Nil
insertTree x (Node y left right)
  | x == y = Node y left right
  | x < y = Node y (insertTree x left) right
  | otherwise = Node y left (insertTree x right)

insertAll :: Ord a => BSTree a -> [a] -> BSTree a
insertAll = foldl (flip insertTree)

flattenTree :: BSTree a -> [a]
flattenTree Nil = []
flattenTree (Node x left right) = flattenTree left ++ [x] ++ flattenTree right

sizeTree :: BSTree a -> Int
sizeTree Nil = 0
sizeTree (Node _ left right) = 1 + sizeTree left + sizeTree right

filterTree :: Ord a => (a -> Bool) -> BSTree a -> BSTree a
filterTree _ Nil = Nil
filterTree p (Node x left right)
  | p x = Node x (filterTree p left) (filterTree p right)
  | otherwise = insertAll (filterTree p left) (flattenTree (filterTree p right))

-- ============================================================================
-- MÓDULO Set (implementação básica)
-- ============================================================================

data Set a = Set (BSTree a)
  deriving Show

instance (Ord a) => Eq (Set a) where
  (==) = eqSet

emptySet :: Set a
emptySet = Set nil

isEmptySet :: Set a -> Bool
isEmptySet (Set tree) = isNil tree

singSet :: a -> Set a
singSet x = Set (sing x)

memSet :: Ord a => Set a -> a -> Bool
memSet (Set tree) x = elemInTree x tree

union :: Ord a => Set a -> Set a -> Set a
union (Set t1) (Set t2) = Set (insertAll t1 (flattenTree t2))

inter :: Ord a => Set a -> Set a -> Set a
inter (Set t1) (Set t2) = Set (filterTree (\x -> elemInTree x t2) t1)

diffSet :: Ord a => Set a -> Set a -> Set a
diffSet (Set t1) (Set t2) = Set (filterTree (\x -> not(elemInTree x t2)) t1)

eqSet :: Ord a => Set a -> Set a -> Bool
eqSet s1 s2 = isEmptySet (diffSet s1 s2) && isEmptySet (diffSet s2 s1)

subSet :: Ord a => Set a -> Set a -> Bool
subSet s1 s2 = eqSet (inter s1 s2) s1

makeSet :: Ord a => [a] -> Set a
makeSet list = Set (insertAll nil list)

mapSet :: Ord b => (a -> b) -> Set a -> Set b
mapSet f s = makeSet (map f (flattenSet s))

filterSet :: Ord a => (a -> Bool) -> Set a -> Set a
filterSet f (Set t) = Set (filterTree f t)

foldSet :: (a -> a -> a) -> a -> Set a -> a
foldSet f s (Set t) = foldr f s (flattenTree t)

cardSet :: Set a -> Int
cardSet (Set tree) = sizeTree tree

flattenSet :: Set a -> [a]
flattenSet (Set tree) = flattenTree tree

-- ============================================================================
-- QUESTÃO 1: Propriedades QuickCheck para BSTree
-- ============================================================================

-- Gerador para BSTree
genBSTree :: (Ord a, Arbitrary a) => Int -> Gen (BSTree a)
genBSTree 0 = return nil
genBSTree n = frequency
  [ (1, return nil)
  , (4, do
      x <- arbitrary
      left <- genBSTree (n `div` 2)
      right <- genBSTree (n `div` 2)
      return (node x left right))
  ]

instance (Ord a, Arbitrary a) => Arbitrary (BSTree a) where
  arbitrary = sized genBSTree

-- Propriedade 1: Propriedade de ordem da BST
-- Para toda árvore, os elementos à esquerda são menores e à direita são maiores
prop_BSTOrdering :: BSTree Int -> Bool
prop_BSTOrdering tree = isOrdered (flattenTree tree)
  where
    isOrdered [] = True
    isOrdered [_] = True
    isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)

-- Propriedade 2: Busca e inserção
-- Se inserimos um elemento, ele deve ser encontrado na árvore
prop_BSTInsertMember :: Int -> BSTree Int -> Bool
prop_BSTInsertMember x tree = elemInTree x (insertTree x tree)

-- Propriedade 3: Tamanho após inserção
-- O tamanho aumenta em no máximo 1 após inserção (pode não aumentar se já existe)
prop_BSTInsertSize :: Int -> BSTree Int -> Bool
prop_BSTInsertSize x tree = 
  let oldSize = sizeTree tree
      newSize = sizeTree (insertTree x tree)
  in newSize == oldSize || newSize == oldSize + 1

-- ============================================================================
-- QUESTÃO 2: Cinco propriedades QuickCheck para Set
-- ============================================================================

-- Gerador para Set
genSet :: (Ord a, Arbitrary a) => Int -> Gen (Set a)
genSet n = do
  list <- vectorOf n arbitrary
  return (makeSet list)

instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
  arbitrary = sized genSet

-- Propriedade 1: Idempotência da união - A ∪ A = A
prop_SetUnionIdempotent :: Set Int -> Bool
prop_SetUnionIdempotent s = eqSet (union s s) s

-- Propriedade 2: Comutatividade da união - A ∪ B = B ∪ A
prop_SetUnionCommutative :: Set Int -> Set Int -> Bool
prop_SetUnionCommutative s1 s2 = eqSet (union s1 s2) (union s2 s1)

-- Propriedade 3: Elemento neutro da união - A ∪ ∅ = A
prop_SetUnionIdentity :: Set Int -> Bool
prop_SetUnionIdentity s = eqSet (union s emptySet) s

-- Propriedade 4: Interseção com conjunto vazio - A ∩ ∅ = ∅
prop_SetInterEmpty :: Set Int -> Bool
prop_SetInterEmpty s = isEmptySet (inter s emptySet)

-- Propriedade 5: Lei de De Morgan - A \ (B ∪ C) = (A \ B) ∩ (A \ C)
prop_SetDeMorgan :: Set Int -> Set Int -> Set Int -> Bool
prop_SetDeMorgan a b c = 
  eqSet (diffSet a (union b c)) (inter (diffSet a b) (diffSet a c))

-- ============================================================================
-- QUESTÃO 3: Expressões aritméticas sem variáveis
-- ============================================================================

data Expr = Const Integer | Add Expr Expr | Mul Expr Expr | Neg Expr
  deriving (Show, Eq)

-- 3.1: Gerador para Expr com limite de tamanho
genExpr :: Int -> Gen Expr
genExpr 0 = liftM Const arbitrary
genExpr n = frequency
  [ (1, liftM Const arbitrary)
  , (3, liftM2 Add (genExpr (n `div` 2)) (genExpr (n `div` 2)))
  , (3, liftM2 Mul (genExpr (n `div` 2)) (genExpr (n `div` 2)))
  , (2, liftM Neg (genExpr (n - 1)))
  ]

-- 3.2: Instância Arbitrary para Expr
instance Arbitrary Expr where
  arbitrary = sized genExpr

-- Função de avaliação (fornecida)
eval :: Expr -> Integer
eval (Const n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Neg e) = -(eval e)

-- 3.3: Propriedades algébricas

-- Propriedade 1: Comutatividade da adição - A + B = B + A
prop_ExprAddCommutative :: Expr -> Expr -> Bool
prop_ExprAddCommutative e1 e2 = eval (Add e1 e2) == eval (Add e2 e1)

-- Propriedade 2: Associatividade da adição - A + (B + C) = (A + B) + C
prop_ExprAddAssociative :: Expr -> Expr -> Expr -> Bool
prop_ExprAddAssociative e1 e2 e3 = 
  eval (Add e1 (Add e2 e3)) == eval (Add (Add e1 e2) e3)

-- Propriedade 3: Comutatividade da multiplicação - A * B = B * A
prop_ExprMulCommutative :: Expr -> Expr -> Bool
prop_ExprMulCommutative e1 e2 = eval (Mul e1 e2) == eval (Mul e2 e1)

-- Propriedade 4: Associatividade da multiplicação - A * (B * C) = (A * B) * C
prop_ExprMulAssociative :: Expr -> Expr -> Expr -> Bool
prop_ExprMulAssociative e1 e2 e3 = 
  eval (Mul e1 (Mul e2 e3)) == eval (Mul (Mul e1 e2) e3)

-- Propriedade 5: Distributividade - A * (B + C) = A * B + A * C
prop_ExprDistributive :: Expr -> Expr -> Expr -> Bool
prop_ExprDistributive e1 e2 e3 = 
  eval (Mul e1 (Add e2 e3)) == eval (Add (Mul e1 e2) (Mul e1 e3))

-- Propriedade 6: Dupla negação - -(-A) = A
prop_ExprDoubleNeg :: Expr -> Bool
prop_ExprDoubleNeg e = eval (Neg (Neg e)) == eval e

-- ============================================================================
-- QUESTÃO 4: Tree com Leaf e Split
-- ============================================================================

data Tree = Leaf Int | Split Tree Tree
  deriving (Show, Eq)

-- Gerador para Tree
genTree :: Int -> Gen Tree
genTree 0 = liftM Leaf arbitrary
genTree n = frequency
  [ (1, liftM Leaf arbitrary)
  , (3, liftM2 Split (genTree (n `div` 2)) (genTree (n `div` 2)))
  ]

instance Arbitrary Tree where
  arbitrary = sized genTree

-- 4.a: collapse - retorna valores como lista
collapse :: Tree -> [Int]
collapse (Leaf n) = [n]
collapse (Split left right) = collapse left ++ collapse right

-- 4.b: mirror - espelha a árvore
mirror :: Tree -> Tree
mirror (Leaf n) = Leaf n
mirror (Split left right) = Split (mirror right) (mirror left)

-- 4.c: Propriedade espelhar duas vezes - mirror (mirror t) = t
prop_TreeMirrorTwice :: Tree -> Bool
prop_TreeMirrorTwice t = mirror (mirror t) == t

-- 4.d: Relacionamento entre mirror, collapse e reverse
-- collapse (mirror t) = reverse (collapse t)
prop_TreeMirrorCollapse :: Tree -> Bool
prop_TreeMirrorCollapse t = collapse (mirror t) == reverse (collapse t)

-- ============================================================================
-- QUESTÃO 5: Expressões com variáveis e diferenciação
-- ============================================================================

data ExprVar
  = Num Integer
  | AddVar ExprVar ExprVar
  | MulVar ExprVar ExprVar
  | Var Name
  deriving (Eq, Show)

type Name = String

-- Gerador para ExprVar
genExprVar :: Int -> Gen ExprVar
genExprVar 0 = frequency
  [ (1, liftM Num arbitrary)
  , (1, liftM Var (elements ["x", "y", "z", "w"]))
  ]
genExprVar n = frequency
  [ (1, liftM Num arbitrary)
  , (1, liftM Var (elements ["x", "y", "z", "w"]))
  , (3, liftM2 AddVar (genExprVar (n `div` 2)) (genExprVar (n `div` 2)))
  , (3, liftM2 MulVar (genExprVar (n `div` 2)) (genExprVar (n `div` 2)))
  ]

instance Arbitrary ExprVar where
  arbitrary = sized genExprVar

-- Função diffVar para derivadas (renomeada para evitar conflito)
diffVar :: ExprVar -> Name -> ExprVar
diffVar (Num n) x = Num 0
diffVar (AddVar a b) x = AddVar (diffVar a x) (diffVar b x)
diffVar (MulVar a b) x = AddVar (MulVar a (diffVar b x)) (MulVar b (diffVar a x))
diffVar (Var y) x
  | x == y = Num 1
  | otherwise = Num 0

-- Conta variáveis em uma expressão
countVars :: ExprVar -> Int
countVars (Num _) = 0
countVars (Var _) = 1
countVars (AddVar a b) = countVars a + countVars b
countVars (MulVar a b) = countVars a + countVars b

-- 5.a: Propriedade - derivada não adiciona novas variáveis
prop_DiffNoNewVars :: ExprVar -> Bool
prop_DiffNoNewVars e = countVars (diffVar e "x") <= countVars e
-- O oposto NÃO é verdade: a derivada pode remover variáveis
-- Exemplo: diffVar (Num 5) "x" = Num 0 tem 0 variáveis, menos que Num 5

-- 5.b: Função simplify
simplify :: ExprVar -> ExprVar
simplify (Num n) = Num n
simplify (Var x) = Var x
simplify (AddVar e1 e2) = 
  case (simplify e1, simplify e2) of
    (Num 0, e) -> e
    (e, Num 0) -> e
    (Num n1, Num n2) -> Num (n1 + n2)
    (e1', e2') -> AddVar e1' e2'
simplify (MulVar e1 e2) = 
  case (simplify e1, simplify e2) of
    (Num 0, _) -> Num 0
    (_, Num 0) -> Num 0
    (Num 1, e) -> e
    (e, Num 1) -> e
    (Num n1, Num n2) -> Num (n1 * n2)
    (e1', e2') -> MulVar e1' e2'

-- Tipo para ambiente de variáveis
newtype Env = Env [(Name, Integer)]
  deriving Show

instance Arbitrary Env where
  arbitrary = do
    vals <- vectorOf 4 arbitrary
    return $ Env (zip ["x", "y", "z", "w"] vals)

-- Avaliação com ambiente
evalVar :: [(Name, Integer)] -> ExprVar -> Integer
evalVar env (Num n) = n
evalVar env (Var x) = case lookup x env of
  Just v -> v
  Nothing -> 0
evalVar env (AddVar e1 e2) = evalVar env e1 + evalVar env e2
evalVar env (MulVar e1 e2) = evalVar env e1 * evalVar env e2

-- Propriedade: simplify preserva semântica
prop_SimplifyCorrect :: ExprVar -> Env -> Bool
prop_SimplifyCorrect e (Env env) = 
  evalVar env e == evalVar env (simplify e)

-- ============================================================================
-- Função main para executar os testes
-- ============================================================================

main :: IO ()
main = do
  putStrLn "=== Testando propriedades BSTree ==="
  quickCheck prop_BSTOrdering
  quickCheck prop_BSTInsertMember
  quickCheck prop_BSTInsertSize
  
  putStrLn "\n=== Testando propriedades Set ==="
  quickCheck prop_SetUnionIdempotent
  quickCheck prop_SetUnionCommutative
  quickCheck prop_SetUnionIdentity
  quickCheck prop_SetInterEmpty
  quickCheck prop_SetDeMorgan
  
  putStrLn "\n=== Testando propriedades Expr ==="
  quickCheck prop_ExprAddCommutative
  quickCheck prop_ExprAddAssociative
  quickCheck prop_ExprMulCommutative
  quickCheck prop_ExprMulAssociative
  quickCheck prop_ExprDistributive
  quickCheck prop_ExprDoubleNeg
  
  putStrLn "\n=== Testando propriedades Tree ==="
  quickCheck prop_TreeMirrorTwice
  quickCheck prop_TreeMirrorCollapse
  
  putStrLn "\n=== Testando propriedades ExprVar ==="
  quickCheck prop_DiffNoNewVars
  quickCheck prop_SimplifyCorrect