module UArith.Eval where

------------------------------------------------------------------------------
import UArith.Types
------------------------------------------------------------------------------

isNumericVal :: Term -> Bool
isNumericVal CZero = True
isNumericVal (ESucc e) = isNumericVal e
isNumericVal (EPred e) = isNumericVal e -- ??? not in book
isNumericVal _ = False

isVal :: Term -> Bool
isVal CTrue = True
isVal CFalse = True
isVal t = isNumericVal t

evalStep :: Term -> Either String Term
evalStep (ECond CTrue t1 _) = pure t1
evalStep (ECond CFalse _ t2) = pure t2
evalStep (ECond c t1 t2) = do
  c' <- evalStep c
  pure $ ECond c' t1 t2
evalStep (ESucc t) = do
  t' <- evalStep t
  pure $ ESucc t'
evalStep (EPred CZero) = pure CZero
evalStep (EPred (ESucc t)) = pure t
evalStep (EPred t) = do
  t' <- evalStep t
  pure $ EPred t'
evalStep t = Left "No rule applies"

eval :: Term -> Term
eval t = either (const t) eval $ evalStep t

