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

evalStep (EPred (ESucc t)) | isNumericVal t = pure t
evalStep (EPred t) = do
  t' <- evalStep t
  pure $ EPred t'
evalStep (EIsZero CZero) = pure CTrue
evalStep (EIsZero (ESucc t)) | isNumericVal t = pure CFalse
evalStep _ = err

eval :: Term -> Term
eval t = either (const t) eval $ evalStep t

evalBigStep :: Term -> Either String Term
evalBigStep CTrue = pure CTrue
evalBigStep CFalse = pure CFalse
evalBigStep CZero = pure CZero
evalBigStep (ECond c t1 t2) = do
  c' <- evalBigStep c
  case c' of
    CTrue -> evalBigStep t1
    CFalse -> evalBigStep t2
    _ -> err
evalBigStep (ESucc t) = ESucc <$> evalBigStep t
evalBigStep (EPred t) = do
  t' <- evalBigStep t
  case t' of
    CZero -> pure CZero
    ESucc v -> pure v
    _ -> err
evalBigStep (EIsZero t) = do
  t' <- evalBigStep t
  case t' of
    CZero -> pure CTrue
    ESucc _ -> pure CFalse
    _ -> err

err :: Either String Term
err = Left "No rule applies"
