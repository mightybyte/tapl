module UArith.Types where

data Term
  = CTrue
  | CFalse
  | CZero
  | ECond Term Term Term
  | ESucc Term
  | EPred Term
  | EIsZero Term
  deriving (Eq,Ord,Show,Read)
