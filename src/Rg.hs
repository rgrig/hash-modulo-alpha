module Rg where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Hashable (Hashable, hash)
import Expr (Expr(Var, Lam, App))
import qualified Expr

noninc :: (Hashable v, Ord v) => Expr h v -> Expr Int v
noninc = dumb
-- noninc e = dumb2 Map.empty (dumb e)

noninc16 e = m16 (noninc e)

t16 = 2 ^ 16
m16 (Var h v) = Var (h `mod` t16) v
m16 (Lam h v e) = Lam (h `mod` t16) v (m16 e)
m16 (App h e1 e2) = App (h `mod` t16) (m16 e1) (m16 e2) 

dumb2 :: (Hashable v, Ord v) => Map v Int -> Expr Int v -> Expr Int v
dumb2 env (Var _ v) = case Map.lookup v env of
    Just h -> Var h v
    Nothing -> Var (hash v) v
dumb2 env (Lam h v e) = Lam (33*33*2 + 33*hash v + hf) v f
    where
        f = dumb2 (Map.insert v h env) e
        hf = Expr.annotation f
dumb2 env (App _ e1 e2) = App (33*33*3 + 33 * h1 + h2) f1 f2
    where
        f1 = dumb2 env e1
        f2 = dumb2 env e2
        h1 = Expr.annotation f1
        h2 = Expr.annotation f2

dumb :: Hashable v => Expr h v -> Expr Int v
dumb (Var _ v) = Var 1 v
dumb (Lam _ v e) = Lam (33*33*2 + 33*1 +h) v f
    where
        f = dumb e
        h = Expr.annotation f
dumb (App _ e1 e2) = App (33*33*3 + 33 * h1 + h2) f1 f2
    where
        f1 = dumb e1
        f2 = dumb e2
        h1 = Expr.annotation f1
        h2 = Expr.annotation f2
