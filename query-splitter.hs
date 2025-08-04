import Control.Monad.State (State, get)
import SimpleSMT qualified as SMT

transExpr :: SExpr -> State TransEnv
transExpr state (SMT.List [SMT.Atom "push", SMT.Atom "1"]) = do
  newAssertLevel
transExpr state (SMT.List [SMT.Atom "pop", SMT.Atom num]) = do
  assertStackPop (read num :: Integer)
transExpr state expr@(SMT.List [SMT.Atom "check-sat"]) = do
  addA
  trackExpr expr
transExpr state expr@(SMT.List ((SMT.Atom "declare-fun"):_)) = do
  addPreamble state expr
  trackExpr expr
transExpr _ _ = pure Nothing

-- getQueries :: [SExpr] -> [SExpr]
-- getQueries = _
  -- TODO: Detect preamble (set*)
  -- TODO: Detect variables (declare*)
  -- TODO: Inline push/pop

-- readSExprs :: String -> Maybe [SExpr]
-- readSExprs = _
--
-- splitQueries :: FilePath -> IO
-- splitQueries fp = do
--   exprs <- readFile fp >>= readSExprs
--   getQueries exprs

