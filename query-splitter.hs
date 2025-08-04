import Control.Monad (forM_)
import Control.Monad.State (State, get, modify)
import SimpleSMT qualified as SMT

data TransState
  = TransState
  { assertStack :: [[SMT.SExpr]] }

newAssertLevel :: State TransEnv
newAssertLevel =
  modify (\s -> s { assertStack = assertStack s ++ [] }

transExpr :: SMT.SExpr -> State TransEnv
transExpr state (SMT.List [SMT.Atom "push", SMT.Atom num]) =
  forM_ [0..num] newAssertLevel
-- transExpr state (SMT.List [SMT.Atom "pop", SMT.Atom num]) = do
--   assertStackPop (read num :: Integer)
-- transExpr state expr@(SMT.List [SMT.Atom "check-sat"]) = do
--   addA
--   trackExpr expr
-- transExpr state expr@(SMT.List ((SMT.Atom "declare-fun"):_)) = do
--   addPreamble state expr
--   trackExpr expr
-- transExpr _ _ = pure Nothing

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

