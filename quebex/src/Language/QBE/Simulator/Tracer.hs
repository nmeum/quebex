module Language.QBE.Simulator.Tracer where

-- data Branch v
--   = Branch
--   { BTLabel :: QBE.BlockIdent,
--     BFLabel :: QBE.BlockIdent,
--     BConVal :: (Bool, v) }

class Tracer t v where
  branch :: t -> v -> Bool -> t

data NoOp = NoOp

instance Tracer NoOp a where
  branch t _ _ = t
