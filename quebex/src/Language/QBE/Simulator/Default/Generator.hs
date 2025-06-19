module Language.QBE.Simulator.Default.Generator (generateOperators) where

import Language.Haskell.TH

operators :: [(Name, Name)]
operators =
  [ (mkName "add'", mkName "+"),
    (mkName "sub'", mkName "-")
  ]

cons :: [Name]
cons =
  [ mkName "VByte",
    mkName "VHalf",
    mkName "VWord",
    mkName "VLong",
    mkName "VSingle",
    mkName "VDouble"
  ]

makeClause :: Exp -> Name -> Q Clause
makeClause op con = do
  lhs <- newName "lhs"
  rhs <- newName "rhs"

  let body =
        AppE
          (ConE (mkName "Just"))
          (AppE (ConE con) (UInfixE (VarE lhs) op (VarE rhs)))

  return $
    Clause
      [ ConP con [] [VarP lhs],
        ConP con [] [VarP rhs]
      ]
      (NormalB body)
      []

typingErrorClause :: Clause
typingErrorClause =
  Clause
    [WildP, WildP]
    ( NormalB $
        (ConE $ mkName "Nothing")
    )
    []

generateOperator :: (Name, Name) -> Q Dec
generateOperator (name, op) = do
  let opExpr = VarE op

  valDefs <- mapM (makeClause opExpr) cons
  return $ FunD name (valDefs ++ [typingErrorClause])

generateOperators :: Q [Dec]
generateOperators = mapM generateOperator operators
