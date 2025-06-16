module Language.QBE.Simulator.Generator (generateOperators) where

import Language.Haskell.TH

operators :: [(Name, Name)]
operators =
  [ (mkName "add", mkName "+"),
    (mkName "sub", mkName "-")
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

makeClause :: Exp -> (Name, Name -> Exp) -> (Name, Name -> Exp) -> Name -> Q Clause
makeClause op (lhsCon, lhsExpr) (rhsCon, rhsExpr) resCon = do
  lhs <- newName "lhs"
  rhs <- newName "rhs"

  let body =
        AppE
          (ConE (mkName "Right"))
          (AppE (ConE resCon) (UInfixE (lhsExpr lhs) op (rhsExpr rhs)))

  return $
    Clause
      [ ConP lhsCon [] [VarP lhs],
        ConP rhsCon [] [VarP rhs]
      ]
      (NormalB body)
      []

makeStdClause :: Exp -> Name -> Q Clause
makeStdClause op conName = do
  makeClause op (conName, VarE) (conName, VarE) conName

makeSubClauses :: Exp -> Q [Clause]
makeSubClauses op = do
  let wcon = mkName "VWord"
  let lcon = mkName "VLong"

  c1 <- makeClause op (wcon, VarE) (lcon, longToWord) wcon
  c2 <- makeClause op (lcon, longToWord) (wcon, VarE) wcon
  return [c1, c2]
  where
    longToWord :: Name -> Exp
    longToWord var =
      AppE
        (VarE $ mkName "fromIntegral")
        ( UInfixE
            (VarE var)
            (VarE (mkName ".&."))
            (LitE (IntegerL 0xffffffff))
        )

typingErrorClause :: Clause
typingErrorClause =
  Clause
    [WildP, WildP]
    ( NormalB $
        AppE
          (ConE $ mkName "Left")
          (ConE $ mkName "TypingError")
    )
    []

generateOperator :: (Name, Name) -> Q Dec
generateOperator (name, op) = do
  let opExpr = VarE op

  stdClauses <- mapM (makeStdClause opExpr) cons
  subClauses <- makeSubClauses opExpr

  return $ FunD name (stdClauses ++ subClauses ++ [typingErrorClause])

generateOperators :: Q [Dec]
generateOperators = mapM generateOperator operators
