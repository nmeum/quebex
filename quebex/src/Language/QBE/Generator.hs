module Language.QBE.Generator (generateOperators) where

import Language.Haskell.TH

-- TODO: mkName here
operators :: [(String, String)]
operators =
  [ ("add", "+"),
    ("sub", "-")
  ]

cons :: [Name]
cons =
  [ mkName "EByte",
    mkName "EHalf",
    mkName "EWord",
    mkName "ELong",
    mkName "ESingle",
    mkName "EDouble"
  ]

-- TODO: Take tuple (lhsCon, lhsValue)
makeClause :: Exp -> Name -> Name -> Name -> Q Clause
makeClause op lhsCon rhsCon resCon = do
  lhs <- newName "lhs"
  rhs <- newName "rhs"

  let body =
        AppE
          (ConE (mkName "Right"))
          (AppE (ConE resCon) (UInfixE (VarE lhs) op (VarE rhs)))

  return $
    Clause
      [ ConP lhsCon [] [(VarP lhs)],
        ConP rhsCon [] [(VarP rhs)]
      ]
      (NormalB body)
      []

makeStdClause :: Exp -> Name -> Q Clause
makeStdClause op conName = do
  makeClause op conName conName conName

makeSubClauses :: Exp -> Q [Clause]
makeSubClauses op = do
  let wcon = mkName "EWord"
  let lcon = mkName "ELong"

  c1 <- makeClause op wcon lcon wcon
  c2 <- makeClause op lcon wcon wcon
  return $ [c1, c2]

generateOperator :: (String, String) -> Q Dec
generateOperator (fnName, opName) = do
  let name = mkName $ fnName ++ "RegVals"
  let op = VarE $ mkName opName

  stdClauses <- mapM (makeStdClause op) cons
  subClauses <- makeSubClauses op

  return $ FunD name (stdClauses ++ subClauses)

generateOperators :: Q [Dec]
generateOperators = mapM generateOperator operators
