-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Default.Generator (generateOperators) where

import Language.Haskell.TH

operators :: [(Name, Exp)]
operators =
  [ (mkName "add'", ParensE (VarE $ mkName "+")),
    (mkName "sub'", ParensE (VarE $ mkName "-")),
    -- (mkName "div'", ParensE (VarE $ mkName "/")),
    (mkName "mul'", ParensE (VarE $ mkName "*"))
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
          (AppE (ConE con) (AppE (AppE op (VarE lhs)) (VarE rhs)))

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
    (NormalB (ConE $ mkName "Nothing"))
    []

generateOperator :: (Name, Exp) -> Q Dec
generateOperator (name, opExpr) = do
  valDefs <- mapM (makeClause opExpr) cons
  return $ FunD name (valDefs ++ [typingErrorClause])

generateOperators :: Q [Dec]
generateOperators = mapM generateOperator operators
