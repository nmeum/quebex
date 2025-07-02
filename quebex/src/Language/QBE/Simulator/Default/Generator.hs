-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Default.Generator (generateOperators) where

import Language.Haskell.TH

-- Takes an lhs and rhs value and transform it to some 'Exp'.
type Transformer = Exp -> Exp -> Exp

applyFunc :: Exp -> Exp -> Exp -> Exp
applyFunc func lhs rhs =
  AppE (AppE func lhs) rhs

applyOp :: Name -> Exp -> Exp -> Exp
applyOp opName =
  applyFunc (ParensE (VarE opName))

applyBoolOp :: Name -> Exp -> Exp -> Exp
applyBoolOp opName lhs rhs =
  let res = applyOp opName lhs rhs
    in CondE res (LitE $ IntegerL 1) (LitE $ IntegerL 0)

------------------------------------------------------------------------

operators :: [(Name, Transformer)]
operators =
  [ (mkName "add'", applyOp (mkName "+")),
    (mkName "sub'", applyOp (mkName "-")),
    -- TODO: div
    (mkName "mul'", applyOp (mkName "*")),
    (mkName "eq'", applyBoolOp (mkName "==")),
    (mkName "ne'", applyBoolOp (mkName "/=")),
    (mkName "ule'", applyBoolOp (mkName "<=")),
    (mkName "ult'", applyBoolOp (mkName "<")),
    (mkName "uge'", applyBoolOp (mkName ">=")),
    (mkName "ugt'", applyBoolOp (mkName ">"))
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

makeClause :: Transformer -> Name -> Q Clause
makeClause trans con = do
  lhs <- newName "lhs"
  rhs <- newName "rhs"

  let res = trans (VarE lhs) (VarE rhs)
  let body =
        AppE
          (ConE (mkName "Just"))
          (AppE (ConE con) res)

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

generateOperator :: (Name, Transformer) -> Q Dec
generateOperator (name, trans) = do
  valDefs <- mapM (makeClause trans) cons
  return $ FunD name (valDefs ++ [typingErrorClause])

generateOperators :: Q [Dec]
generateOperators = mapM generateOperator operators
