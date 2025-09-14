-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Simulator.Default.Generator (generateOperators) where

import Language.Haskell.TH

data ValueCons
  = VWord
  | VLong
  | VSingle
  | VDouble
  deriving (Show)

toSigned :: ValueCons -> Maybe String
toSigned VWord = Just "Int32"
toSigned VLong = Just "Int64"
toSigned VSingle = Nothing
toSigned VDouble = Nothing

toSignedExp :: ValueCons -> Exp -> Exp
toSignedExp vCons expr =
  case toSigned vCons of
    Nothing -> expr
    Just st ->
      let cast = AppE (VarE $ mkName "fromIntegral") expr
       in SigE cast (ConT $ mkName st)

------------------------------------------------------------------------

thBinaryFunc :: Exp -> Exp -> Exp -> Exp
thBinaryFunc func lhs = AppE (AppE func lhs)

thBinaryOp :: Exp -> Exp -> Exp -> Exp
thBinaryOp op = thBinaryFunc (ParensE op)

------------------------------------------------------------------------

-- Takes an lhs and rhs value and transform it to some 'Exp'.
type Transformer = ValueCons -> Exp -> Exp -> Exp

applyFunc :: Exp -> ValueCons -> Exp -> Exp -> Exp
applyFunc func vCon lhs rhs =
  AppE (ConE $ mkName (show vCon)) (thBinaryFunc func lhs rhs)

applyOp :: Name -> ValueCons -> Exp -> Exp -> Exp
applyOp opName =
  applyFunc (ParensE (VarE opName))

applySignedOp :: Name -> ValueCons -> Exp -> Exp -> Exp
applySignedOp opName vCon lhs rhs =
  let lhs' = toSignedExp vCon lhs
      rhs' = toSignedExp vCon rhs
      cast = AppE (VarE $ mkName "fromIntegral")
   in -- TODO: Code duplication with applyFunc
      AppE (ConE $ mkName (show vCon)) (cast $ thBinaryFunc (VarE opName) lhs' rhs')

applyBoolOp :: Name -> ValueCons -> Exp -> Exp -> Exp
applyBoolOp opName _vCons lhs rhs =
  let res = thBinaryOp (VarE opName) lhs rhs
      toL = AppE (AppE (VarE $ mkName "E.fromLit") (AppE (ConE $ mkName "QBE.Base") (ConE $ mkName "QBE.Long")))
   in toL $ CondE res (LitE $ IntegerL 1) (LitE $ IntegerL 0)

applySignedBoolOp :: Name -> ValueCons -> Exp -> Exp -> Exp
applySignedBoolOp opName vCons lhs rhs =
  applyBoolOp opName vCons (toSignedExp vCons lhs) (toSignedExp vCons rhs)

------------------------------------------------------------------------

operators :: [(Name, Transformer)]
operators =
  [ (mkName "add'", applyOp (mkName "+")),
    (mkName "sub'", applyOp (mkName "-")),
    (mkName "mul'", applyOp (mkName "*")),
    (mkName "eq'", applyBoolOp (mkName "==")),
    (mkName "ne'", applyBoolOp (mkName "/=")),
    (mkName "sle'", applySignedBoolOp (mkName "<=")),
    (mkName "slt'", applySignedBoolOp (mkName "<")),
    (mkName "sge'", applySignedBoolOp (mkName ">=")),
    (mkName "sgt'", applySignedBoolOp (mkName ">")),
    (mkName "ule'", applyBoolOp (mkName "<=")),
    (mkName "ult'", applyBoolOp (mkName "<")),
    (mkName "uge'", applyBoolOp (mkName ">=")),
    (mkName "ugt'", applyBoolOp (mkName ">"))
  ]

decOperators :: [(Name, Transformer)]
decOperators =
  [ (mkName "srem'", applySignedOp (mkName "rem")),
    (mkName "urem'", applyOp (mkName "rem")),
    (mkName "udiv'", applyOp (mkName "div")),
    (mkName "or'", applyOp (mkName ".|.")),
    (mkName "xor'", applyOp (mkName "Data.Bits.xor")),
    (mkName "and'", applyOp (mkName ".&."))
  ]

------------------------------------------------------------------------

decCons :: [ValueCons]
decCons = [VWord, VLong]

cons :: [ValueCons]
cons = decCons ++ [VSingle, VDouble]

makeClause :: Transformer -> ValueCons -> Q Clause
makeClause trans vCon = do
  lhs <- newName "lhs"
  rhs <- newName "rhs"

  let res = trans vCon (VarE lhs) (VarE rhs)
  let body = AppE (ConE (mkName "Just")) res

  let con = mkName (show vCon)
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

------------------------------------------------------------------------

genOp :: [ValueCons] -> (Name, Transformer) -> Q Dec
genOp opLst (name, trans) = do
  valDefs <- mapM (makeClause trans) opLst
  return $ FunD name (valDefs ++ [typingErrorClause])

generateOperators :: Q [Dec]
generateOperators = do
  o1 <- mapM (genOp cons) operators
  o2 <- mapM (genOp decCons) decOperators
  pure $ o1 ++ o2
