-- SPDX-FileCopyrightText: 2025 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Types where

import Control.Monad (foldM)
import Data.Word (Word64)

-- TODO: Prefix all constructors

newtype UserIdent = UserIdent {userIdent :: String}
  deriving (Eq, Ord)

instance Show UserIdent where
  show (UserIdent s) = ':' : s

newtype LocalIdent = LocalIdent {localIdent :: String}
  deriving (Eq, Ord)

instance Show LocalIdent where
  show (LocalIdent s) = '%' : s

newtype BlockIdent = BlockIdent {blockIdent :: String}
  deriving (Eq, Ord)

instance Show BlockIdent where
  show (BlockIdent s) = '@' : s

newtype GlobalIdent = GlobalIdent {globalIdent :: String}
  deriving (Eq, Ord)

instance Show GlobalIdent where
  show (GlobalIdent s) = '$' : s

------------------------------------------------------------------------

data BaseType
  = Word
  | Long
  | Single
  | Double
  deriving (Show, Eq)

baseTypeByteSize :: BaseType -> Int
baseTypeByteSize Word = 4
baseTypeByteSize Long = 8
baseTypeByteSize Single = 4
baseTypeByteSize Double = 8

baseTypeBitSize :: BaseType -> Int
baseTypeBitSize ty = baseTypeByteSize ty * 8

data ExtType
  = Base BaseType
  | Byte
  | HalfWord
  deriving (Show, Eq)

extTypeByteSize :: ExtType -> Int
extTypeByteSize (Base b) = baseTypeByteSize b
extTypeByteSize Byte = 1
extTypeByteSize HalfWord = 2

extTypeBitSize :: ExtType -> Int
extTypeBitSize ty = extTypeByteSize ty * 8

data SubWordType
  = SignedByte
  | UnsignedByte
  | SignedHalf
  | UnsignedHalf
  deriving (Show, Eq)

data Abity
  = ABase BaseType
  | ASubWordType SubWordType
  | AUserDef UserIdent
  deriving (Show, Eq)

abityToBase :: Abity -> BaseType
-- Calls with a sub-word return type define a temporary of base type
-- w with its most significant bits unspecified.
abityToBase (ASubWordType _) = Word
-- When an aggregate type is used as argument type or return type, the
-- value respectively passed or returned needs to be a pointer to a
-- memory location holding the value.
abityToBase (AUserDef _) = Long
abityToBase (ABase ty) = ty

data Const
  = Number Word64
  | SFP Float
  | DFP Double
  | Global GlobalIdent
  deriving (Show, Eq)

data DynConst
  = Const Const
  | Thread GlobalIdent
  deriving (Show, Eq)

data Value
  = VConst DynConst
  | VLocal LocalIdent
  deriving (Show, Eq)

data Linkage
  = LExport
  | LThread
  | LSection String (Maybe String)
  deriving (Show, Eq)

data AllocSize
  = AlignWord
  | AlignLong
  | AlignLongLong
  deriving (Show, Eq)

getSize :: AllocSize -> Int
getSize AlignWord = 4
getSize AlignLong = 8
getSize AlignLongLong = 16

data TypeDef
  = TypeDef
  { aggName :: UserIdent,
    aggAlign :: Maybe AllocSize,
    aggType :: AggType
  }
  deriving (Show, Eq)

data SubType
  = SExtType ExtType
  | SUserDef UserIdent
  deriving (Show, Eq)

type Field = (SubType, Maybe Word64)

-- TODO: Type for tuple
data AggType
  = ARegular [Field]
  | AUnion [[Field]]
  | AOpaque Word64
  deriving (Show, Eq)

data DataDef
  = DataDef
  { linkage :: [Linkage],
    name :: GlobalIdent,
    align :: Maybe AllocSize,
    objs :: [DataObj]
  }
  deriving (Show, Eq)

data DataObj
  = OItem ExtType [DataItem]
  | OZeroFill Word64
  deriving (Show, Eq)

data DataItem
  = DSymbol GlobalIdent (Maybe Word64)
  | DString String
  | DConst Const
  deriving (Show, Eq)

data FuncDef
  = FuncDef
  { fLinkage :: [Linkage],
    fName :: GlobalIdent,
    fAbity :: Maybe Abity,
    fParams :: [FuncParam],
    fBlock :: [Block] -- TODO: Use a Map here
  }
  deriving (Show, Eq)

data FuncParam
  = Regular Abity LocalIdent
  | Env LocalIdent
  | Variadic
  deriving (Show, Eq)

data FuncArg
  = ArgReg Abity Value
  | ArgEnv Value
  | ArgVar
  deriving (Show, Eq)

data JumpInstr
  = Jump BlockIdent
  | Jnz Value BlockIdent BlockIdent
  | Return (Maybe Value)
  | Halt
  deriving (Show, Eq)

data LoadType
  = LSubWord SubWordType
  | LBase BaseType
  deriving (Show, Eq)

-- TODO: Could/Should define this on ExtType instead.
loadByteSize :: LoadType -> Word64
loadByteSize (LSubWord UnsignedByte) = 1
loadByteSize (LSubWord SignedByte) = 1
loadByteSize (LSubWord SignedHalf) = 2
loadByteSize (LSubWord UnsignedHalf) = 2
loadByteSize (LBase Word) = 4
loadByteSize (LBase Long) = 8
loadByteSize (LBase Single) = 4
loadByteSize (LBase Double) = 8

data SubLongType
  = SLSubWord SubWordType
  | SLSignedWord
  | SLUnsignedWord
  deriving (Show, Eq)

-- TODO: Distinict types for floating point comparison?
data CmpOp
  = CEq
  | CNe
  | CSle
  | CSlt
  | CSge
  | CSgt
  | CUle
  | CUlt
  | CUge
  | CUgt
  deriving (Show, Eq)

data Instr
  = Add Value Value
  | Sub Value Value
  | -- | Div Value Value
    Mul Value Value
  | URem Value Value
  | Rem Value Value
  | UDiv Value Value
  | Or Value Value
  | Xor Value Value
  | And Value Value
  | Alloc AllocSize Value
  | Load LoadType Value
  | Compare BaseType CmpOp Value Value
  | Ext SubLongType Value
  deriving (Show, Eq)

data VolatileInstr
  = Store ExtType Value Value
  | Blit Value Value Word64
  deriving (Show, Eq)

data Statement
  = Assign LocalIdent BaseType Instr
  | Call (Maybe (LocalIdent, Abity)) Value [FuncArg]
  | Volatile VolatileInstr
  deriving (Show, Eq)

data Block'
  = Block'
  { label' :: BlockIdent,
    -- TODO: phi
    stmt' :: [Statement],
    term' :: Maybe JumpInstr
  }
  deriving (Show, Eq)

insertJumps :: [Block'] -> Maybe [Block]
insertJumps xs = foldM go [] $ zipWithNext xs
  where
    zipWithNext :: [a] -> [(a, Maybe a)]
    zipWithNext [] = []
    zipWithNext lst@(_ : t) = zip lst $ map Just t ++ [Nothing]

    fromBlock' :: Block' -> JumpInstr -> Block
    fromBlock' (Block' l s _) ji = Block l s ji

    go :: [Block] -> (Block', Maybe Block') -> Maybe [Block]
    go acc (x@Block' {term' = Just ji}, _) =
      Just (acc ++ [fromBlock' x ji])
    go acc (x@Block' {term' = Nothing}, Just nxt) =
      Just (acc ++ [fromBlock' x (Jump $ label' nxt)])
    go _ (Block' {term' = Nothing}, Nothing) =
      Nothing

data Block
  = Block
  { label :: BlockIdent,
    -- TODO: phi
    stmt :: [Statement],
    term :: JumpInstr
  }
  deriving (Show, Eq)
