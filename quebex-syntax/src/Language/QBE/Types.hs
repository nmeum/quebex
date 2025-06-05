module Language.QBE.Types where

import Data.Word (Word64)

-- TODO: Prefix all constructors

newtype UserIdent = UserIdent { userIdent :: String }
  deriving (Eq)

instance Show UserIdent where
  show (UserIdent s) = ':' : s

newtype LocalIdent = LocalIdent { localIdent :: String }
  deriving (Eq)

instance Show LocalIdent where
  show (LocalIdent s) = '%' : s

newtype BlockIdent = BlockIdent { blockIdent :: String }
  deriving (Eq)

instance Show BlockIdent where
  show (BlockIdent s) = '@' : s

newtype GlobalIdent = GlobalIdent { globalIdent :: String }
  deriving (Eq)

instance Show GlobalIdent where
  show (GlobalIdent s) = '$' : s

------------------------------------------------------------------------

data BaseType
  = Word
  | Long
  | Single
  | Double
  deriving (Show, Eq)

data ExtType
  = Base BaseType
  | Byte
  | HalfWord
  deriving (Show, Eq)

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
  { fLinkage :: [Linkage]
  , fName :: GlobalIdent
  , fAbity :: Maybe Abity
  , fParams :: [FuncParam]
  , fBlock :: [Block]
  }
  deriving (Show, Eq)

data FuncParam
  = Regular Abity LocalIdent
  | Env LocalIdent
  | Variadic
  deriving (Show, Eq)

data JumpInstr
  = Jump BlockIdent
  | Jnz Value BlockIdent BlockIdent
  | Return (Maybe Value)
  | Halt
  deriving (Show, Eq)

data Instr
  = Add Value Value
  | Sub Value Value
  | Alloc AllocSize Word64
  deriving (Show, Eq)

data VolatileInstr
  = Store ExtType Value Value
  | Blit Value Value Word64
  deriving (Show, Eq)

data Statement
  = Assign LocalIdent BaseType Instr
  | Volatile VolatileInstr
  deriving (Show, Eq)

data Block
  = Block
  { label :: BlockIdent
  -- TODO: phi
  , stmt :: [Statement]
  , term :: JumpInstr
  }
  deriving (Show, Eq)
