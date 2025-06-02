module Language.QBE.Types where

import Data.Word (Word64)

-- TODO: Indent type
-- TODO: Prefix all constructors

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

data Abity
  = ABase BaseType
  -- | SubWordType
  | AUserDef String
  deriving (Show, Eq)

data Const
  = Number Word64
  | SFP Float
  | DFP Double
  | Global String
  deriving (Show, Eq)

data DynConst
  = Const Const
  | Thread String
  deriving (Show, Eq)

data Value
  = VConst DynConst
  | VLocal String
  deriving (Show, Eq)

data Linkage
  = LExport
  | LThread
  | LSection String (Maybe String)
  deriving (Show, Eq)

data AllocAlign
  = AlignWord
  | AlignLong
  | AlignLongLong
  deriving (Show, Eq)

data TypeDef
  = TypeDef
  { aggName :: String,
    aggAlign :: Maybe AllocAlign,
    aggType :: AggType
  }
  deriving (Show, Eq)

data SubType
  = SExtType ExtType
  | SUserDef String
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
    name :: String,
    align :: Maybe AllocAlign,
    objs :: [DataObj]
  }
  deriving (Show, Eq)

data DataObj
  = OItem ExtType [DataItem]
  | OZeroFill Word64
  deriving (Show, Eq)

data DataItem
  = DSymbol String (Maybe Word64)
  | DString String
  | DConst Const
  deriving (Show, Eq)

data FuncDef
  = FuncDef
  { fLinkage :: [Linkage]
  , fName :: String
  , fAbity :: Maybe Abity
  , fParams :: [FuncParam]
  , fBlock :: [Block]
  }
  deriving (Show, Eq)

data FuncParam
  = Regular Abity String
  | Env String
  | Variadic
  deriving (Show, Eq)

data JumpInstr
  = Jump String
  | Jnz Value String String
  | Return (Maybe Value)
  | Halt
  deriving (Show, Eq)

data Instr
  = Add Value Value
  | Sub Value Value
  deriving (Show, Eq)

data Statement
  = Assign String BaseType Instr
  -- | Call
  -- | Volative
  deriving (Show, Eq)

data Block
  = Block
  { label :: String
  -- TODO: phi
  , stmt :: [Statement]
  , term :: JumpInstr
  }
  deriving (Show, Eq)
