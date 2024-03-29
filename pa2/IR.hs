module IR where

data ConstOperand
    = GP -- global pointer
    | FP -- frame pointer
    | C Integer -- constant
    | A String Integer -- global variable address offset
    | SF String Integer -- class field address offset 
    | DF String -- class field w/ unknown offset
    | R Integer -- register
    | T String Integer -- type
    | L Integer -- code location
    deriving (Eq, Ord, Show)

data VarOperand
    = SV String Integer -- stack variable
    deriving (Eq, Ord, Show)

data Operand = Const ConstOperand | Var VarOperand | SSAVar VarOperand Integer deriving (Eq, Ord, Show)

data Opcode = Phi VarOperand [(Integer, Integer)] | Z ZOp | U UOp Operand | B BOp Operand Operand 
	    | Ter TOp Operand Operand Operand deriving (Eq, Ord, Show)
data ZOp = Wrl | Entrypc | Nop deriving (Eq, Ord, Show)
data UOp = Neg | Isnull | Load | New | Newlist | Checknull | Br | Call | Write | Enter | Ret | Param deriving (Eq,Ord,  Show)
data BOp = Add | Sub | Mul | Div | Mod | Cmpeq | Cmple | Cmplt | Istype | Move | Lddynamic
	 | Blbc | Blbs | Store | Checktype | Checkbounds deriving (Eq, Ord, Show)
data TOp = Stdynamic deriving (Eq, Ord, Show)

data Type = UInt | UBool | BInt | BBool | List | Class String | Dynamic | Pointer Type deriving (Eq, Ord, Show)

data UserType = UserType String [(String, Integer, Type)] deriving (Eq, Show)
data Method = Method String Integer [(String, Integer, Type)] deriving (Eq, Show)
data Global = Global String Integer Type deriving (Eq, Show)
data Instruction = Instruction Integer Opcode (Maybe Type) deriving (Eq, Ord, Show)

data Program = Program [UserType] [Method] [Global] [Instruction] deriving (Eq, Show)
