module IR where

-- Data Types for the IR
data Operand 
    = GP -- global pointer
    | FP -- frame pointer
    | C Integer -- constant
    | A String Integer -- global variable address offset
    | SF String Integer -- class field address offset 
    | DF String -- class field w/ unknown offset
    | SV String Integer -- stack variable
    | R Integer -- register
    | T String Integer -- type
    | L Integer -- code location
    deriving (Eq, Show)

data Opcode = Z ZOp | U UOp Operand | B BOp Operand Operand deriving (Eq, Show)
data ZOp = Wrl | Entrypc | Nop deriving (Eq, Show)
data UOp = Neg | Isnull | Load | New | Newlist | Checknull | Br | Call | Write | Enter | Ret | Param deriving (Eq, Show)
data BOp = Add | Sub | Mul | Div | Mod | Cmpeq | Cmple | Cmplt | Istype | Move | Lddynamic
	 | Blbc | Blbs | Store | Checktype | Checkbounds | Stdynamic deriving (Eq, Show)

data Type = UInt | UBool | BInt | BBool | List | Class String | Dynamic | Pointer Type deriving (Eq, Show)

data UserType = UserType String [(String, Integer, Type)] deriving (Eq, Show)
data Method = Method String Integer [(String, Integer, Type)] deriving (Eq, Show)
data Global = Global String Integer Type deriving (Eq, Show)
data Instruction = Instruction Integer Opcode (Maybe Type) deriving (Eq, Show)

data Program = Program [UserType] [Method] [Global] [Instruction] deriving (Eq, Show)
