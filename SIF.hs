{- Start Intermediate Format data structures -}
module SIF where

type SIFNum = Integer -- maybe switch to Int32

type Identifier = String -- name of a variable
type Offset = SIFNum -- offset of a variable
type Size = SIFNum -- size of a type
type Location = SIFNum -- instruction number

data SIFOperand = 
    Global -- pointer to the beginning of the global address space. abbv = GP
  | Frame -- pointer to the beginning of the stack frame of the current function. abbv = FP
  | Constant SIFNum -- constant 32-bit integer
  | Address Identifier Offset -- starting address of a global variable as offset relative to GP
  | StaticField Identifier Offset -- known offset of a field of a class
  | DyanmicField Identifier -- unknown offset of a field of a class
  | Stack Identifier Offset -- stack variable with offset relative to FP. Negative = local; positive = parameter
  | Register SIFNum -- virtual register
  | Type Identifier Size -- represents the named type with its size
  | Label SIFNum -- a code location to jump to
  deriving Show

data SIFType = 
    PrimInt -- 32 bit unboxed integer
  | PrimBool -- unboxed boolean
  | BoxInt -- 32 bit boxed integer
  | BoxBool -- boxed boolean
  | List -- fixed size list
  | Class Identifier -- user defined class
  | Dynamic -- dynamic type representing any boxed value. elements of a list are typed as dynamic
  | Pointer SIFType -- pointer to something of another type
  deriving Show

data SIFOpcode = 
    SideEffect SIFSideEffect -- instructions that only have side effects and produce no value
  | Unary SIFUnary SIFOperand {- input -}  SIFType {- return type -} -- unary value producing instructions
  | Binary SIFBinary SIFOperand {- left input -} SIFOperand {- right input -} SIFType {- return type -} -- binary value producing instructions
  | Branch SIFBranch SIFOperand {- target location -} -- instructions that change the flow of control
  | NOP -- nothing
  deriving Show

data SIFSideEffect = 
    Call SIFOperand {- location -}
  | Store SIFOperand {- value -} SIFOperand {- location -}
  | Move SIFOperand {- value -} SIFOperand {- location -}
  | Checkbounds SIFOperand {- list -} SIFOperand {- index -}
  | StoreDynamic SIFOperand {- value -} SIFOperand {- location -} SIFOperand {- field -}
  | Write SIFOperand {- value -}
  | Newline -- newline
  | Enter SIFOperand {- bytes to allocate for local variables -} -- entry point for functions
  | Ret SIFOperand {- bytes to pop for formal parameters -}
  | Param SIFOperand {- value to push onto stack -}
  | Entrypc -- entry point of main function
  deriving Show

data SIFUnary = Neg | Isnull | Load | New | Newlist | Checknull deriving Show
data SIFBinary = Add | Sub | Mul | Div | Mod | Equal | LessEqual | LessThan | Istype | Checktype | LoadDyanmic deriving Show
data SIFBranch = Jump | IfZero SIFOperand {- test -} | IfSet SIFOperand {- test -} deriving Show

type VarDecl = (Identifier, Size, SIFType)
data SIFTypeDecl = SIFTypeDecl Identifier [VarDecl] deriving Show
data SIFMethod = SIFMethod Identifier Location {- entry instruction location -} [VarDecl] deriving Show
data SIFGlobal = SIFGlobal Identifier Offset {- offset from GP -} SIFType deriving Show
data SIFInstruction = SIFInstruction Location SIFOpcode  deriving Show
data SIFProgram = SIFProgram [SIFTypeDecl] [SIFMethod] [SIFGlobal] [SIFInstruction] deriving Show
