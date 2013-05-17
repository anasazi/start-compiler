{- Start Intermediate Format data structures -}
module SIF where

import Pretty
import Text.PrettyPrint.HughesPJ
import InstructionSet

type SIFNum = Integer -- maybe switch to Int32

type SIFIdentifier = String -- name of a variable
type SIFOffset = SIFNum -- offset of a variable
type SIFSize = SIFNum -- size of a type
type SIFLocation = SIFNum -- instruction number

data SIFOperand = 
    Global -- pointer to the beginning of the global address space. abbv = GP
  | Frame -- pointer to the beginning of the stack frame of the current function. abbv = FP
  | Constant SIFNum -- constant 32-bit integer
  | Address SIFIdentifier SIFOffset -- starting address of a global variable as offset relative to GP
  | StaticField SIFIdentifier SIFOffset -- known offset of a field of a class
  | DynamicField SIFIdentifier -- unknown offset of a field of a class
  | Stack SIFIdentifier SIFOffset -- stack variable with offset relative to FP. Negative = local; positive = parameter
  | Register SIFLocation -- virtual register
  | Type SIFIdentifier SIFSize -- represents the named type with its size
  | Label SIFLocation -- a code location to jump to
  deriving Show

data SIFType = 
    UnboxInt -- 32 bit unboxed integer
  | UnboxBool -- unboxed boolean
  | BoxInt -- 32 bit boxed integer
  | BoxBool -- boxed boolean
  | List -- fixed size list
  | Class SIFIdentifier -- user defined class
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
data SIFBinary = Add | Sub | Mul | Div | Mod | Equal | LessEqual | Less | Istype | Checktype | LoadDyanmic deriving Show
data SIFBranch = Jump | IfZero SIFOperand {- test -} | IfSet SIFOperand {- test -} deriving Show

type SIFVarDecl = (SIFIdentifier, SIFSize, SIFType)
data SIFTypeDecl = SIFTypeDecl SIFIdentifier [SIFVarDecl] deriving Show
data SIFMethodDecl = SIFMethodDecl SIFIdentifier SIFLocation {- entry instruction location -} [SIFVarDecl] deriving Show
data SIFGlobalDecl = SIFGlobalDecl SIFIdentifier SIFOffset {- offset from GP -} SIFType deriving Show
data SIFInstruction = SIFInstruction SIFLocation SIFOpcode  deriving Show
data SIFProgram = SIFProgram [SIFTypeDecl] [SIFMethodDecl] [SIFGlobalDecl] [SIFInstruction] deriving Show

instance InstructionSet SIFInstruction where
  idnum (SIFInstruction l _) = l
  isEnter (SIFInstruction _ o) = case o of
    SideEffect (Enter _) -> True
    SideEffect Entrypc -> True
    _ -> False
  isJump (SIFInstruction _ o) = case o of
    Branch _ _ -> True
    SideEffect (Ret _) -> True
    _ -> False
  target (SIFInstruction _ o) = case o of
    Branch _ (Label l) -> Just l
    _ -> Nothing

instance Pretty SIFOperand where
  pretty operand = case operand of
    Global -> text "GP"
    Frame -> text "FP"
    Constant n -> integer n
    Address i o -> text (i ++ "_base#") <> integer o
    StaticField i o -> text (i ++ "_offset#") <> integer o
    DynamicField i -> text (i ++ "_offset#?")
    Stack i o -> text (i ++ "#") <> integer o
    Register l -> parens . integer $ l
    Type i s -> text (i ++ "_type#") <> integer s
    Label l -> brackets . integer $ l

instance Pretty SIFType where
  pretty t = case t of
    UnboxInt -> text "int"
    UnboxBool -> text "bool"
    BoxInt -> text "Integer"
    BoxBool -> text "Boolean"
    List -> text "List"
    Class t -> text t
    Dynamic -> text "dynamic"
    Pointer t -> pretty t <> char '*'

instance Pretty SIFSideEffect where
  pretty se = case se of
    Call l -> text "call" <+> pretty l
    Store v l -> text "store" <+> pretty v <+> pretty l
    Move v l -> text "move" <+> pretty v <+> pretty l
    Checkbounds l x -> text "checkbounds" <+> pretty l <+> pretty x
    StoreDynamic v l f -> text "stdynamic" <+> pretty v <+> pretty l <+> pretty f
    Write v -> text "write" <+> pretty v
    Newline -> text "wrl"
    Enter b -> text "enter" <+> pretty b
    Ret b -> text "ret" <+> pretty b
    Param v -> text "param" <+> pretty v
    Entrypc -> text "entrypc"

instance Pretty SIFUnary where
  pretty u = case u of
    Neg -> text "neg"
    Isnull -> text "isnull"
    Load -> text "load"
    New -> text "new"
    Newlist -> text "newlist"
    Checknull -> text "checknull"

instance Pretty SIFBinary where
  pretty b = case b of
    Add -> text "add"
    Sub -> text "sub"
    Mul -> text "mul"
    Div -> text "div"
    Mod -> text "mod"
    Equal -> text "cmpeq"
    LessEqual -> text "cmple"
    Less -> text "cmplt"
    Istype -> text "istype"
    Checktype -> text "checktype"
    LoadDyanmic -> text "lddynamic"

instance Pretty SIFBranch where
  pretty br = case br of
    Jump -> text "br"
    IfZero t -> text "blbc" <+> pretty t
    IfSet t -> text "blbs" <+> pretty t

instance Pretty SIFOpcode where
  pretty opc = case opc of
    SideEffect se -> pretty se
    Unary uop i t -> pretty uop <+> pretty i <+> colon <> pretty t
    Binary bop i1 i2 t -> pretty bop <+> pretty i1 <+> pretty i2 <+> colon <> pretty t
    Branch br tar -> pretty br <+> pretty tar
    NOP -> text "nop"

prettyVarDecl (i,s,t) = text (i ++ "#") <> integer s <> colon <> pretty t
prettyVarList = hsep . map prettyVarDecl

instance Pretty SIFTypeDecl where
  pretty (SIFTypeDecl i vars) = text ("    type " ++ i) <> colon <+> prettyVarList vars

instance Pretty SIFMethodDecl where
  pretty (SIFMethodDecl i l params) = text ("    method " ++ i ++ "@") <> integer l <> colon <+> prettyVarList params

instance Pretty SIFGlobalDecl where
  pretty (SIFGlobalDecl i o t) = text ("    global " ++ i ++ "#") <> integer o <> colon <> pretty t

instance Pretty SIFInstruction where
  pretty (SIFInstruction l op) = text "    instr" <+> integer l <> colon <+> pretty op

instance (Pretty a) => Pretty [a] where
  pretty = vcat . map pretty

instance Pretty SIFProgram where
  pretty (SIFProgram ts ms gs is) = pretty ts $$ pretty ms $$ pretty gs $$ pretty is
