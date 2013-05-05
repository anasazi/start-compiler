{- Start Intermediate Format data structures -}
module SIF where

import Pretty
import Text.PrettyPrint.HughesPJ
import InstructionSet

type SIFNum = Integer 

type SIFIdentifier = String
type SIFOffset = SIFNum 
type SIFSize = SIFNum 
type SIFLocation = SIFNum 

data SIFOperand = 
    Global
  | Frame
  | Constant SIFNum 
  | Address SIFIdentifier SIFOffset 
  | StaticField SIFIdentifier SIFOffset 
  | DynamicField SIFIdentifier 
  | Stack SIFIdentifier SIFOffset 
  | Register SIFLocation 
  | Type SIFIdentifier SIFSize 
  | Label SIFLocation 
  deriving Show

data SIFType = 
    UnboxInt 
  | UnboxBool
  | BoxInt 
  | BoxBool
  | List 
  | Class SIFIdentifier 
  | Dynamic 
  | Pointer SIFType 
  deriving (Eq, Ord, Show)

data SIFOpcode a = 
    SideEffect (SIFSideEffect a)
  | Unary SIFUnary a {- input -}  SIFType {- return type -} 
  | Binary SIFBinary a {- left input -} a {- right input -} SIFType {- return type -} 
  | Branch (SIFBranch a) a {- target location -} 
  | NOP 
  deriving Show

data SIFSideEffect a = 
    Call a {- location -}
  | Store a {- value -} a {- location -}
  | Move a {- value -} a {- location -}
  | Checkbounds a {- list -} a {- index -}
  | StoreDynamic a {- value -} a {- location -} a {- field -}
  | Write a {- value -}
  | Newline 
  | Enter a {- bytes to allocate for local variables -} 
  | Ret a {- bytes to pop for formal parameters -}
  | Param a {- value to push onto stack -}
  | Entrypc
  deriving Show

data SIFUnary = Neg | Isnull | Load | New | Newlist | Checknull deriving Show
data SIFBinary = Add | Sub | Mul | Div | Mod | Equal | LessEqual | Less | Istype | Checktype | LoadDyanmic deriving Show
data SIFBranch a = Jump | IfZero a {- test -} | IfSet a {- test -} deriving Show

data SIFVarDecl = SIFVarDecl SIFIdentifier SIFSize SIFType deriving (Eq, Ord, Show)
data SIFTypeDecl = SIFTypeDecl SIFIdentifier [SIFVarDecl] deriving Show
data SIFMethodDecl = SIFMethodDecl SIFIdentifier SIFLocation {- entry location -} [SIFVarDecl] deriving (Eq, Ord, Show)
data SIFGlobalDecl = SIFGlobalDecl SIFIdentifier SIFOffset {- offset from GP -} SIFType deriving Show
data SIFInstruction = SIFInstruction SIFLocation (SIFOpcode SIFOperand)  deriving Show
data SIFProgram = SIFProgram [SIFTypeDecl] [SIFMethodDecl] [SIFGlobalDecl] [SIFInstruction] deriving Show

instance InstructionSet SIFInstruction where
  loc (SIFInstruction l _) = l

  isJump (SIFInstruction _ o) = case o of
    Branch _ _ -> True
    _ -> False

  target (SIFInstruction _ (Branch _ (Label l))) = Just l
  target _ = Nothing

  canFall (SIFInstruction _ (Branch Jump _)) = False
  canFall (SIFInstruction _ (SideEffect (Ret _))) = False
  canFall _ = True

  isCall (SIFInstruction _ (SideEffect (Call _))) = True
  isCall _ = False

  callTarget (SIFInstruction _ (SideEffect (Call (Label l)))) = Just l
  callTarget _ = Nothing

  isMain (SIFInstruction _ (SideEffect Entrypc)) = True
  isMain _ = False

  isRet (SIFInstruction _ (SideEffect (Ret _))) = True
  isRet _ = False

  nop n = SIFInstruction n NOP

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

instance Pretty a => Pretty (SIFSideEffect a) where
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

instance Pretty a => Pretty (SIFBranch a) where
  pretty br = case br of
    Jump -> text "br"
    IfZero t -> text "blbc" <+> pretty t
    IfSet t -> text "blbs" <+> pretty t

instance Pretty a => Pretty (SIFOpcode a) where 
  pretty opc = case opc of
    SideEffect se -> pretty se
    Unary uop i t -> pretty uop <+> pretty i <+> colon <> pretty t
    Binary bop i1 i2 t -> pretty bop <+> pretty i1 <+> pretty i2 <+> colon <> pretty t
    Branch br tar -> pretty br <+> pretty tar
    NOP -> text "nop"

instance Pretty SIFVarDecl where
  pretty (SIFVarDecl i s t) = text (i ++ "#") <> integer s <> colon <> pretty t

instance Pretty SIFTypeDecl where
  pretty (SIFTypeDecl i vars) = text ("    type " ++ i) <> colon <+> pretty (Horizontal vars)

instance Pretty SIFMethodDecl where
  pretty (SIFMethodDecl i l params) = text ("    method " ++ i ++ "@") <> integer l <> colon <+> pretty (Horizontal params)

instance Pretty SIFGlobalDecl where
  pretty (SIFGlobalDecl i o t) = text ("    global " ++ i ++ "#") <> integer o <> colon <> pretty t

instance Pretty SIFInstruction where
  pretty (SIFInstruction l op) = text "    instr" <+> integer l <> colon <+> pretty op

instance Pretty SIFProgram where
  pretty (SIFProgram ts ms gs is) = pretty (Vertical ts) $$ pretty (Vertical ms) $$ pretty (Vertical gs) $$ pretty (Vertical is)
