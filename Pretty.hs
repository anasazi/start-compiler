module Pretty where

import Text.PrettyPrint.HughesPJ
import Data.Char (toLower)
import IR

class Pretty a where
    pretty :: a -> Doc

instance Pretty ConstOperand where
    pretty GP = text "GP"
    pretty FP = text "FP"
    pretty (C n) = integer n
    pretty (R r) = parens . integer $ r
    pretty (T t s) = text (t ++ "_type#") <> integer s
    pretty (L l) = brackets . integer $ l

    pretty (A v o) = text (v ++ "_base#") <> integer o
    pretty (SF v o) = text (v ++ "_offset#") <> integer o
    pretty (DF v) = text (v ++ "_offset#?")

instance Pretty VarOperand where
{-
    pretty (A v o) = text (v ++ "_base#") <> integer o
    pretty (SF v o) = text (v ++ "_offset#") <> integer o
    pretty (DF v) = text (v ++ "_offset#?")
-}
    pretty (SV v o) = text (v ++ "#") <> integer o

newtype SSAVarOperand = SSAVarOperand (VarOperand, Integer)

instance Pretty SSAVarOperand where
{-
    pretty (SSAVarOperand ((A v _),s))  = text (v ++ "_base$") <> integer s
    pretty (SSAVarOperand ((SF v _),s)) = text (v ++ "_offset$") <> integer s
    pretty (SSAVarOperand ((DF v),s))   = text (v ++ "_offset$") <> integer s
-}
    pretty (SSAVarOperand ((SV v _),s)) = text (v ++ "$") <> integer s

instance Pretty Operand where
    pretty (Const co) = pretty co
    pretty (Var vo) = pretty vo
    pretty (SSAVar vo sub) = pretty . SSAVarOperand $ (vo, sub)

instance Pretty ZOp where
    pretty = text . map toLower . show

instance Pretty UOp where
    pretty = text . map toLower . show

instance Pretty BOp where
    pretty = text . map toLower . show

instance Pretty TOp where
    pretty = text . map toLower . show

instance Pretty Opcode where
    --pretty (Phi v subs) = text "phi" <+> (hsep . map pretty . map (\(_,s) -> SSAVarOperand (v, s)) $ subs)
    -- TODO remove this debug printing
    pretty (Phi v subs) = text "phi" <+> (hsep . map (\(n,s) -> parens (integer n <> comma <> pretty (SSAVarOperand (v, s)))) $ subs)
    pretty (Z z) = pretty z
    pretty (U u x) = pretty u <+> pretty x
    pretty (B b x y) = pretty b <+> pretty x <+> pretty y
    pretty (Ter t x y z) = pretty t <+> pretty x <+> pretty y <+> pretty z

instance Pretty Type where
    pretty UInt = text "int"
    pretty UBool = text "bool"
    pretty BInt = text "Integer"
    pretty BBool = text "Boolean"
    pretty List = text "List"
    pretty (Class t) = text t
    pretty Dynamic = text "dynamic"
    pretty (Pointer t) = pretty t <> char '*'

instance Pretty UserType where
    pretty (UserType n vs) = text ("    type " ++ n) <> colon <+> vars
	where vars = hsep . map (\(n,s,t) -> text (n ++ "#") <> integer s <> colon <> pretty t) $ vs

instance Pretty Method where
    pretty (Method n l ps) = text ("    method " ++ n ++ "@") <> integer l <> colon <+> params
	where params = hsep . map (\(n,s,t) -> text (n ++ "#") <> integer s <> colon <> pretty t) $ ps

instance Pretty Global where
    pretty (Global n s t) = text ("    global " ++ n ++ "#") <> integer s <> colon <> pretty t

instance Pretty Instruction where
    pretty (Instruction n op mt) = text "    instr" <+> integer n <> colon <+> pretty op <+> pt
	where pt = case mt of
		    (Just t) -> colon <> pretty t
		    Nothing -> empty

instance (Pretty a) => Pretty [a] where
    pretty = vcat . map pretty

instance Pretty Program where
    pretty (Program uts ms gs is) = pretty uts $$ pretty ms $$ pretty gs $$ pretty is
