module Pretty where

import Text.PrettyPrint.HughesPJ

class Pretty a where
  pretty :: a -> Doc

newtype Vertical x = Vertical [x]
newtype Horizontal x = Horizontal [x]

instance Pretty x => Pretty (Vertical x) where
  pretty (Vertical xs) = vcat $ map pretty xs

instance Pretty x => Pretty (Horizontal x) where
  pretty (Horizontal xs) = hsep $ map pretty xs
