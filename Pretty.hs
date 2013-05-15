module Pretty where

import Text.PrettyPrint.HughesPJ

class Pretty a where
  pretty :: a -> Doc
