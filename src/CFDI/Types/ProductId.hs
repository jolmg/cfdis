module CFDI.Types.ProductId where

import CFDI.Chainable
import CFDI.Types.Type
import Data.Text        (Text, pack, unpack)
import Text.Regex       (mkRegex)
import Text.Regex.Posix (matchTest)

newtype ProductId = ProductId Text deriving (Eq, Show)

instance Chainable ProductId where
  chain (ProductId i) = i

instance Type ProductId where
  parseExpr str
    | matchTest regExp str = Right . ProductId $ pack str
    | otherwise = Left $ DoesNotMatchExpr "[^|]{1,100}"
    where
      regExp = mkRegex "^(.|á|é|í|ó|ú|ñ|Á|É|Í|Ó|Ú|Ñ){1,100}$"

  render (ProductId i) = unpack i
