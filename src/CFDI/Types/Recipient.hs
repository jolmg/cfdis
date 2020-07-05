module CFDI.Types.Recipient where

import CFDI.Chainable
import CFDI.Types.Country
import CFDI.Types.Name
import CFDI.Types.RFC
import CFDI.Types.TaxId
import CFDI.Types.Use
import CFDI.XmlNode
import Data.Maybe         (catMaybes)

data Recipient = Recipient
  { recCfdiUse   :: Maybe Use
  , recName      :: Maybe Name
  , recRfc       :: RFC
  , recTaxId     :: Maybe TaxId
  , taxResidence :: Maybe Country
  } deriving (Eq, Show)

instance Chainable Recipient where
  chain c = recRfc
        <@> recName
        <~> taxResidence
        <~> recTaxId
        <~> recCfdiUse
        <~> (c, "")

instance XmlNode Recipient where
  attributes n = attr "Rfc" (recRfc n)
    : catMaybes
      [ attr "UsoCFDI"          <$> recCfdiUse n
      , attr "Nombre"           <$> recName n
      , attr "ResidenciaFiscal" <$> taxResidence n
      , attr "NumRegIdTrib"     <$> recTaxId n
      ]

  nodeName = const "Receptor"

  parseNode n = Recipient
    <$> parseAttribute "UsoCFDI" n
    <*> parseAttribute "Nombre" n
    <*> requireAttribute "Rfc" n
    <*> parseAttribute "NumRegIdTrib" n
    <*> parseAttribute "ResidenciaFiscal" n
