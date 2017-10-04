module CFDI.Types.Concept where

import CFDI.Types.Amount
import CFDI.Types.ConceptTaxes
import CFDI.Types.CustomInfo
import CFDI.Types.MeasurementUnit
import CFDI.Types.ProductDescription
import CFDI.Types.ProductId
import CFDI.Types.ProductOrService
import CFDI.Types.ProductUnit
import CFDI.Types.Quantity
import CFDI.XmlNode
import Data.Maybe                    (catMaybes)

data Concept = Concept
  { conAmount     :: Amount
  , conCustomInfo :: [CustomInfo]
  , conDesc       :: ProductDescription
  , conDiscount   :: Maybe Amount
  , conMeasUnit   :: MeasurementUnit
  , conProdId     :: Maybe ProductId
  , conProdServ   :: ProductOrService
  , conQuantity   :: Quantity
  , conTaxes      :: Maybe ConceptTaxes
  , conUnit       :: Maybe ProductUnit
  , conUnitPrice  :: Amount
  } deriving (Eq, Show)

instance XmlNode Concept where
  children n = catMaybes [renderNode <$> conTaxes n]
            ++ map renderNode (conCustomInfo n)

  nodeName = const "Concepto"

  optionalAttributes n =
    [ attr "Descuento"        <$> conDiscount n
    , attr "NoIdentificacion" <$> conProdId n
    , attr "Unidad"           <$> conUnit n
    ]

  parseNode n = Concept
    <$> requireAttribute "Importe" n
    <*> parseChildren "InformacionAduanera" n
    <*> requireAttribute "Descripcion" n
    <*> parseAttribute "Descuento" n
    <*> requireAttribute "ClaveUnidad" n
    <*> parseAttribute "NoIdentificacion" n
    <*> requireAttribute "ClaveProdServ" n
    <*> requireAttribute "Cantidad" n
    <*> parseChild "Impuestos" n
    <*> parseAttribute "Unidad" n
    <*> requireAttribute "ValorUnitario" n

  requiredAttributes n =
    [ attr "Importe"       $ conAmount n
    , attr "Descripcion"   $ conDesc n
    , attr "ClaveUnidad"   $ conMeasUnit n
    , attr "ClaveProdServ" $ conProdServ n
    , attr "Cantidad"      $ conQuantity n
    , attr "ValorUnitario" $ conUnitPrice n
    ]
