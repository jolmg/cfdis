module CFDI.V3_2.Parser
  ( ParseError(..)
  , Parsed
  , parse
  ) where

import BasicPrelude         (read)
import CFDI.V3_2.Types
import Control.Error.Safe   (justErr)
import Control.Monad        (forM, sequence)
import Data.Maybe           (fromMaybe)
import Data.Text            (Text, pack, unpack)
import Data.Time.Calendar   (Day)
import Data.Time.LocalTime  (LocalTime)
import Data.Time.Format     (defaultTimeLocale, parseTimeM)
import Prelude       hiding (elem, read)
import Text.XML.Light       (parseXMLDoc)
import Text.XML.Light.Lexer (XmlSource)
import Text.XML.Light.Proc  (filterElementName, filterElementsName, findAttrBy)
import Text.XML.Light.Types (Element, QName(QName))

data ParseError
  = AttrNotFound
    { attrName :: String
    }
  | ElemNotFound
    { elemName :: String
    }
  | InvalidFormat
    { formattedName :: String
    }
  | MalformedXML
  | ParseErrorInChild
    { parsedName :: String
    , childErr   :: ParseError
    }
  deriving (Eq, Show)

type Parsed = Either ParseError

parse :: XmlSource s => s -> Parsed CFDI
parse xmlSource = justErr MalformedXML (parseXMLDoc xmlSource) >>= parseCFDI

-- Parsers

parseAddress :: Element -> Parsed Address
parseAddress element = Address
  <$> requireAttrValueByName "pais" element
  <*> parseAttribute "noExterior" element
  <*> parseAttribute "noInterior" element
  <*> parseAttribute "localidad" element
  <*> parseAttribute "municipio" element
  <*> parseAttribute "referencia" element
  <*> parseAttribute "colonia" element
  <*> parseAttribute "estado" element
  <*> parseAttribute "calle" element
  <*> parseAttribute "codigoPostal" element

parseCFDI :: Element -> Parsed CFDI
parseCFDI root = CFDI
  <$> parseAttribute "NumCtaPago" root
  <*> requireAttrValueByName "certificado" root
  <*> requireAttrValueByName "noCertificado" root
  <*> parseChildWith parseComplement "Complemento" root
  <*> do
    conceptsNode <- requireChildByName "Conceptos" root
    wrapError "Conceptos"
      $ parseChildrenWith parseConcept "Concepto" conceptsNode
  <*> parseAttribute "Moneda" root
  <*> parseAttribute "descuento" root
  <*> parseAttribute "motivoDescuento" root
  <*> parseAttribute "TipoCambio" root
  <*> parseAttribute "folio" root
  <*> requireAndParseAttrWith parseDateTime "fecha" root
  <*> requireAttrValueByName "LugarExpedicion" root
  <*> requireAndParseChildWith parseIssuer "Emisor" root
  <*> parseAttribute "MontoFolioFiscalOrig" root
  <*> parseAttributeWith parseDateTime "FechaFolioFiscalOrig" root
  <*> parseAttribute "FolioFiscalOrig" root
  <*> parseAttribute "SerieFolioFiscalOrig" root
  <*> parseAttribute "condicionesDePago" root
  <*> requireAttrValueByName "metodoDePago" root
  <*> requireAndParseChildWith parseRecipient "Receptor" root
  <*> parseAttribute "serie" root
  <*> requireAttrValueByName "subTotal" root
  <*> Right (fromMaybe "" $ findAttrValueByName "sello" root)
  <*> requireAndParseChildWith parseTaxes "Impuestos" root
  <*> requireAttrValueByName "total" root
  <*> requireAttrValueByName "tipoDeComprobante" root
  <*> requireAttrValueByName "version" root
  <*> requireAttrValueByName "formaDePago" root

parseComplement :: Element -> Parsed Complement
parseComplement element = Complement
  <$> parseChildWith parsePacStamp "TimbreFiscalDigital" element

parseConcept :: Element -> Parsed Concept
parseConcept element = Concept
  <$> requireAttrValueByName "importe" element
  <*> requireAttrValueByName "descripcion" element
  <*> parseAttribute "noIdentificacion" element
  <*> parseChildrenWith parseImportInfo "InformacionAduanera" element
  <*> parseChildrenWith parseConceptPart "Parte" element
  <*> parseChildWith parsePropertyAccount "CuentaPredial" element
  <*> requireAttrValueByName "cantidad" element
  <*> requireAttrValueByName "unidad" element
  <*> requireAttrValueByName "valorUnitario" element

parseConceptPart :: Element -> Parsed ConceptPart
parseConceptPart element = ConceptPart
  <$> parseAttribute "importe" element
  <*> requireAttrValueByName "descripcion" element
  <*> parseAttribute "noIdentificacion" element
  <*> parseChildrenWith parseImportInfo "InformacionAduanera" element
  <*> requireAttrValueByName "cantidad" element
  <*> parseAttribute "unidad" element
  <*> parseAttribute "valorUnitario" element

parseFiscalAddress :: Element -> Parsed FiscalAddress
parseFiscalAddress element = FiscalAddress
  <$> requireAttrValueByName "pais" element
  <*> parseAttribute "noExterior" element
  <*> parseAttribute "noInterior" element
  <*> parseAttribute "localidad" element
  <*> requireAttrValueByName "municipio" element
  <*> parseAttribute "referencia" element
  <*> requireAttrValueByName "estado" element
  <*> requireAttrValueByName "calle" element
  <*> parseAttribute "colonia" element
  <*> requireAttrValueByName "codigoPostal" element

parseImportInfo :: Element -> Parsed ImportInfo
parseImportInfo element = ImportInfo
  <$> parseAttribute "aduana" element
  <*> requireAndParseAttrWith parseDate "fecha" element
  <*> requireAttrValueByName "numero" element

parseIssuer :: Element -> Parsed Issuer
parseIssuer element = Issuer
  <$> parseChildWith parseFiscalAddress "DomicilioFiscal" element
  <*> parseChildWith parseAddress "ExpedidoEn" element
  <*> parseAttribute "nombre" element
  <*> parseChildrenWith parseTaxRegime "RegimenFiscal" element
  <*> requireAttrValueByName "rfc" element

parsePacStamp :: Element -> Parsed PacStamp
parsePacStamp element = PacStamp
  <$> requireAttrValueByName "selloCFD" element
  <*> requireAttrValueByName "noCertificadoSAT" element
  <*> requireAttrValueByName "selloSAT" element
  <*> requireAndParseAttrWith parseDateTime "FechaTimbrado" element
  <*> requireAttrValueByName "version" element
  <*> requireAttrValueByName "UUID" element

parsePropertyAccount :: Element -> Parsed PropertyAccount
parsePropertyAccount element = PropertyAccount
  <$> requireAttrValueByName "numero" element

parseRecipient :: Element -> Parsed Recipient
parseRecipient element = Recipient
  <$> parseChildWith parseAddress "Domicilio" element
  <*> parseAttribute "nombre" element
  <*> requireAttrValueByName "rfc" element

parseRetainedTax :: Element -> Parsed RetainedTax
parseRetainedTax element = RetainedTax
  <$> requireAttrValueByName "importe" element
  <*> requireAndReadAttribute "impuesto" element

parseTaxes :: Element -> Parsed Taxes
parseTaxes element = Taxes
  <$> sequence rt
  <*> sequence tt
  <*> parseAttribute "totalImpuestosRetenidos" element
  <*> parseAttribute "totalImpuestosTrasladados" element
  where
    rt = maybe [] (map parseRetainedTax . findChildrenByName "Retencion")
       $ findChildByName "Retenciones" element
    tt = maybe [] (map parseTransferedTax . findChildrenByName "Traslado")
       $ findChildByName "Traslados" element

parseTaxRegime :: Element -> Parsed TaxRegime
parseTaxRegime element = TaxRegime
  <$> requireAttrValueByName "Regimen" element

parseTransferedTax :: Element -> Parsed TransferedTax
parseTransferedTax element = TransferedTax
  <$> requireAttrValueByName "importe" element
  <*> requireAttrValueByName "tasa" element
  <*> requireAndReadAttribute "impuesto" element

-- Helpers

findAttrValueByName :: String -> Element -> Maybe Text
findAttrValueByName _attrName =
  fmap pack . findAttrBy (nameEquals _attrName)

findChildByName :: String -> Element -> Maybe Element
findChildByName childName =
  filterElementName (nameEquals childName)

findChildrenByName :: String -> Element -> [Element]
findChildrenByName childName =
  filterElementsName (nameEquals childName)

nameEquals :: String -> QName -> Bool
nameEquals s (QName _name _ _) =
  s == _name

parseAttribute :: String -> Element -> Parsed (Maybe Text)
parseAttribute _attrName =
  Right . findAttrValueByName _attrName

parseAttributeWith :: (String -> Maybe a) -> String -> Element -> Parsed (Maybe a)
parseAttributeWith parserFunc _attrName elem =
  case findAttrValueByName _attrName elem of
    Nothing -> Right Nothing
    Just x  -> Just <$> justErr (InvalidFormat _attrName) (parserFunc $ unpack x)

parseChildrenWith :: (Element -> Parsed a) -> String -> Element -> Parsed [a]
parseChildrenWith parserFunc childName parent =
  forM children parseOrErr
  where
    children   = findChildrenByName childName parent
    parseOrErr = wrapError childName . parserFunc

parseChildWith :: (Element -> Parsed a) -> String -> Element -> Parsed (Maybe a)
parseChildWith parserFunc childName parent =
  case findChildByName childName parent of
    Nothing -> Right Nothing
    Just x  -> Just <$> wrapError childName (parserFunc x)

parseDate :: String -> Maybe Day
parseDate =
  parseTimeM True defaultTimeLocale "%Y-%m-%d"

parseDateTime :: String -> Maybe LocalTime
parseDateTime =
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

requireAndParseAttrWith :: (String -> Maybe a) -> String -> Element -> Parsed a
requireAndParseAttrWith parserFunc _attrName elem =
  requireAttrValueByName _attrName elem
    >>= justErr (InvalidFormat _attrName) . parserFunc . unpack

requireAndParseChildWith :: (Element -> Parsed a) -> String -> Element -> Parsed a
requireAndParseChildWith parserFunc childName parent =
  requireChildByName childName parent
    >>= wrapError childName . parserFunc

requireAndReadAttribute :: Read r => String -> Element -> Parsed r
requireAndReadAttribute _attrName =
  fmap read . requireAttrValueByName _attrName

requireAttrValueByName :: String -> Element -> Parsed Text
requireAttrValueByName _attrName =
  justErr (AttrNotFound _attrName) . findAttrValueByName _attrName

requireChildByName :: String -> Element -> Parsed Element
requireChildByName childName =
  justErr (ElemNotFound childName) . findChildByName childName

wrapError :: String -> Parsed a -> Parsed a
wrapError childName (Left err) = Left $ ParseErrorInChild childName err
wrapError _ x = x
