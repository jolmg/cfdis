{-# LANGUAGE OverloadedStrings #-}

module CFDI.Catalogs where

import Data.Text (Text, pack, unpack)
import Text.Read (readMaybe)

class Catalog c where
  fromCode :: Text -> Maybe c

  toCode :: c -> Text

data CfdiType
  = Income
  | Outcome
  | Transfer
  | Paysheet
  | Payment
  deriving (Eq, Show)

instance Catalog CfdiType where
  fromCode "I" = Just Income
  fromCode "E" = Just Outcome
  fromCode "T" = Just Transfer
  fromCode "N" = Just Paysheet
  fromCode "P" = Just Payment
  fromCode _   = Nothing

  toCode Income   = "I"
  toCode Outcome  = "E"
  toCode Transfer = "T"
  toCode Paysheet = "N"
  toCode Payment  = "P"

data Country
  = ABW
  | AFG
  | AGO
  | AIA
  | ALA
  | ALB
  | AND
  | ARE
  | ARG
  | ARM
  | ASM
  | ATA
  | ATF
  | ATG
  | AUS
  | AUT
  | AZE
  | BDI
  | BEL
  | BEN
  | BES
  | BFA
  | BGD
  | BGR
  | BHR
  | BHS
  | BIH
  | BLM
  | BLR
  | BLZ
  | BMU
  | BOL
  | BRA
  | BRB
  | BRN
  | BTN
  | BVT
  | BWA
  | CAF
  | CAN
  | CCK
  | CHE
  | CHL
  | CHN
  | CIV
  | CMR
  | COD
  | COG
  | COK
  | COL
  | COM
  | CPV
  | CRI
  | CUB
  | CUW
  | CXR
  | CYM
  | CYP
  | CZE
  | DEU
  | DJI
  | DMA
  | DNK
  | DOM
  | DZA
  | ECU
  | EGY
  | ERI
  | ESH
  | ESP
  | EST
  | ETH
  | FIN
  | FJI
  | FLK
  | FRA
  | FRO
  | FSM
  | GAB
  | GBR
  | GEO
  | GGY
  | GHA
  | GIB
  | GIN
  | GLP
  | GMB
  | GNB
  | GNQ
  | GRC
  | GRD
  | GRL
  | GTM
  | GUF
  | GUM
  | GUY
  | HKG
  | HMD
  | HND
  | HRV
  | HTI
  | HUN
  | IDN
  | IMN
  | IND
  | IOT
  | IRL
  | IRN
  | IRQ
  | ISL
  | ISR
  | ITA
  | JAM
  | JEY
  | JOR
  | JPN
  | KAZ
  | KEN
  | KGZ
  | KHM
  | KIR
  | KNA
  | KOR
  | KWT
  | LAO
  | LBN
  | LBR
  | LBY
  | LCA
  | LIE
  | LKA
  | LSO
  | LTU
  | LUX
  | LVA
  | MAC
  | MAF
  | MAR
  | MCO
  | MDA
  | MDG
  | MDV
  | MEX
  | MHL
  | MKD
  | MLI
  | MLT
  | MMR
  | MNE
  | MNG
  | MNP
  | MOZ
  | MRT
  | MSR
  | MTQ
  | MUS
  | MWI
  | MYS
  | MYT
  | NAM
  | NCL
  | NER
  | NFK
  | NGA
  | NIC
  | NIU
  | NLD
  | NOR
  | NPL
  | NRU
  | NZL
  | OMN
  | PAK
  | PAN
  | PCN
  | PER
  | PHL
  | PLW
  | PNG
  | POL
  | PRI
  | PRK
  | PRT
  | PRY
  | PSE
  | PYF
  | QAT
  | REU
  | ROU
  | RUS
  | RWA
  | SAU
  | SDN
  | SEN
  | SGP
  | SGS
  | SHN
  | SJM
  | SLB
  | SLE
  | SLV
  | SMR
  | SOM
  | SPM
  | SRB
  | SSD
  | STP
  | SUR
  | SVK
  | SVN
  | SWE
  | SWZ
  | SXM
  | SYC
  | SYR
  | TCA
  | TCD
  | TGO
  | THA
  | TJK
  | TKL
  | TKM
  | TLS
  | TON
  | TTO
  | TUN
  | TUR
  | TUV
  | TWN
  | TZA
  | UGA
  | UKR
  | UMI
  | URY
  | USA
  | UZB
  | VAT
  | VCT
  | VEN
  | VGB
  | VIR
  | VNM
  | VUT
  | WLF
  | WSM
  | YEM
  | ZAF
  | ZMB
  | ZWE
  | ZZZ
  deriving (Eq, Read, Show)

instance Catalog Country where
  fromCode = readMaybe . unpack

  toCode = pack . show

data Currency
  = AED
  | AFN
  | ALL
  | AMD
  | ANG
  | AOA
  | ARS
  | AUD
  | AWG
  | AZN
  | BAM
  | BBD
  | BDT
  | BGN
  | BHD
  | BIF
  | BMD
  | BND
  | BOB
  | BOV
  | BRL
  | BSD
  | BWP
  | BYN
  | BZD
  | CAD
  | CDF
  | CHF
  | CHW
  | CLF
  | CLP
  | CNY
  | COP
  | COU
  | CRC
  | CUC
  | CUP
  | CVE
  | CZK
  | DJF
  | DKK
  | DOP
  | DZD
  | EGP
  | ERN
  | ETB
  | EUR
  | FJD
  | FKP
  | GBP
  | GEL
  | GHS
  | GIP
  | GMD
  | GNF
  | GTQ
  | GYD
  | HKD
  | HNL
  | HRK
  | HTG
  | HUF
  | IDR
  | ILS
  | INR
  | IQD
  | IRR
  | ISK
  | JMD
  | JOD
  | JPY
  | KES
  | KGS
  | KHR
  | KMF
  | KPW
  | KRW
  | KWD
  | KYD
  | KZT
  | LAK
  | LBP
  | LKR
  | LRD
  | LSL
  | LYD
  | MAD
  | MDL
  | MGA
  | MMK
  | MNT
  | MOP
  | MRO
  | MUR
  | MVR
  | MWK
  | MXN
  | MXV
  | MYR
  | MZN
  | NAD
  | NGN
  | NIO
  | NOK
  | NPR
  | NZD
  | OMR
  | PAB
  | PEN
  | PGK
  | PHP
  | PKR
  | PLN
  | PYG
  | QAR
  | RON
  | RSD
  | RUB
  | RWF
  | SAR
  | SBD
  | SCR
  | SDG
  | SEK
  | SGD
  | SHP
  | SLL
  | SOS
  | SRD
  | SSP
  | STD
  | SVC
  | SYP
  | SZL
  | THB
  | TJS
  | TMT
  | TND
  | TOP
  | TRY
  | TTD
  | TWD
  | TZS
  | UAH
  | UGX
  | USD
  | USN
  | UYI
  | UYU
  | UZS
  | VEF
  | VND
  | VUV
  | WST
  | XAF
  | XAG
  | XAU
  | XBA
  | XBB
  | XBC
  | XBD
  | XCD
  | XDR
  | XOF
  | XPD
  | XPF
  | XPT
  | XSU
  | XTS
  | XUA
  | XXX
  | YER
  | ZAR
  | ZMW
  | ZWL
  deriving (Eq, Read, Show)

instance Catalog Currency where
  fromCode = readMaybe . unpack

  toCode = pack . show

data PaymentMethod
  = OneTimePayment
  | PartialPayment

instance Catalog PaymentMethod where
  fromCode "PUE" = Just OneTimePayment
  fromCode "PPD" = Just PartialPayment
  fromCode _     = Nothing

  toCode OneTimePayment = "PUE"
  toCode PartialPayment = "PPD"

data RelationshipType
  = CreditNote
  | DebitNote
  | ReturnedGoods
  | Substitution
  | PrevInvoicedTransfer
  | PrevTransferedInvoice
  | AdvanceApplication
  deriving (Eq, Show)

instance Catalog RelationshipType where
  fromCode "01" = Just CreditNote
  fromCode "02" = Just DebitNote
  fromCode "03" = Just ReturnedGoods
  fromCode "04" = Just Substitution
  fromCode "05" = Just PrevInvoicedTransfer
  fromCode "06" = Just PrevTransferedInvoice
  fromCode "07" = Just AdvanceApplication
  fromCode _    = Nothing

  toCode CreditNote            = "01"
  toCode DebitNote             = "02"
  toCode ReturnedGoods         = "03"
  toCode Substitution          = "04"
  toCode PrevInvoicedTransfer  = "05"
  toCode PrevTransferedInvoice = "06"
  toCode AdvanceApplication    = "07"

data TaxRegime
  = GeneralForPeople
  | NonProfitCompany
  | WagesAndSalaries
  | Leasing
  | GoodsAcquisition
  | OtherIncomes
  | Consolidation
  | ForeignResidents
  | DividendIncomes
  | PeopleWithBusinessActivities
  | InterestsIncomes
  | RevenueForPrizes
  | NoTaxObligations
  | CooperativeSocieties
  | FiscalIncorporation
  | AgriculturalActivities
  | OptionalForSocialGroups
  | Coordinated
  | Hydrocarbons
  | MultinationalCompanies
  | SharesAlienation
  deriving (Eq, Show)

instance Catalog TaxRegime where
  fromCode "600" = Just GeneralForPeople
  fromCode "602" = Just NonProfitCompany
  fromCode "605" = Just WagesAndSalaries
  fromCode "606" = Just Leasing
  fromCode "607" = Just GoodsAcquisition
  fromCode "608" = Just OtherIncomes
  fromCode "609" = Just Consolidation
  fromCode "610" = Just ForeignResidents
  fromCode "611" = Just DividendIncomes
  fromCode "612" = Just PeopleWithBusinessActivities
  fromCode "614" = Just InterestsIncomes
  fromCode "615" = Just RevenueForPrizes
  fromCode "616" = Just NoTaxObligations
  fromCode "620" = Just CooperativeSocieties
  fromCode "621" = Just FiscalIncorporation
  fromCode "622" = Just AgriculturalActivities
  fromCode "623" = Just OptionalForSocialGroups
  fromCode "624" = Just Coordinated
  fromCode "628" = Just Hydrocarbons
  fromCode "629" = Just MultinationalCompanies
  fromCode "630" = Just SharesAlienation
  fromCode _     = Nothing

  toCode GeneralForPeople             = "600"
  toCode NonProfitCompany             = "602"
  toCode WagesAndSalaries             = "605"
  toCode Leasing                      = "606"
  toCode GoodsAcquisition             = "607"
  toCode OtherIncomes                 = "608"
  toCode Consolidation                = "609"
  toCode ForeignResidents             = "610"
  toCode DividendIncomes              = "611"
  toCode PeopleWithBusinessActivities = "612"
  toCode InterestsIncomes             = "614"
  toCode RevenueForPrizes             = "615"
  toCode NoTaxObligations             = "616"
  toCode CooperativeSocieties         = "620"
  toCode FiscalIncorporation          = "621"
  toCode AgriculturalActivities       = "622"
  toCode OptionalForSocialGroups      = "623"
  toCode Coordinated                  = "624"
  toCode Hydrocarbons                 = "628"
  toCode MultinationalCompanies       = "629"
  toCode SharesAlienation             = "630"

data Use
  = GoodsAcquisition_
  | ReturnsDiscountsOrBonuses
  | GeneralExpenses
  | Constructions
  | Furniture
  | TransportEquipment
  | ComputerEquipment
  | Tooling
  | PhoneComunications
  | SatelliteComunications
  | OtherMachinery
  | MedicalFees
  | MedicalExpenses
  | FuneralExpenses
  | Donations
  | PaidInterests
  | VoluntaryContributions
  | InsurancePremiums
  | SchoolTransportation
  | DepositsInSavingsAccounts
  | TuitionFees
  | ToBeDefined_
  deriving (Eq, Show)

instance Catalog Use where
  fromCode "G01" = Just GoodsAcquisition_
  fromCode "G02" = Just ReturnsDiscountsOrBonuses
  fromCode "G03" = Just GeneralExpenses
  fromCode "I01" = Just Constructions
  fromCode "I02" = Just Furniture
  fromCode "I03" = Just TransportEquipment
  fromCode "I04" = Just ComputerEquipment
  fromCode "I05" = Just Tooling
  fromCode "I06" = Just PhoneComunications
  fromCode "I07" = Just SatelliteComunications
  fromCode "I08" = Just OtherMachinery
  fromCode "D01" = Just MedicalFees
  fromCode "D02" = Just MedicalExpenses
  fromCode "D03" = Just FuneralExpenses
  fromCode "D04" = Just Donations
  fromCode "D05" = Just PaidInterests
  fromCode "D06" = Just VoluntaryContributions
  fromCode "D07" = Just InsurancePremiums
  fromCode "D08" = Just SchoolTransportation
  fromCode "D09" = Just DepositsInSavingsAccounts
  fromCode "D10" = Just TuitionFees
  fromCode "P01" = Just ToBeDefined_
  fromCode _     = Nothing

  toCode GoodsAcquisition_         = "G01"
  toCode ReturnsDiscountsOrBonuses = "G02"
  toCode GeneralExpenses           = "G03"
  toCode Constructions             = "I01"
  toCode Furniture                 = "I02"
  toCode TransportEquipment        = "I03"
  toCode ComputerEquipment         = "I04"
  toCode Tooling                   = "I05"
  toCode PhoneComunications        = "I06"
  toCode SatelliteComunications    = "I07"
  toCode OtherMachinery            = "I08"
  toCode MedicalFees               = "D01"
  toCode MedicalExpenses           = "D02"
  toCode FuneralExpenses           = "D03"
  toCode Donations                 = "D04"
  toCode PaidInterests             = "D05"
  toCode VoluntaryContributions    = "D06"
  toCode InsurancePremiums         = "D07"
  toCode SchoolTransportation      = "D08"
  toCode DepositsInSavingsAccounts = "D09"
  toCode TuitionFees               = "D10"
  toCode ToBeDefined_              = "P01"

data WayToPay
  = Cash
  | NominalCheck
  | ElectronicTransfer
  | CreditCard
  | ElectronicPurse
  | ElectronicCash
  | PantryCoupons
  | PaymentIn
  | Subrogation
  | Consignment
  | Condonation
  | Compensation
  | Novation
  | Confusion
  | DebtReferral
  | PrescriptionOrExpiration
  | ToTheSatisfactionOfTheCreditor
  | DebitCard
  | ServiceCard
  | AdvancesApplication
  | ToBeDefined
  deriving (Eq, Show)

instance Catalog WayToPay where
  fromCode "01" = Just Cash
  fromCode "02" = Just NominalCheck
  fromCode "03" = Just ElectronicTransfer
  fromCode "04" = Just CreditCard
  fromCode "05" = Just ElectronicPurse
  fromCode "06" = Just ElectronicCash
  fromCode "08" = Just PantryCoupons
  fromCode "12" = Just PaymentIn
  fromCode "13" = Just Subrogation
  fromCode "14" = Just Consignment
  fromCode "15" = Just Condonation
  fromCode "17" = Just Compensation
  fromCode "23" = Just Novation
  fromCode "24" = Just Confusion
  fromCode "25" = Just DebtReferral
  fromCode "26" = Just PrescriptionOrExpiration
  fromCode "27" = Just ToTheSatisfactionOfTheCreditor
  fromCode "28" = Just DebitCard
  fromCode "29" = Just ServiceCard
  fromCode "30" = Just AdvancesApplication
  fromCode "99" = Just ToBeDefined
  fromCode _    = Nothing

  toCode Cash                           = "01"
  toCode NominalCheck                   = "02"
  toCode ElectronicTransfer             = "03"
  toCode CreditCard                     = "04"
  toCode ElectronicPurse                = "05"
  toCode ElectronicCash                 = "06"
  toCode PantryCoupons                  = "08"
  toCode PaymentIn                      = "12"
  toCode Subrogation                    = "13"
  toCode Consignment                    = "14"
  toCode Condonation                    = "15"
  toCode Compensation                   = "17"
  toCode Novation                       = "23"
  toCode Confusion                      = "24"
  toCode DebtReferral                   = "25"
  toCode PrescriptionOrExpiration       = "26"
  toCode ToTheSatisfactionOfTheCreditor = "27"
  toCode DebitCard                      = "28"
  toCode ServiceCard                    = "29"
  toCode AdvancesApplication            = "30"
  toCode ToBeDefined                    = "99"

data ZipCode = ZipCode
  { zipCode :: Int
  } deriving (Eq, Show)

instance Catalog ZipCode where
  fromCode c = readMaybe (unpack c) >>= isValid >>= return . ZipCode
    where
      isValid x
        | x > 0     && x <  1000 = Nothing
        | x > 16999 && x < 20000 = Nothing
        | x == 20640             = Nothing
        | x > 28469 && x < 28500 = Nothing
        | x > 52799 && x < 52900 = Nothing
        | x > 54499 && x < 54539 = Nothing
        | x > 54539 && x < 54570 = Nothing
        | x > 68929 && x < 68940 = Nothing
        | x > 90750 && x < 90754 = Nothing
        | x > 90754 && x < 90760 = Nothing
        | x > 91310 && x < 91315 = Nothing
        | x > 93769 && x < 93780 = Nothing
        | x > 95049 && x < 95670 = Nothing
        | otherwise = Just x

  toCode (ZipCode x) = pack $ replicate (5 - length xStr) '0' ++ xStr
    where
      xStr = show x
