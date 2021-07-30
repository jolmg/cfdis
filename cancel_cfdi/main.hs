{-# LANGUAGE OverloadedStrings #-}

import CFDI (UUID(..))
import CFDI.PAC (ppCancelError, cancelCFDI)
import CFDI.PAC.Dummy (Dummy(..))
import CFDI.PAC.Fel (Fel(Fel), FelEnv(..))
import CFDI.PAC.ITimbre (ITimbre(ITimbre), ITimbreEnv(..))
import qualified Data.ByteString.Char8 as C8 (putStrLn)
import Data.Char (toLower)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import System.Environment (getArgs, getEnv, lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs

  case args of
    (pacName : pfxPemPath : pfxPass : uuidStr : _) -> do
      environment <- lookupEnv "CFDI_ENVIRONMENT"

      pfxPem <- readFile pfxPemPath

      let uuid = UUID $ pack uuidStr
          isTest = (map toLower <$> environment) == Just "test"

      case map toLower pacName of
        "itimbre" -> do
          user <- getEnv "CFDI_ITIMBRE_USER"
          pass <- getEnv "CFDI_ITIMBRE_PASS"
          rfc <- getEnv "CFDI_ITIMBRE_RFC"

          let pac = ITimbre
                      (pack user)
                      (pack pass)
                      (pack rfc)
                      (pack pfxPem)
                      (pack pfxPass)
                      (if isTest then Testing else Production)

          eitherErrOrAck <- cancelCFDI pac uuid

          case eitherErrOrAck of
            Left cancelError -> do
              hPutStrLn stderr $ ppCancelError cancelError
              exitWith $ ExitFailure 4

            Right ack -> do
              C8.putStrLn $ encodeUtf8 ack

        "fel" -> do
          user <- getEnv "CFDI_FEL_USER"
          pass <- getEnv "CFDI_FEL_PASS"
          rfc <- getEnv "CFDI_FEL_RFC"

          let pac = Fel
                      (pack user)
                      (pack pass)
                      (pack rfc)
                      (pack pfxPem)
                      (pack pfxPass)
                      (if isTest then FelTestingEnv else FelProductionEnv)

          eitherErrOrAck <- cancelCFDI pac uuid

          case eitherErrOrAck of
            Left cancelError -> do
              hPutStrLn stderr $ ppCancelError cancelError
              exitWith $ ExitFailure 4

            Right ack -> do
              C8.putStrLn $ encodeUtf8 ack

        "dummy" -> do
          eitherErrOrAck <- cancelCFDI Dummy uuid

          case eitherErrOrAck of
            Left cancelError -> do
              hPutStrLn stderr $ ppCancelError cancelError
              exitWith $ ExitFailure 4

            Right ack -> do
              C8.putStrLn $ encodeUtf8 ack

        unknownPac -> do
          hPutStrLn stderr $  "Unknown PAC " ++ unknownPac
          exitWith $ ExitFailure 3

    _ -> do
      hPutStrLn stderr $ "Usage: cfdi_cancel [PAC name] [CFDI UUID]"
      exitWith $ ExitFailure 2
