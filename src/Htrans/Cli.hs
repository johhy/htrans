{-# LANGUAGE DeriveGeneric, DataKinds, OverloadedStrings, TypeOperators #-}

module Htrans.Cli (
  -- funcs
  cli,
  appName

) where

import Network.Yandex.Translate
import qualified Data.Text as T
import Paths_htrans (version)
import Data.Version (showVersion)
import Options.Generic
import Data.Maybe
import Htrans.Types


appName :: String
appName = "htranslator"

getAppVersion :: T.Text 
getAppVersion  = T.pack $ showVersion version

defaultLangFrom :: Language
defaultLangFrom = "en"

defaultLangTo   :: Language
defaultLangTo   = "ru"

defaultLogLevel :: LogLevel
defaultLogLevel =  EME

defaultLogPath  :: FilePath
defaultLogPath  =  "./" ++ appName ++ ".log"

defaultOnScreen :: Bool
defaultOnScreen =  False

cli :: IO Config
cli = do
  x <- getRecord $ T.append "Translate text from one language to another\
          \ using Yandex Translate API ********\
          \ translator - Yandex Translate API console tool version: "  getAppVersion

  return Config {
    textToTranslate = unHelpful $ text x,
    keyAPI          = unHelpful $ key x,
    fromLang        = fromMaybe defaultLangFrom $ unHelpful $ from x,
    toLang          = fromMaybe defaultLangTo   $ unHelpful $ to x,
    logLevel        = fromMaybe defaultLogLevel $ unHelpful $ logs x,
    logPath         = fromMaybe defaultLogPath  $ unHelpful $ path x,
    onScreen        = fromMaybe defaultOnScreen $ unHelpful $ notify x
   } 


