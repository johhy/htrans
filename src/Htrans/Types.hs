{-# LANGUAGE DeriveGeneric, DataKinds, OverloadedStrings, TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Htrans.Types (
 --types
  Args(..),
  LogLevel(..),
  Config(..),
  Lang(..),
 --funcs
  convertLang

) where

import Options.Generic
import Network.Yandex.Translate
import qualified Data.Text as T

data Args = Args
  {
    text   :: Maybe T.Text    <?> "Text to translate."
  , from   :: Maybe Lang      <?> "From language translate (default:English EN) see key --to"
  , to     :: Maybe Lang      <?> "To language translate (default:Russian RU)\
                                  \availables: RU,EN,DE,FR,NO,PL,PT,BG,RO,NL,EL, \
                                  \DA,HE,TR,IT,UK,ES,FI,ZH,SV,JA - \
              \see https://tech.yandex.ru/translate/doc/dg/concepts/api-overview-docpage/#languages"
  , key    :: APIKey          <?> "Key to access Yandex service (required)"
  , logs   :: Maybe LogLevel  <?> "Log level on stderr (default:EME - emergency) availables:\
                                  \EME - emergency, ERR - error, INF - info,DEB -debug.\
                                  \ To save log in file you can use stdout (>>out) or stderr (2>>err)"
  , path   :: Maybe FilePath  <?> "Path and name to log file (default: ./htrans.log)"
  , notify :: Maybe Bool      <?> "Show result on X screen (use libnotify) or only console (default:False)"

  } deriving (Generic, Show)

instance ParseRecord Args

data LogLevel = EME
              | ERR
              | INF
              | DEB
  deriving (Read, Show, Generic)

instance ParseField  LogLevel
instance ParseFields LogLevel
instance ParseRecord LogLevel

data Lang = RU | EN | DE | FR | NO | PL | PT | BG | RO | NL | EL
          | DA | HE | TR | IT | UK | ES | FI | ZH | SV | JA
  deriving (Read, Show, Generic)

instance ParseField  Lang
instance ParseFields Lang
instance ParseRecord Lang

data Config = Config
    {
     textToTranslate :: Maybe T.Text
   , fromLang        :: Lang
   , toLang          :: Lang
   , keyAPI          :: APIKey
   , logLevel        :: LogLevel
   , logPath         :: FilePath
   , onScreen        :: Bool

   } deriving Show

convertLang :: Lang -> Language
convertLang x = T.toLower $ T.pack $ show x
