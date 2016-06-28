{-# LANGUAGE DeriveGeneric, DataKinds, OverloadedStrings, TypeOperators #-}

module Htrans.Types (
 --types
  Args(..),
  LogLevel(..),
  Config(..)

) where

import Options.Generic
import Network.Yandex.Translate
import qualified Data.Text as T

data Args = Args
  {
    text   :: Maybe T.Text    <?> "Text to translate."
  , from   :: Maybe Language  <?> "From language translate (default:English 'en') format ru,en etc."
  , to     :: Maybe Language  <?> "To language translate (default:Russian 'ru') format ru,en etc."
  , key    :: APIKey          <?> "Key to access Yandex service (required)"
  , logs   :: Maybe LogLevel  <?> "Log level on stderr (default:EME - emergency) avialable:\
                                  \EME - emergency, ERR - error, INF - info,DEB -debug.\
                                  \ To save log in file you can use stdout (>>out) or stderr (2>>err)"
  , path   :: Maybe FilePath  <?> "Path and name to log file (default: ./htrans.log)"
  , notify :: Maybe Bool      <?> "Show result on X screen (use libnotify) or only console (default:False)"

  } deriving (Generic, Show)

instance ParseRecord Args
instance ParseField  LogLevel
instance ParseFields LogLevel
instance ParseRecord LogLevel

data LogLevel = EME
              | ERR
              | INF
              | DEB
  deriving (Read, Show, Generic)

data Config = Config
    {
     textToTranslate :: Maybe T.Text
   , fromLang        :: Language
   , toLang          :: Language
   , keyAPI          :: APIKey
   , logLevel        :: LogLevel
   , logPath         :: FilePath
   , onScreen        :: Bool

   } deriving Show
