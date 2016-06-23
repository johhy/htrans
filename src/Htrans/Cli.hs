{-# LANGUAGE OverloadedStrings #-}

module Htrans.Cli (
  --types
  Config(..),
  -- funcs
  cli

) where

import Network.Yandex.Translate
import qualified Data.Text as T
import Options.Applicative (Parser, execParser, value, fullDesc,
                            header, help, helper, info, long, metavar,
                            option, progDesc,short, str,
                            (<>))
import Htrans.Logger (Priority(..))
import Data.Char (toLower)

data Config = Config
  {
    text       :: [T.Text]
  , from       :: Language
  , to         :: Language
  , key        :: APIKey
  , logLevel   :: Priority
  , logPath    :: FilePath
  } deriving Show

opts :: Parser Config
opts = Config
  <$> option (str >>= parseText)
      (long "text"
    <> short 't'
    <> metavar "TEXT"
    <> help "Text to translate. (reqired)")
  <*> option (str >>= parseLang)
      (long "from"
     <> short 'f'
     <> metavar "LANG"
     <> value "en"
     <> help "From language translate (default:English 'en') format ru,en etc.")
  <*> option (str >>= parseLang)
      (long "to"
     <> short 't'
     <> metavar "LANG"
     <> value "ru"
     <> help "To language translate (default:Russian 'ru') format ru,en etc.")
  <*> option (str >>= parseAPIKey)
      (long "key"
     <> short 'k'
     <> metavar "APIKEY"
     <> help "Key to access Yandex service (required)")
  <*> option (str >>= parseLogLevel)
      (long "loglevel"
    <>  short 'l'
    <>  metavar "LogLevel"
    <>  value EMERGENCY
    <>  help ("Log level on stderr (default: EMERGENCY) avialable:\n" ++
             "EMERGENCY,ALERT,CRITICAL,ERROR,WARNING,NOTICE,INFO,DEBUG\n" ++
             "To save log in file you can use stdout (>>out) or stderr (2>>err)"))
  <*> option (str >>= parseLogPath)
      (long "logpath"
    <>  short 'p'
    <>  metavar "LogPath"
    <>  value "./htrans.log"
    <>  help "Path and name to log file (default: ./htrans.log)")

cli :: IO Config
cli = execParser
    $ info (helper <*> opts)
      (fullDesc
    <> progDesc ("Translate text from one language to another\n" ++
                "using Yandex Translate API\n" )
    <> header "htrans - Yandex Translate API console tool")

parseLang :: Monad m => String -> m Language
parseLang st = return $ T.pack st

parseText :: Monad m => String -> m [T.Text]
parseText st = return [T.pack st]

parseAPIKey :: Monad m => String -> m APIKey
parseAPIKey st = return $ T.pack st

parseLogLevel :: Monad m => String -> m Priority
parseLogLevel st = return level
    where level = case map toLower st of
                      "debug"     -> DEBUG
                      "info"      -> INFO
                      "notice"    -> NOTICE
                      "warning"   -> WARNING
                      "error"     -> ERROR
                      "critical"  -> CRITICAL
                      "alert"     -> ALERT
                      _           -> EMERGENCY

parseLogPath :: Monad m => String -> m FilePath
parseLogPath st = return st

