{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Yandex.Translate
import Data.Default.Class
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Control.Monad.IO.Class
import Options.Applicative (Parser, execParser, value, fullDesc,
                            header, help, helper, info, long, metavar,
                            option, progDesc,short, str,
                            (<>))
import System.Exit (ExitCode(..), exitWith)
import System.Log.Logger
import System.Log.Handler.Simple (fileHandler)
import System.Log.Handler (setFormatter, LogHandler)
import System.Log.Formatter (simpleLogFormatter)
import Data.Char (toLower)
import Data.Maybe

appName = "htrans"

main :: IO ()
main = cli >>= doTrans >>= exitWith

doTrans :: Config -> IO ExitCode
doTrans cfg = do

  setAppLogger (logPath cfg) (logLevel cfg)
  debugM appName "---- Start translation! ----"
  debugM appName ("Get configuration:" ++ show cfg)

  res <- getTranslate (key cfg) (from cfg) (to cfg) (text cfg)

  infoM appName ("input:"  ++ unpackMaybe (validateText (text cfg)) ++
                " output:" ++ unpackMaybe res)

  maybe (return $ ExitFailure 1)
        ((>> return ExitSuccess) . I.putStrLn) res

  where unpackMaybe Nothing  = "No text"
        unpackMaybe (Just x) = T.unpack x


setAppLogger :: FilePath -> Priority -> IO ()
setAppLogger logPath priority = do
  fHandler <- fileHandler logPath priority
  let cFormatter = setCustomFormatter fHandler
  updateGlobalLogger appName (addHandler cFormatter . setLevel priority)

setCustomFormatter :: System.Log.Handler.LogHandler a => a -> a
setCustomFormatter h =
  setFormatter h f
     where f = simpleLogFormatter "[$time : $prio] : $msg "

validateText :: [T.Text] -> Maybe T.Text
validateText []    = Nothing
validateText (x:_) = if T.null x then Nothing else Just x

getTranslate :: APIKey -> Language -> Language -> [T.Text] -> IO (Maybe T.Text)
getTranslate key' from' to' text' =
  runYandexApiSession (configureApi key') $
  do (result,_,_) <-translate (Just from') to' def text'
     liftIO $ return (validateText result)

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
            "emergency" -> EMERGENCY

parseLogPath :: Monad m => String -> m FilePath
parseLogPath st = return st
