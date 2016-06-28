module Htrans.Logger (
  -- funcs
  setAppLogger,
  appName,
  logStartAppDebug,
  logStopAppDebug,
  logConfigDebug,
  logInOutInfo

) where

import System.Log.Logger
import System.Log.Handler.Simple (fileHandler)
import System.Log.Handler (setFormatter, LogHandler)
import System.Log.Formatter (simpleLogFormatter)
import qualified Data.Text as T
import Paths_htrans (getDataDir)
import System.FilePath.Posix (takeBaseName)
import Htrans.Types (LogLevel(..))

appName :: IO String
appName = takeBaseName <$> getDataDir

setAppLogger :: FilePath -> LogLevel -> IO ()
setAppLogger logPath priority = do
  fHandler <- fileHandler logPath (getPriority priority)
  let cFormatter = setCustomFormatter fHandler
  name <- appName
  updateGlobalLogger name (addHandler cFormatter . setLevel (getPriority priority))

setCustomFormatter :: System.Log.Handler.LogHandler a => a -> a
setCustomFormatter h = setFormatter h f
  where f = simpleLogFormatter "[$time : $prio $loggername] : $msg "

logStartAppDebug :: IO ()
logStartAppDebug = do
  name <- appName
  debugM name "---- Start translation! ----"

logConfigDebug :: Show a => a -> IO ()
logConfigDebug cfg = do
  name <- appName
  debugM name ("Get configuration:" ++ show cfg)

logInOutInfo :: Maybe T.Text -> Maybe T.Text -> IO ()
logInOutInfo input output = do
  name <- appName
  infoM name $ "input:"  ++ showTxt input ++
              " output:" ++ showTxt output
  where showTxt = maybe noText T.unpack
        noText  = "No text"

logStopAppDebug :: IO ()
logStopAppDebug = do
  name <- appName
  debugM name "---- Stop translation! -----"

getPriority :: LogLevel -> Priority
getPriority level = case level of
  ERR -> ERROR
  INF -> INFO
  DEB -> DEBUG
  _   -> EMERGENCY
