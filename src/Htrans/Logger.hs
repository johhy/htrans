module Htrans.Logger (
  -- types
  Priority(..),
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

appName :: String
appName = "htrans"

setAppLogger :: FilePath -> Priority -> IO ()
setAppLogger logPath priority = do
    fHandler <- fileHandler logPath priority
    let cFormatter = setCustomFormatter fHandler
    updateGlobalLogger appName (addHandler cFormatter . setLevel priority)

setCustomFormatter :: System.Log.Handler.LogHandler a => a -> a
setCustomFormatter h = setFormatter h f
    where f = simpleLogFormatter "[$time : $prio] : $msg "


logStartAppDebug :: IO ()
logStartAppDebug = debugM appName "---- Start translation! ----"

logConfigDebug :: Show a => a -> IO ()
logConfigDebug cfg = debugM appName ("Get configuration:" ++ show cfg)

logInOutInfo :: Maybe T.Text -> Maybe T.Text -> IO ()
logInOutInfo input output = infoM appName $ "input:"  ++ showTxt input ++
                                          " output:" ++ showTxt output
     where showTxt = maybe noText T.unpack
           noText  = "No text"
           
logStopAppDebug :: IO ()
logStopAppDebug = debugM appName "---- Stop translation! -----"

