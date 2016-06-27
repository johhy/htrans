{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T 
import qualified Data.Text.IO as I
import System.Exit (ExitCode(..), exitWith)
import Htrans.Logger (setAppLogger, logStartAppDebug,
                      logStopAppDebug, logConfigDebug, logInOutInfo)
import Htrans.Cli (Config(..), cli)
import Htrans.YandexTranslator (getTranslate)
import Htrans.XSelector (xselect)
import Htrans.EndPoint (showResult)

main :: IO ()
main = cli >>= xselect >>= doTrans >>= exitWith

doTrans :: Config -> IO ExitCode
doTrans cfg = do

  setAppLogger (logPath cfg) (logLevel cfg)
  logStartAppDebug
  logConfigDebug cfg

  res <- getTranslate (key cfg) (from cfg) (to cfg) (text cfg)

  logInOutInfo (unpackMaybe (text cfg)) (unpackMaybe res)
  logStopAppDebug

  showResult cfg res
  
  where unpackMaybe Nothing  = "No text"
        unpackMaybe (Just x) = T.unpack x




