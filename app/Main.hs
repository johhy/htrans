{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Exit (ExitCode(..), exitWith)
import Htrans.Logger (setAppLogger, logStartAppDebug,
                      logStopAppDebug, logConfigDebug, logInOutInfo)
import Htrans.Cli (cli)
import Htrans.Types (Config(..))
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

  res <- getTranslate (keyAPI cfg) (fromLang cfg) (toLang cfg) (textToTranslate cfg)

  logInOutInfo (textToTranslate cfg) res
  logStopAppDebug

  showResult cfg res
  




