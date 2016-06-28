{-# LANGUAGE OverloadedStrings #-}

module Htrans.EndPoint (

  showResult

) where

import qualified Data.Text as T
import qualified Data.Text.IO as I

import System.Exit (ExitCode(..))
import Htrans.Types(Config(..))
import Htrans.Screener (notify)

showResult :: Config -> Maybe T.Text -> IO ExitCode
showResult cfg res = case res of
  Nothing -> return $ ExitFailure 1
  Just x  -> if onScreen cfg
             then do
                    _ <- notify (T.unpack $ unpackMaybe (textToTranslate cfg)) (T.unpack x)
                    return ExitSuccess
             else do
                    I.putStrLn x
                    return ExitSuccess
                    
  where unpackMaybe Nothing  = "No text"
        unpackMaybe (Just x) = x

