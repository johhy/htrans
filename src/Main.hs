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

main :: IO ()
main = cli >>= doTrans >>= exitWith

doTrans :: Config -> IO ExitCode
doTrans cfg = do
  res <- getTranslate (key cfg) (from cfg) (to cfg) (text cfg)
  maybe (return $ ExitFailure 1)
        ((>> return ExitSuccess) . I.putStrLn) res

getStr :: [T.Text] -> Maybe T.Text
getStr txt =
  if not (null txt)
                then Just txt'
                else Nothing
  where txt' = head txt

getTranslate :: APIKey -> Language -> Language -> [T.Text] -> IO (Maybe T.Text)
getTranslate k f t txt = 
  runYandexApiSession (configureApi k) $
  do (msg,_,_) <-translate (Just f) t def txt
     liftIO $ return (getStr msg)

data Config = Config
  {
    text       :: [T.Text]
  , from       :: Language
  , to         :: Language
  , key        :: APIKey
  }

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
