module Htrans.XSelector (

  xselect

) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import qualified Data.Text as T hiding (map)
import Codec.Binary.UTF8.String (decode)
import Htrans.Cli (Config(..))

xselect :: Config -> IO Config
xselect cfg = 
  case text cfg of
    Nothing -> do
      dpy <- openDisplay ""
      let dflt = defaultScreen dpy
      rootw  <- rootWindow dpy dflt
      win <- createSimpleWindow dpy rootw 0 0 1 1 0 0 0
      p   <- internAtom dpy "PRIMARY" True
      ty  <- internAtom dpy "UTF8_STRING" False
      clp <- internAtom dpy "BLITZ_SEL_STRING" False
      xConvertSelection dpy p ty clp win currentTime
      allocaXEvent $ \e -> do
        nextEvent dpy e
        ev <- getEvent e
        if ev_event_type ev == selectionNotify
          then do res <- getWindowProperty8 dpy clp win
                  destroyWindow dpy win
                  return $ cfg {text = validateText (return T.pack <*> (return decode <*> (return (map fromIntegral) <*> res)))}
          else do
               destroyWindow dpy win
               return cfg
    _ -> return cfg
    
validateText :: Maybe T.Text -> Maybe T.Text
validateText Nothing = Nothing
validateText (Just x) = if T.null x then Nothing else Just x 
