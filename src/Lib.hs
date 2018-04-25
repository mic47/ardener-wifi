module Lib
    ( setWifi
    , turnOffEcho
    , setBaudRate
    , guessCommSpeed
    , possibleSpeeds
    ) where

import Control.Monad
import Control.Monad.Catch (catchAll)
import Control.Monad.IO.Class
import Control.Monad.Loops
import System.Hardware.Serialport
import Text.Printf

import SerialIO

turnOffEcho :: SerialIO ()
turnOffEcho = void $ executeCommand "ATE0"

setWifi :: String -> String -> SerialIO ()
setWifi ssid psk = do
  void $ executeCommand "AT+CWMODE=1"
  -- Use longer TTL (2 minutes) because it can take longer time.
  void $ executeCommandWithTTL 120000000 $
    printf "AT+CWJAP_DEF=\"%s\",\"%s\"" ssid psk

setBaudRate :: CommSpeed -> SerialIO ()
setBaudRate baudRate = do
  void $ executeCommand $
    printf "AT+UART_DEF=%s,8,1,0,1" (drop 2 $ show baudRate)
  reconnect baudRate
  r <- executeCommand "AT+UART_DEF?"
  liftIO $ print $ show r

guessCommSpeed :: String -> IO CommSpeed
guessCommSpeed port = do
  frst <- flip firstM possibleSpeeds $ \speed -> do
    print $ "Trying " ++ show speed
    (const True <$> runSerial
      port
      (defaultSerialSettings {commSpeed = speed})
      (executeCommandWithTTL 1000000 "AT")
     ) `catchAll` (\e -> do
      print $ show e
      return False)
  case frst of
    Nothing -> fail
      "Serial connection is not accepting any speed."
    Just speed -> do
      print $ "Selected speed " ++ show speed
      return speed

possibleSpeeds :: [CommSpeed]
possibleSpeeds =
  [ CS9600
  , CS115200
  , CS110
  , CS300
  , CS600
  , CS1200
  , CS2400
  , CS4800
  , CS19200
  , CS38400
  , CS57600
  ]
