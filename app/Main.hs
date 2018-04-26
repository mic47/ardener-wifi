{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Monad.Extra
import Control.Monad.IO.Class
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Monoid
import Options.Applicative
import System.Exit
import System.Hardware.Serialport
import System.FilePath.Posix
import System.Process

import Lib
import SerialIO

newtype Options = Options
  { serialPort :: String
  }

data Command
  = FlashFirmware
  | Setup SetupOpts
  | Console

data SetupOpts = SetupOpts
  { baudRate :: CommSpeed
  , ssid :: String
  , psk :: String
  }

allParser :: Parser (Options, [Command])
allParser = (,)
  <$> optionsParser
  <*> many commandParser

commandParser :: Parser Command
commandParser = subparser
  ( command "flash-firmware"
    (info (pure FlashFirmware) (progDesc "Flash firmware."))
  <> command "setup-defaults"
    (info (Setup <$> setupOptsParser) (progDesc "Set default settings."))
  <> command "serial-console"
    (info (pure Console) (progDesc "Run serial console"))
  )

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
    ( long "serial-port"
    <> metavar "SERIALPORT"
    <> value "/dev/ttyUSB0"
    )

setupOptsParser :: Parser SetupOpts
setupOptsParser = SetupOpts
  <$> option
    ( eitherReader
      (\val -> case HashMap.lookup val speedsHM of
        Nothing -> Left $ val ++ " is not valid baud rate."
        Just x -> Right x
      )
    )
    ( long "baud-rate"
    <> metavar "BAUDRATE"
    <> value CS9600
    )
  <*> strOption
    ( long "ssid"
    <> metavar "SSID"
    )
  <*> strOption
    ( long "psk"
    <> metavar "PASSWORD"
    )
  where
    speedsHM = HashMap.fromList $
      map (\x -> (drop 2 $ show x, x)) possibleSpeeds

parseOptions :: IO (Options, [Command])
parseOptions = execParser
  (info (allParser <**> helper)
     ( fullDesc
     <> progDesc "Tool for preparing serial wifi parts."
     )
  )

binariesPrefix :: FilePath
binariesPrefix = "../ESP8266_NONOS_SDK/bin"

main :: IO ()
main = do
  (Options{..}, commands) <- parseOptions
  forM_ commands $ \case
    FlashFirmware -> do
      putStrLn
        "Set wifi dongle to program mode, reset it and press enter when ready."
      void getLine
      eCode <- rawSystem
        "esptool.py"
        [ "-p", serialPort
        , "write_flash"
        , "0x0000", binariesPrefix </> "boot_v1.7.bin"
        , "0x01000", binariesPrefix </> "at/512+512/user1.1024.new.2.bin"
        , "0x7C000", binariesPrefix </> "esp_init_data_default_v08.bin"
        , "0x7E000", binariesPrefix </> "blank.bin"
        ]

      if eCode /= ExitSuccess then do
        putStrLn "Firmware update failed. Quitting."
        exitFailure
      else do
        putStrLn $
          "Firmware update completed. Please set device to UART mode and"
          ++ " reset it. Press enter when ready."
        void getLine
    Console -> do
      putStrLn "Starting console"
      commSpeed <- guessCommSpeed serialPort
      runSerial
        serialPort
        (defaultSerialSettings { commSpeed = commSpeed})
        $ whileM $ do
          cmd <- liftIO getLine
          when (cmd /= "") $ do
            ret <- catchPrintCE [] $ executeCommand cmd
            liftIO $ putStrLn $ intercalate "\n" ret
          return True
    Setup SetupOpts{..} -> do
      commSpeed <- guessCommSpeed serialPort
      runSerial
        serialPort
        (defaultSerialSettings { commSpeed = commSpeed})
        $ do
          setVerboseFlag True
          liftIO $ putStrLn "Running serial"
          turnOffEcho
          setWifi ssid psk
          setBaudRate baudRate
