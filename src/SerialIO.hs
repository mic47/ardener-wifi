{-# LANGUAGE RecordWildCards #-}
module SerialIO
  ( runSerial
  , SerialException(..)
  , SerialIO
  , executeCommandWithTTL
  , executeCommandWithoutTTL
  , executeCommand
  , catchSerialIO
  , throwSerialIO
  , catchPrintCE
  , reconnect
  , setVerboseFlag
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad.Extra
import Control.Monad.Catch (catchAll)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.RWS.Lazy
import Data.List
import Data.Maybe
import System.Hardware.Serialport hiding (timeout)
import System.IO
import System.Timeout

data State = State
  { serialConnection :: Handle
  , verbose :: Bool
  }

data Env = Env
  { serialPort :: String
  , defaultTTL :: Maybe Int
  }

data SerialException
  = SerialCommandTimedOut
  | SerialCommandFailure [String]
  deriving (Show)

instance Exception SerialException

newtype SerialIO a = SerialIO (ExceptT SomeException (RWST Env () State IO) a)
  deriving (Monad, Applicative, Functor)

instance MonadIO SerialIO where
  liftIO a = do
    r <- SerialIO $ lift $ lift $ catch (Right <$> a) (return . Left)
    case r of
      Left e -> throwSerialIO e
      Right v -> return v

askSerialIO :: SerialIO Env
askSerialIO = SerialIO $ lift ask

getSerialIO :: SerialIO State
getSerialIO = SerialIO $ lift get

putSerialIO :: State -> SerialIO ()
putSerialIO x = SerialIO $ lift $ put x

throwSerialIO :: SerialException -> SerialIO a
throwSerialIO = SerialIO . throwE . toException

setVerboseFlag :: Bool -> SerialIO ()
setVerboseFlag flag = do
  state' <- getSerialIO
  putSerialIO $ state' {verbose = flag}

runSerial :: String -> SerialPortSettings -> SerialIO a -> IO a
runSerial serialPort settings (SerialIO a) = do
  h <- hOpenSerial serialPort settings
  -- ExceptT should catch all IO exception, so therefore handle
  -- will be released in the end.
  (ret, state', _) <- runRWST
    (runExceptT a)
    Env
      { serialPort = serialPort
      , defaultTTL = Nothing
      }
    State
      { serialConnection = h
      , verbose = False
      }
  hClose (serialConnection state')
  case ret of
    Left e -> throw e
    Right val -> return val

eol :: String
eol = "\r\n"

-- Executes
executeCommandIO :: Handle -> String -> IO (Either SerialException [String])
executeCommandIO conn command = do
  hPutStr conn $ command ++ eol
  readResponse []
  where
    safeHGetLineImpl =
      hGetLine conn `catchAll` \e ->
        if "hGetLine: end of file" `isSuffixOf` show e
        then do
          threadDelay 100000
          safeHGetLineImpl
        else throw e
    readResponse response = do
      line <- safeHGetLineImpl `catchAll` \e -> do
        print $ show e
        throw e
      case line of
        r@"OK\r" -> return $ Right (toLines (r:response))
        r@"ERROR\r" -> return $ Left $ SerialCommandFailure $ toLines $ r:response
        cmd -> readResponse (cmd:response)
    toLines = reverse . map (dropWhileEnd ('\r'==))

executeCommandWithTTL :: Int -> String -> SerialIO [String]
executeCommandWithTTL ttl command = do
  State{..} <- getSerialIO
  liftIO $ when verbose $ print command
  ret <- liftIO $ timeout ttl $ executeCommandIO serialConnection command
  liftIO $ when verbose $ print $ show ret
  case ret of
    Nothing -> throwSerialIO SerialCommandTimedOut
    (Just (Left e)) -> throwSerialIO e
    (Just (Right e)) -> return e

catchSerialIO
  :: (Exception e)
  => SerialIO a
  -> (e -> SerialIO a)
  -> SerialIO a
catchSerialIO (SerialIO action) handler = SerialIO $
  action `catchE` \e -> case fromException e of
    Nothing -> throwE e
    Just exc -> unSerialIO $ handler exc
  where
    unSerialIO (SerialIO a) = a

catchPrintCE :: a -> SerialIO a -> SerialIO a
catchPrintCE defVal action =
  action `catchSerialIO` (\(e :: SerialException) -> do
    liftIO $ print $ show e
    return defVal
  )

executeCommandWithoutTTL :: String -> SerialIO [String]
executeCommandWithoutTTL = executeCommandWithTTL (-1)

executeCommand :: String -> SerialIO [String]
executeCommand command = do
  Env{..} <- askSerialIO
  executeCommandWithTTL (fromMaybe 5000000 defaultTTL) command

reconnect :: CommSpeed -> SerialIO ()
reconnect baudRate = do
  state' <- getSerialIO
  liftIO $ hClose $ serialConnection state'
  Env{..} <- askSerialIO
  sc <- liftIO $ hOpenSerial serialPort defaultSerialSettings {commSpeed = baudRate}
  putSerialIO State
    { serialConnection = sc
    , verbose = verbose state'
    }
