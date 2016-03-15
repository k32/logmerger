-- Dirty trickery to test on OSX. Stub code.
import System.IO
import System.Environment
import GHC.IO.Device

import GHC.IO.Device as IODevice
import GHC.IO.Handle.Types
import GHC.IO.Handle.Internals

isEndpoint :: IO Bool
isEndpoint = do
  ret <- withHandle_ "isEndpoint" stdout $ \Handle__{haType = htype, haDevice = hdev} -> do
     case htype of
       ClosedHandle -> ioe_closedHandle
       _            -> do
                  isTerm <- IODevice.isTerminal hdev
                  devtype <- IODevice.devType hdev
                  return (isTerm, devtype)
  case ret of
    (True, _) -> return True
    (_, RegularFile) -> return True
    _ -> return False
    
main = do
  args <- getArgs
  ep <- isEndpoint
  upstream <- hIsTerminalDevice stdin
  contents <- case upstream of
                False -> hGetContents stdin
                _ -> return ""
  hPutStrLn stderr (show args ++ " -> " ++ show ep ++ show contents)
  if ep
  then putStrLn "This is the 'actual' output"
  else putStrLn (contents ++ show args)
