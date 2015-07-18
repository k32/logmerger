{-# LANGUAGE UnicodeSyntax, OverloadedStrings #-}
import qualified Data.ByteString as B
import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Word (Word64)
import Data.Aeson
import Data.Time
import qualified Data.Map as M
import Snap.Core
import Snap.Http.Server
import System.FastLogger
import System.Console.CmdArgs
import Rest.Driver.Snap
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad
import System.Random (randomIO)

data UserSession = UserSession {
    _last_access    ∷ UTCTime
  , _sessionId      ∷ Word64
  , _sessionCookie  ∷ Word64
  , _myEntries      ∷ [Entry]
  -- , _myFilter       ∷ Filter
  } deriving (Show, Read)

type Sessions = M.Map Word64 MySession

data NodeDump =
  NodeDump
  {
    _title
  , _hardware
  , _site ∷ String
  , _dateCollected ∷ Date
  } deriving (Show, Read)

data Entry =
  Entry
  {
    origin, severity, date, txt ∷ String
  } deriving (Show, Read)

instance ToJSON Entry where
  toJSON (Entry o s d t) = object [ "origin" .= o
                                  , "severity" .= s
                                  , "date" .= d
                                  , "txt" .= t]

mockEntries = [ Entry "log.rb" "sev_crash" "12-13-1922" "Serious shit"
              , Entry "isp.log" "sev_normal" "12-13-1993" "ncl;sss"
              , Entry "isp.log" "sev_crash" "12-13-1995" "capsule_failure"]

data LogViewer = 
  LogViewer
  {
    _sessionLifetime ∷ Int
  , _staticDir ∷ String
  } deriving (Show, Data, Typeable)

myCmdArgs ∷ LogViewer
myCmdArgs =
  LogViewer
  {
    _sessionLifetime = 24*60*60
  , _staticDir = "../web"
  }

main ∷ IO ()
main = do
  cfg ← cmdArgs myCmdArgs
  
