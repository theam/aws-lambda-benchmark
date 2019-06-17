module Lib where

import GHC.Generics
import Aws.Lambda.Runtime
import Data.Aeson

data Event = Event
  { resource :: String
  } deriving (Generic)

instance FromJSON Event

data Response = Response
  { statusCode:: Int,
    body :: String,
    isBase64Encoded :: Bool
  } deriving (Generic)
instance ToJSON Response

handler :: Event -> Context -> IO (Either String Response)
handler _ context = do
  return (Left "Error")
  return (Right (Response { statusCode = 200, body = "hello", isBase64Encoded = False}))
