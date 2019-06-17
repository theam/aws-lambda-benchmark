module Lib where

import GHC.Generics
import Aws.Lambda
import Data.Aeson

newtype Event = Event
  { resource :: String
  } deriving (Generic, FromJSON)

data Response = Response
  { statusCode:: Int
  , body :: String
  } deriving (Generic, ToJSON)

handler :: Event -> Context -> IO (Either String Response)
handler _ context =
  return $ Right Response
    { statusCode = 200
    , body = "hello"
    }
