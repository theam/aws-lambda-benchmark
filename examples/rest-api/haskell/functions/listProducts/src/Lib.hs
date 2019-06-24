module Lib where

import           GHC.Generics
import           Aws.Lambda
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.ByteString.Lazy.Char8 as ByteString
import qualified Data.HashMap.Strict as HashMap
import           System.Environment
import qualified Network.AWS as Aws
import qualified Network.AWS.Data as Aws
import qualified Network.AWS.DynamoDB as DynamoDB
import           Control.Lens
import           System.IO

data Product = Product
  { name :: Maybe Text
  , sku :: Text
  , description :: Maybe Text
  }
  deriving (Generic, FromJSON, ToJSON)

data Event = Event
  { pathParameters :: Maybe (HashMap.HashMap Text Text)
  , body :: Maybe String
  }
  deriving (Generic, FromJSON, ToJSON)

data Response = Response
  { statusCode :: Int
  , body :: String
  }
  deriving (Generic, ToJSON)

handler :: Event -> Aws.Lambda.Context -> IO (Either String Response)
handler event context = do
  tableName <- getEnv "PRODUCTS_TABLE_NAME"
  env <- Aws.newEnv Aws.Discover
  res <- Aws.runResourceT . Aws.runAWS env $ do
      let listItems = DynamoDB.scan (Aws.toText tableName)
      Aws.send $ listItems
  let items = res & view DynamoDB.srsItems
  return $ Right Response { statusCode = 200, body = (show items) }