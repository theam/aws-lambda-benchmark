module Lib where

import           GHC.Generics
import           Aws.Lambda
import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.HashMap.Strict as HashMap
import           System.Environment
import qualified Network.AWS as Aws
import qualified Network.AWS.Data as Aws
import qualified Network.AWS.DynamoDB as DynamoDB
import           Network.AWS.DynamoDB.AttributeValue
import           Control.Lens
import           System.IO

data Product = Product { name :: Text, sku :: Text, description :: Text }
  deriving (Generic, FromJSON, ToJSON)

newtype Event = Event { resource :: String }
  deriving (Generic, FromJSON)

data Response = Response { statusCode :: Int, body :: Product }
  deriving (Generic, ToJSON)

handler :: Event -> Aws.Lambda.Context -> IO (Either String Response)
handler _ context = do
  tableName <- getEnv "PRODUCTS_TABLE_NAME"
  env <- Aws.newEnv Aws.Discover
  res <- Aws.runResourceT . Aws.runAWS env
    $ do
      let skuAttributeValue = DynamoDB.attributeValue
            & set DynamoDB.avS (Just "test")
      let keys = HashMap.fromList [("sku", skuAttributeValue)]
      let getItem = DynamoDB.getItem (Aws.toText tableName)
            & set DynamoDB.giKey keys
      Aws.send $ getItem
  let item = res & view DynamoDB.girsItem
  let decodingResult = fromAttributeValueMap item
        :: Either ConversionError Product
  case decodingResult of
    Left _
      -> return $ Right Response { statusCode = 500, body = Product {  } }
    Right value -> return $ Right Response { statusCode = 200, body = value }