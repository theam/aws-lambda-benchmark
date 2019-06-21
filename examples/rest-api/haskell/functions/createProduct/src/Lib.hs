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
  , body :: String 
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
  let jsonBody = body (event::Event)
  let parsedProduct = (decode (ByteString.pack jsonBody) :: Maybe Product )
  case parsedProduct of
    Nothing -> 
      return $ Right Response { statusCode = 500, body = "Invalid input" }
    Just product -> do
      res <- Aws.runResourceT . Aws.runAWS env $ do
          let skuAttributeValue = DynamoDB.attributeValue
                & set DynamoDB.avS (Just (sku product))
          let nameAttributeValue = DynamoDB.attributeValue
                & set DynamoDB.avS (name product)
          let descriptionAttributeValue = DynamoDB.attributeValue
                & set DynamoDB.avS (description product)
          let keys = HashMap.fromList [("sku", skuAttributeValue), ("name", nameAttributeValue), ("description", descriptionAttributeValue)]
          let putItem = DynamoDB.putItem (Aws.toText tableName)
                & set DynamoDB.piItem keys
          Aws.send $ putItem
      return $ Right Response { statusCode = 200, body = (show res) }
  -- return $ Right Response { statusCode = 200, body = (body (event::Event)) }