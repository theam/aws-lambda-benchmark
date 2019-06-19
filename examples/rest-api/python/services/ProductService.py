import os
import datetime
import boto3
dynamoDB = boto3.resource('dynamodb')


class ProductService:
    def __init__(self):
        self.dynamoDBClient = dynamoDB.Table(os.environ['PRODUCTS_TABLE_NAME'])

    def getItem(self, sku):
        return self.dynamoDBClient.get_item(
            Key={'sku': sku}
        )

    def createItem(self, sku, name, description):
        timestamp = datetime.datetime.utcnow().isoformat()

        item = {
            'sku': sku,
            'name': name,
            'description': description,
            'createdAt': timestamp,
            'updatedAt': timestamp
        }

        self.dynamoDBClient.put_item(Item=item, ReturnValues='NONE')

        return item

    def deleteItem(self, sku):

        return self.dynamoDBClient.delete_item(Key={'sku': sku})

    def updateItem(self, sku, name, description):

        return self.dynamoDBClient.update_item(
            Key={'sku':sku},
            ExpressionAttributeNames={
                '#name': 'name',
                '#description': 'description'
            },
            ExpressionAttributeValues={
                ':n': name,
                ':d': description
            },
            UpdateExpression="set #name = :n, #description = :d",
            ReturnValues="UPDATED_NEW"
        )
    
    def listItems(self):

        return self.dynamoDBClient.scan()
