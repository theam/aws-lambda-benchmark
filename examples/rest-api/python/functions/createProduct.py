import os
import json
from services import ProductService as service

productService = service.ProductService()


def handler(event, context):

    body = json.loads(event['body'])
    sku = body.get('sku', '')
    name = body.get('name', '')
    description = body.get('description', '')

    result = productService.createItem(sku, name, description)

    return {
        "statusCode": 200,
        "body": json.dumps(result)
    }
