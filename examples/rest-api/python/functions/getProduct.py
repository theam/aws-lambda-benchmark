import os
import json
from services import ProductService as service

productService = service.ProductService()


def handler(event, context):

    sku = event['pathParameters']['sku']

    result = productService.getItem(sku)
    item = result.get('Item', '')

    return {
        "statusCode": 200,
        "body": json.dumps(item)
    }
