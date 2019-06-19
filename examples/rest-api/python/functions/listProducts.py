import os
import json
from services import ProductService as service

productService = service.ProductService()


def handler(event, context):

    result = productService.listItems()
    items = result.get('Items', [])
    print(items)

    return {
        "statusCode": 200,
        "body": json.dumps(items)
    }
