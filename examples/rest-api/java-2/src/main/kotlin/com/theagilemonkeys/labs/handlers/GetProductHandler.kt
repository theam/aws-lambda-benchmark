package com.theagilemonkeys.labs.handlers

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent
import com.theagilemonkeys.labs.model.Product
import com.theagilemonkeys.labs.responses.ProductResponse
import com.theagilemonkeys.labs.responses.generateErrorResponse
import com.theagilemonkeys.labs.responses.generateOKResponse
import com.theagilemonkeys.labs.services.DynamoDBProductService
import com.theagilemonkeys.labs.services.ProductService
import org.apache.http.HttpStatus

class GetProductHandler: ProductHandler  {
    private val productService = DynamoDBProductService()

    override fun handle(request: APIGatewayProxyRequestEvent): APIGatewayProxyResponseEvent {
        return try {
            val sku = request.pathParameters?.get(Product.SKU_FIELD)
            if (sku.isNullOrEmpty()) {
                return generateErrorResponse(errorCode = HttpStatus.SC_BAD_REQUEST,
                        message = "Error processing sku or sku not provided")
            }

            val product = productService.getBySku(sku)

            product?.let {
                return generateOKResponse(ProductResponse(product))
            }

            generateErrorResponse(HttpStatus.SC_NOT_FOUND,
                    "Product not found")

        } catch (e: Exception) {
            generateErrorResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR,
                    e.localizedMessage)
        }
    }
}
