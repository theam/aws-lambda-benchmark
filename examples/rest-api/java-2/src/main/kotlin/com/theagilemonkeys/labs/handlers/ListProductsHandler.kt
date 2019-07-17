package com.theagilemonkeys.labs.handlers

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent
import com.theagilemonkeys.labs.responses.ProductsResponse
import com.theagilemonkeys.labs.responses.generateErrorResponse
import com.theagilemonkeys.labs.responses.generateOKResponse
import com.theagilemonkeys.labs.services.DynamoDBProductService
import org.apache.http.HttpStatus

class ListProductsHandler: ProductHandler {
    private val productService = DynamoDBProductService()

    override fun handle(request: APIGatewayProxyRequestEvent): APIGatewayProxyResponseEvent {
        return try {
            val products = productService.getAll()
            generateOKResponse(ProductsResponse(products))
        } catch (e: Exception) {
            generateErrorResponse(errorCode = HttpStatus.SC_INTERNAL_SERVER_ERROR,
                    message = e.localizedMessage)
        }
    }
}
