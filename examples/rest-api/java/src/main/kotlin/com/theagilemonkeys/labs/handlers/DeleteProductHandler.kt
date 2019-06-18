package com.theagilemonkeys.labs.handlers

import com.amazonaws.services.lambda.runtime.Context
import com.amazonaws.services.lambda.runtime.RequestHandler
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent
import com.theagilemonkeys.labs.model.Product
import com.theagilemonkeys.labs.responses.generateErrorResponse
import com.theagilemonkeys.labs.responses.generateOKResponse
import com.theagilemonkeys.labs.services.ProductService
import org.apache.http.HttpStatus
import java.lang.Exception

class DeleteProductHandler : RequestHandler<APIGatewayProxyRequestEvent, APIGatewayProxyResponseEvent> {
    private val productService = ProductService()

    override fun handleRequest(request: APIGatewayProxyRequestEvent, context: Context): APIGatewayProxyResponseEvent {
        return try {
            val sku = request.pathParameters?.get(Product.SKU_FIELD)
            if (sku.isNullOrEmpty()) {
                return generateErrorResponse(errorCode = HttpStatus.SC_BAD_REQUEST, message = "Error processing sku or sku not provided")
            }
            productService.deleteProduct(sku)
            generateOKResponse()
        } catch (e: Exception) {
            generateErrorResponse(errorCode = HttpStatus.SC_INTERNAL_SERVER_ERROR, message = e.localizedMessage)
        }
    }
}
