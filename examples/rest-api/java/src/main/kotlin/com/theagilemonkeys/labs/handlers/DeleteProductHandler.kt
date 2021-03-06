package com.theagilemonkeys.labs.handlers

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent
import com.theagilemonkeys.labs.model.Product
import com.theagilemonkeys.labs.responses.generateErrorResponse
import com.theagilemonkeys.labs.responses.generateOKResponse
import com.theagilemonkeys.labs.services.ProductService
import org.apache.http.HttpStatus

class DeleteProductHandler(private val productService: ProductService): ProductHandler {
    override fun handle(request: APIGatewayProxyRequestEvent): APIGatewayProxyResponseEvent {
        return try {
            val sku = request.pathParameters?.get(Product.SKU_FIELD)
            if (sku.isNullOrEmpty()) {
                return generateErrorResponse(errorCode = HttpStatus.SC_BAD_REQUEST, message = "Error processing sku or sku not provided")
            }
            productService.delete(sku)
            generateOKResponse()
        } catch (e: Exception) {
            generateErrorResponse(errorCode = HttpStatus.SC_INTERNAL_SERVER_ERROR, message = e.localizedMessage)
        }
    }
}
