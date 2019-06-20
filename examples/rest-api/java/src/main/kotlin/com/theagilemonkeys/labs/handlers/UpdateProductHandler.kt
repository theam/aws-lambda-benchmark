package com.theagilemonkeys.labs.handlers

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent
import com.google.gson.Gson
import com.google.gson.JsonObject
import com.theagilemonkeys.labs.model.Product
import com.theagilemonkeys.labs.responses.ProductResponse
import com.theagilemonkeys.labs.responses.generateErrorResponse
import com.theagilemonkeys.labs.responses.generateOKResponse
import com.theagilemonkeys.labs.services.ProductService
import org.apache.http.HttpStatus

class UpdateProductHandler (private val productService: ProductService): ProductHandler {
    override fun handle(request: APIGatewayProxyRequestEvent): APIGatewayProxyResponseEvent {
        return try {
            val sku = request.pathParameters?.get(Product.SKU_FIELD)
            if (sku.isNullOrEmpty()) {
                return generateErrorResponse(errorCode = HttpStatus.SC_BAD_REQUEST, message = "Error processing sku or sku not provided")
            }

            request.body ?: return generateErrorResponse(errorCode = HttpStatus.SC_BAD_REQUEST, message = "Empty body, nothing to update")

            val requestBody = Gson().fromJson(request.body, JsonObject::class.java)
            val name = requestBody.get(Product.NAME_FIELD)?.asString
            val description = requestBody.get(Product.DESCRIPTION_FIELD)?.asString

            val product = productService.update(sku, name, description)

            generateOKResponse(ProductResponse(product))

        } catch (e: Exception) {
            generateErrorResponse(errorCode = HttpStatus.SC_INTERNAL_SERVER_ERROR, message = e.localizedMessage)
        }
    }
}
