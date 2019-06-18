package com.theagilemonkeys.labs.handlers

import com.amazonaws.services.lambda.runtime.Context
import com.amazonaws.services.lambda.runtime.RequestHandler
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent
import com.google.gson.Gson
import com.google.gson.JsonObject
import com.theagilemonkeys.labs.model.Product
import com.theagilemonkeys.labs.responses.*
import com.theagilemonkeys.labs.services.ProductService
import org.apache.http.HttpStatus


class CreateProductHandler : RequestHandler<APIGatewayProxyRequestEvent, APIGatewayProxyResponseEvent> {
    private val productService = ProductService()

    override fun handleRequest(request: APIGatewayProxyRequestEvent, context: Context): APIGatewayProxyResponseEvent {
        return try {
            request.body ?: return generateErrorResponse(errorCode = HttpStatus.SC_BAD_REQUEST, message = "Product sku and product name are required")

            val requestBody = Gson().fromJson(request.body, JsonObject::class.java)
            val sku = requestBody.get(Product.SKU_FIELD)?.asString
            val name = requestBody.get(Product.NAME_FIELD)?.asString
            val description = requestBody.get(Product.DESCRIPTION_FIELD)?.asString

            if (sku.isNullOrEmpty() || name.isNullOrEmpty()) {
                return generateErrorResponse(errorCode = HttpStatus.SC_BAD_REQUEST, message = "Product sku and product name are required")
            }

            val product = productService.createProduct(sku, name, description)
            generateOKResponse(ProductResponse(product))

        } catch (e: Exception) {
            generateErrorResponse(errorCode = HttpStatus.SC_INTERNAL_SERVER_ERROR, message = e.localizedMessage)
        }
    }
}

