package com.theagilemonkeys.labs.responses

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent
import com.theagilemonkeys.labs.model.Product
import org.apache.http.HttpStatus

data class ErrorResponse(val status: Int, val error: String) : GenericResponse()
data class ProductResponse(val product: Product) : GenericResponse()
data class ProductsResponse(val products: List<Product>) : GenericResponse()

fun generateOKResponse(entity: GenericResponse? = null) = APIGatewayProxyResponseEvent()
        .withStatusCode(HttpStatus.SC_OK)
        .withBody(entity?.toJSON())

fun generateErrorResponse(errorCode: Int, message: String) = APIGatewayProxyResponseEvent()
        .withStatusCode(errorCode)
        .withBody(ErrorResponse(errorCode, message).toJSON())
