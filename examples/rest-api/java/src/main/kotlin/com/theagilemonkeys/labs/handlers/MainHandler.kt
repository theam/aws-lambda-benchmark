package com.theagilemonkeys.labs.handlers

import com.amazonaws.HttpMethod
import com.amazonaws.services.lambda.runtime.Context
import com.amazonaws.services.lambda.runtime.RequestHandler
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent
import com.theagilemonkeys.labs.responses.generateErrorResponse
import com.theagilemonkeys.labs.services.DynamoDBProductService
import org.apache.http.HttpStatus

class MainHandler : RequestHandler<APIGatewayProxyRequestEvent, APIGatewayProxyResponseEvent> {
    private val productService = DynamoDBProductService()

    override fun handleRequest(request: APIGatewayProxyRequestEvent, context: Context): APIGatewayProxyResponseEvent {
        val handler: ProductHandler? = when(request.httpMethod) {
            HttpMethod.GET.name ->  {
                if (request.pathParameters.isNullOrEmpty()) {
                    ListProductsHandler(productService)
                } else {
                    GetProductHandler(productService)
                }
            }
            HttpMethod.POST.name -> CreateProductHandler(productService)
            HttpMethod.PATCH.name -> UpdateProductHandler(productService)
            HttpMethod.DELETE.name -> DeleteProductHandler(productService)
            else -> null
        }
        handler?.let {
            return it.handle(request)
        }

        return generateErrorResponse(HttpStatus.SC_INTERNAL_SERVER_ERROR, "Unable to route request")
    }
}