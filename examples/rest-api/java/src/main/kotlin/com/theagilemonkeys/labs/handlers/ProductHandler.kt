package com.theagilemonkeys.labs.handlers

import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyRequestEvent
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent

interface ProductHandler {
    fun handle(request: APIGatewayProxyRequestEvent): APIGatewayProxyResponseEvent
}