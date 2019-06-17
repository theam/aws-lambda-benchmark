package com.theagilemonkeys.labs

import com.amazonaws.services.lambda.runtime.Context
import com.amazonaws.services.lambda.runtime.RequestHandler
import com.amazonaws.services.lambda.runtime.events.APIGatewayProxyResponseEvent
import com.theagilemonkeys.labs.responses.HelloResponse

class HelloWorldHandler : RequestHandler<Map<String, Any>, APIGatewayProxyResponseEvent> {
    override fun handleRequest(input: Map<String, Any>, context: Context): APIGatewayProxyResponseEvent {
        return APIGatewayProxyResponseEvent()
                .withStatusCode(200)
                .withBody(HelloResponse(message = "Hello World!").toJSON())
    }
}
