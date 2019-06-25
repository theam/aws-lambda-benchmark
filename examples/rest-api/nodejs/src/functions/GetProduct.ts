import ProductService from "../services/ProductService";
import {
	APIGatewayProxyEvent,
	APIGatewayProxyResult,
	Context
} from "aws-lambda";

var service = new ProductService(process.env.PRODUCTS_TABLE_NAME);
export const handler = async (
	event: APIGatewayProxyEvent,
	context: Context
): Promise<APIGatewayProxyResult> => {
	context.callbackWaitsForEmptyEventLoop = false;

	let sku: string = event.pathParameters ? event.pathParameters.sku : "";
	if (sku == "") {
		return {
			statusCode: 400,
			body: "Bad request, sku cannot be empty"
		};
	} else {
		try {
			var item = await service.getItem(sku);
			return { statusCode: 200, body: JSON.stringify(item) };
		} catch (e) {
			return {
				statusCode: 500,
				body: "Server failure, unable to retrieve item"
			};
		}
	}
};
