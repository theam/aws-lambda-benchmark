import ProductService from "../services/ProductService";
import {
	APIGatewayProxyEvent,
	APIGatewayProxyResult,
	Context
} from "aws-lambda";
import Product from "../models/Product";

var service = new ProductService(process.env.PRODUCTS_TABLE_NAME);
export const handler = async (
	event: APIGatewayProxyEvent,
	context: Context
): Promise<APIGatewayProxyResult> => {
	context.callbackWaitsForEmptyEventLoop = false;

	if (!event.body) {
		return {
			statusCode: 400,
			body: "Bad Request, body cannot be empty"
		};
	} else {
		let product: Product = JSON.parse(event.body!);
		try {
			var item = await service.updateItem(product);
			return { statusCode: 200, body: JSON.stringify(item) };
		} catch (e) {
			return {
				statusCode: 500,
				body: "Server failure, unable to retrieve item"
			};
		}
	}
};
