import ProductService from "../services/ProductService";
import {
	APIGatewayProxyEvent,
	APIGatewayProxyResult,
	Context
} from "aws-lambda";

var service = new ProductService(process.env.PRODUCTS_TABLE_NAME);
export const handler = async (
	_event: APIGatewayProxyEvent,
	context: Context
): Promise<APIGatewayProxyResult> => {
	context.callbackWaitsForEmptyEventLoop = false;

	try {
		var items = await service.listItems();
		return { statusCode: 200, body: JSON.stringify(items) };
	} catch (e) {
		return {
			statusCode: 500,
			body: "Server failure, unable to retrieve item"
		};
	}
};
