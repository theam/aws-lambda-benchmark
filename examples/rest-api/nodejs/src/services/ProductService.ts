import AWS from "aws-sdk";
import Product from "../models/Product";

class ProductService {
	db: any;
	tableName: string;

	constructor(tableName: string | undefined, db?: any) {
		this.db = db ? db : new AWS.DynamoDB.DocumentClient();
		this.tableName = tableName ? tableName : "";
	}

	getItem(key: string): Promise<any> {
		var params = {
			Key: {
				sku: key
			},
			TableName: this.tableName
		};

		return this.db.get(params).promise();
	}

	createItem(product: Product): Promise<any> {
		var params = {
			Item: product,
			TableName: this.tableName
		};

		return this.db.put(params).promise();
	}

	updateItem(product: Product): Promise<any> {
		var params = {
			ExpressionAttributeNames: {
				"#name": "name",
				"#description": "description"
			},
			ExpressionAttributeValues: {
				":name": product.name,
				":description": product.description
			},
			UpdateExpression: "SET #name = :name, #description = :description",
			Key: {
				sku: product.sku
			},
			ReturnValues: "ALL_NEW",
			TableName: this.tableName
		};

		return this.db.update(params).promise();
	}

	deleteItem(key: string): Promise<any> {
		var params = {
			Key: {
				sku: key
			},
			TableName: this.tableName
		};

		return this.db.delete(params).promise();
	}

	listItems(): Promise<any> {
		var params = {
			TableName: this.tableName
		};

		return this.db.scan(params).promise();
	}
}
export default ProductService;
