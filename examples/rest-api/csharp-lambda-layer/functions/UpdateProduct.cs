using Amazon.Lambda.Core;
using System;
using Amazon.Lambda.APIGatewayEvents;
using System.Threading.Tasks;
using RestApiServices;
using Newtonsoft.Json;
using Models;
using Amazon.DynamoDBv2.DocumentModel;

namespace RestApi
{
    public class UpdateProduct
    {
        ProductService service = new ProductService(Environment.GetEnvironmentVariable("PRODUCTS_TABLE_NAME"));

        [LambdaSerializer(typeof(Amazon.Lambda.Serialization.Json.JsonSerializer))]
        public async Task<APIGatewayProxyResponse> Handler(APIGatewayProxyRequest request, ILambdaContext context)
        {
            var body = request.Body;
            Product product = JsonConvert.DeserializeObject<Product>(body);

            if (string.IsNullOrEmpty(product.sku))
            {
                return new APIGatewayProxyResponse
                {
                    StatusCode = 400,
                    Body = "sku cannot be empty"
                };
            }

            try
            {
                Document item = await service.UpdateItem(product);

                return new APIGatewayProxyResponse
                {
                    StatusCode = 200,
                    Body = item == null ? "" : item.ToJson()
                };
            }
            catch (AggregateException e)
            {
                Console.WriteLine(e);
                return new APIGatewayProxyResponse
                {
                    StatusCode = 500,
                    Body = ""
                };
            }
        }
    }
}
