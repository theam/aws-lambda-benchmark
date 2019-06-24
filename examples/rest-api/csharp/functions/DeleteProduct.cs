using Amazon.Lambda.Core;
using System;
using Amazon.Lambda.APIGatewayEvents;
using System.Threading.Tasks;
using RestApiServices;
using Amazon.DynamoDBv2.DocumentModel;

namespace RestApi
{
    public class DeleteProduct
    {
        ProductService service = new ProductService(Environment.GetEnvironmentVariable("PRODUCTS_TABLE_NAME"));

        [LambdaSerializer(typeof(Amazon.Lambda.Serialization.Json.JsonSerializer))]
        public async Task<APIGatewayProxyResponse> Handler(APIGatewayProxyRequest request, ILambdaContext context)
        {
            string sku = request.PathParameters["sku"];
            try
            {
                Document item = await service.DeleteItem(sku);

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
