using Amazon.Lambda.Core;
using System;
using Amazon.Lambda.APIGatewayEvents;
using System.Threading.Tasks;
using RestApiServices;
using Newtonsoft.Json;

namespace RestApi
{
    public class ListProducts
    {
        ProductService service = new ProductService(Environment.GetEnvironmentVariable("PRODUCTS_TABLE_NAME"));

        [LambdaSerializer(typeof(Amazon.Lambda.Serialization.Json.JsonSerializer))]
        public async Task<APIGatewayProxyResponse> Handler(APIGatewayProxyRequest request, ILambdaContext context)
        {

            var item = await service.ListItems();

            return new APIGatewayProxyResponse
            {
                StatusCode = 200,
                Body = JsonConvert.SerializeObject(item.Items)
            };

        }
    }
}
