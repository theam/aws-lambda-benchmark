using System;
using Amazon.Lambda.Core;
using Amazon.Lambda.Serialization.Json;
using System.Threading.Tasks;

namespace dotnet21
{
    public class Hello
    {
        [LambdaSerializer(typeof(JsonSerializer))]
        public async Task<Object> Handler(Object request)
        {
            return new
            {
                statusCode = 200,
                body = "Hello"
            };
        }
    }
}