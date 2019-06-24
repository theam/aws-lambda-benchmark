using System;
using System.Threading.Tasks;
using Amazon.DynamoDBv2;
using Amazon.DynamoDBv2.DocumentModel;
using Amazon.DynamoDBv2.Model;
using Models;

namespace RestApiServices
{
    public class ProductService
    {
        private readonly AmazonDynamoDBClient dbClient;
        private Table table;
        private string tableName;

        public ProductService(string tableName)
        {
            dbClient = new AmazonDynamoDBClient();
            this.tableName = tableName;
            table = Table.LoadTable(dbClient, tableName);
        }

        public async Task<Document> GetItem(string sku)
        {
            return await table.GetItemAsync(sku);
        }

        public async Task<Document> DeleteItem(string sku)
        {
            return await table.DeleteItemAsync(sku);
        }

        public async Task<ScanResponse> ListItems()
        {
            ScanRequest scanRequest = new ScanRequest {
                TableName = tableName
            };
            return await dbClient.ScanAsync(scanRequest);
        }

        public async Task<Document> CreateItem(Product product)
        {
            string currentTime = DateTime.UtcNow.ToString("o");
            Document item = new Document();
            item.Add("sku", product.sku);
            item.Add("name", product.name);
            item.Add("description", product.description);
            item.Add("createdAt", currentTime);
            item.Add("updatedAt", currentTime);
            return await table.PutItemAsync(item);
        }

        public async Task<Document> UpdateItem(Product product)
        {
            string currentTime = DateTime.UtcNow.ToString("o");
            Document item = new Document();
            item.Add("sku", product.sku);
            item.Add("name", product.name);
            item.Add("description", product.description);
            item.Add("updatedAt", currentTime);
            return await table.UpdateItemAsync(item);
        }
    }
}