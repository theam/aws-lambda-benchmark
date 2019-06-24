namespace Models
{
    public class Product
    {
        public string sku { get; set; }
        public string name { get; set; }
        public string description { get; set; }
        public string createdAt { get; set; }

        public Product()
        {
            sku = "";
            name = "";
            description = "";
            createdAt = "";
        }
    }
}
