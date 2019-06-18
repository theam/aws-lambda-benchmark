package com.theagilemonkeys.labs.services

import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBQueryExpression
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBScanExpression
import com.amazonaws.services.dynamodbv2.model.AttributeValue
import com.theagilemonkeys.labs.model.Product
import java.io.IOException
import java.util.*


class ProductService {
    private var dbMapper: DynamoDBMapper

    init {
        val mapperConfig = DynamoDBMapperConfig.builder()
                .withTableNameOverride(DynamoDBMapperConfig.TableNameOverride(Product.PRODUCTS_TABLE_NAME))
                .build()
        dbMapper = DynamoDBAdapter.instance!!.createDbMapper(mapperConfig)!!
    }

    @Throws(IOException::class)
    fun createProduct(sku: String, name: String, description: String?): Product {
        getProduct(sku)?.let {
            // TODO: better exception handling
            throw(IOException("Product already exist"))
        }
        val product = Product(sku, name, description)
        dbMapper.save(Product(sku, name, description))
        return product
    }

    @Throws(IOException::class)
    fun updateProduct(sku: String, name: String?, description: String?): Product {
        getProduct(sku)?.let {
            it.description = description ?: it.description
            it.name = name ?: it.name
            dbMapper.save(it)
            return@let it
        }
        throw(IOException("Product not found"))
    }

    @Throws(IOException::class)
    fun getProducts(): List<Product> {
        val scanExp = DynamoDBScanExpression()
        return dbMapper.scan(Product::class.java, scanExp)
    }

    @Throws(IOException::class)
    fun getProduct(sku: String): Product? {
        var product: Product? = null
        val params = HashMap<String, AttributeValue>()
        params[":v1"] = AttributeValue().withS(sku)

        val queryExp = DynamoDBQueryExpression<Product>()
                .withKeyConditionExpression("sku = :v1")
                .withExpressionAttributeValues(params)

        val result = dbMapper.query(Product::class.java, queryExp)

        if (result.size > 0) {
            product = result[0]
        }
        return product
    }

    @Throws(IOException::class)
    fun deleteProduct(sku: String)  {
        val product = getProduct(sku)
        dbMapper.delete(product)
    }
}