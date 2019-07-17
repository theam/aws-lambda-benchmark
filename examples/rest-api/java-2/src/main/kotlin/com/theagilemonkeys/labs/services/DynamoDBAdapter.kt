package com.theagilemonkeys.labs.services

import com.amazonaws.regions.Regions
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapper
import com.amazonaws.services.dynamodbv2.datamodeling.DynamoDBMapperConfig

class DynamoDBAdapter private constructor() {
    private val dbClient: AmazonDynamoDB? = AmazonDynamoDBClientBuilder.standard()
            .withRegion(Regions.US_EAST_1)
            .build()
    private var mapper: DynamoDBMapper? = null

    fun createDbMapper(mapperConfig: DynamoDBMapperConfig): DynamoDBMapper? {
        if (this.dbClient != null)
            mapper = DynamoDBMapper(this.dbClient, mapperConfig)

        return this.mapper
    }

    companion object {
        private var db_adapter: DynamoDBAdapter? = null

        val instance: DynamoDBAdapter?
            get() {
                if (db_adapter == null)
                    db_adapter = DynamoDBAdapter()

                return db_adapter
            }
    }
}