package com.theagilemonkeys.labs.services

import com.theagilemonkeys.labs.model.Product

interface ProductService {
    fun create(sku: String, name: String, description: String?): Product
    fun update(sku: String, name: String?, description: String?): Product
    fun getAll(): List<Product>
    fun getBySku(sku: String): Product?
    fun delete(sku: String)
}