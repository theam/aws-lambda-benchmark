package com.theagilemonkeys.labs.responses

import com.google.gson.Gson


open class GenericResponse {
    @Transient
    private var gsonMapper: Gson = Gson()
            .newBuilder()
            .setDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'").create()

    fun toJSON(): String {
        return gsonMapper.toJson(this)
    }
}
