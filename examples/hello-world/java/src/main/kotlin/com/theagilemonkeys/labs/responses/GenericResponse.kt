package com.theagilemonkeys.labs.responses

import com.fasterxml.jackson.databind.ObjectMapper

open class GenericResponse {
    fun toJSON() = ObjectMapper().writeValueAsString(this)
}
