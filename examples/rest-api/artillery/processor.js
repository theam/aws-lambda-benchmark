"use strict";
var uuid = require("uuid");
var casual = require("casual");

module.exports = {
	provisionTestData
};

function provisionTestData(userContext, _events, done) {
	userContext.vars.sku = casual.word;
  userContext.vars.name = casual.words(Math.floor(Math.random() * 3) + 1);
	userContext.vars.newName = casual.words(Math.floor(Math.random() * 3) + 1);
	userContext.vars.description = casual.sentence;
	userContext.vars.newDescription = casual.sentence;
	return done();
}
