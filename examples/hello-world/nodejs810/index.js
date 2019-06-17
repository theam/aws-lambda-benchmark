exports.handler = async (event, context, callback) => {

    var response = {
        statusCode: 200,
        body: JSON.stringify("hello"),
        isBase64Encoded: false
    };
    
    callback(null, response)
};
