# Hello world Example
In this example we will just deploy manually a function in AWS Lambda with our desired runtime. Some of the runtimes will be custom, as a result, it will require a bit more work.

## Runtime Index
- [Nodejs 8.10](#nodejs-810)
- [Python 3.6](#python-36)

## Setting up Lambda IAM Role

We need to create a very basic IAM Role that we will assign to each of the functions we create. We will go to `IAM Role` in the AWS Console to get started.

In order to allow logging and invoking Lambda functions we will add the following `CloudWatch Logs` and `Lambda` permissions to the IAM role:
```
logs:CreateLogStream
lambda:InvokeFunction
logs:CreateLogGroup
logs:PutLogEvents
```

Additionally, we will also need to define a `Trust Relationships` in order to allow API Gateway to invoke our function:
```
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "",
      "Effect": "Allow",
      "Principal": {
        "Service": [
          "apigateway.amazonaws.com",
          "lambda.amazonaws.com"
        ]
      },
      "Action": "sts:AssumeRole"
    }
  ]
}
```

## Nodejs 8.10
- Create a `Function from Scratch` and choose a name for the function, e.g. `benchmark-nodejs-hello`
- Choose the correct runtime `Node.js 8.10`
- Select the `execution role` that you just created above
- Click `Create the function`
- Paste the following code
```
exports.handler = async (event, context, callback) => {

    var response = {
        statusCode: 200,
        body: JSON.stringify("hello"),
        isBase64Encoded: false
    };
    
    callback(null, response)
};

```
- Input the `Handler`
`index.handler`
- `Save` and Test the event by clicking `Test` in the top right corner

## Python 3.6
- Create a `Function from Scratch` and choose a name for the function, e.g. `benchmark-python-hello`
- Choose the correct runtime `Python 3.6`
- Select the `execution role` that you just created above
- Click `Create the function`
- Paste the following code

```
exports.handler = async (event, context, callback) => {

    var response = {
        statusCode: 200,
        body: JSON.stringify("hello"),
        isBase64Encoded: false
    };
    
    callback(null, response)
};
```

- Input the `Handler`
`lambda_function.lambda_handler`
- `Save` and Test the event by clicking `Test` in the top right corner
