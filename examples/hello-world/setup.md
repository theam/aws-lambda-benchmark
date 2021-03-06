# Hello world Example

In this example we will just deploy manually a function in AWS Lambda with our desired runtime. Some of the runtimes will be custom, as a result, it will require a bit more work.

Note: We have tested all these functions with 1028 MB of memory


## Runtime Index

- [Nodejs 8.10](#nodejs-810)
- [Python 3.6](#python-36)
- [Java](#java)
- [Rust](#rust)
- [Haskell](#Haskell)
- [Haskell (Runtime Bootstrapped)](#Haskell-runtime-Bootstrapped)
- [Go](#go)
- [Ruby](#ruby-25)
- [C#](#c-net-21)
- [F#](#f-net-21)


## Setting up Lambda IAM Role

We need to create a very basic IAM Role that we will assign to each of the functions we create. We will go to `IAM Role` in the AWS Console to get started.

In order to allow logging we will add the following `CloudWatch Logs` permissions to the IAM role:

```
logs:CreateLogStream
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
- Paste the following code:

```
exports.handler = async () => ({
  statusCode: 200,
  body: `"hello"`,
  isBase64Encoded: false
});
```

- Input the `Handler`
`index.handler`
- `Save` and Test the event by clicking `Test` in the top right corner


## Python 3.6

- Create a `Function from Scratch` and choose a name for the function, e.g. `benchmark-python-hello`
- Choose the correct runtime `Python 3.6`
- Select the `execution role` that you just created above
- Click `Create the function`
- Paste the following code:

```
import json

def lambda_handler(event, context):
    return {
        'statusCode': 200,
        'body': json.dumps('Hello')
    }
```

- Input the `Handler`
`lambda_function.lambda_handler`
- `Save` and Test the event by clicking `Test` in the top right corner


## Java

- First of all, install [Gradle](https://gradle.org/install/)
- Change directory to `examples/hello-world/java`
- Build the project, `gradle build`
- Go to `AWS Lambda` in AWS Console
- Create new function from Scratch
- Enter function name and select `Java 8` as runtime
- Choose the execution role we created above
- `Upload the zip file benchmarks-java-dev-all.jar` we created as function code
- Input `com.theagilemonkeys.labs.HelloWorldHandler` as the `Handler`
- Click `Save`
- Test the function by clicking `Test` in the top right corner


## Rust

- First of all, install [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)
- Install musl-cross `brew install filosottile/musl-cross/musl-cross`
- Set link: `ln -s /usr/local/bin/x86_64-linux-musl-gcc /usr/local/bin/musl-gcc`
- Change directory to `examples/hello-world/rust`
- Build function: `cargo build --release --target x86_64-unknown-linux-musl`
- Zip function: `zip -j rust.zip ./target/x86_64-unknown-linux-musl/release/bootstrap`
- Go to `AWS Lambda` in AWS Console
- Create new function from Scratch
- Enter function name and select `Provide your own bootstrap`
- Choose the execution role we created above
- `Upload the zip file rust.zip` we created as function code and click `save`
- Test the function by clicking `Test` in the top right corner

**Note: In this function we do not need to specify the handler since all you need is in the binary**


## Haskell

- First of all, install [Stack](https://docs.haskellstack.org/en/stable/README/)
- Install [Docker](https://docs.docker.com/docker-for-mac/install/)
- Download Docker Stack image `docker pull fpco/stack-build:lts-13.25`
- build project by running `make` in `examples/hello-world/haskell`
- Go to `AWS Lambda` in AWS Console
- Create new function from Scratch
- Enter function name and select `Provide your own bootstrap`
- Choose the execution role we created above
- `Upload the zip file function.zip` located in `./build` as function code
- Enter `src/Lib.handler` as the `Handler`
- Click `Save` and then Test the function by clicking `Test` in the top right corner


## Haskell runtime Bootstrapped

TODO


## Go

- Install [Go](https://golang.org/dl/)
- Create a `hello.go` file with the following content:

```
package main

import "github.com/aws/aws-lambda-go/lambda"

type Response struct {
    StatusCode      int    `json:"statusCode"`
    Body            string `json:"body"`
    IsBase64Encoded bool   `json:"isBase64Encoded"`
}

func hello() (Response, error) {
    return Response{
        StatusCode:      200,
        Body:            "Hello world",
        IsBase64Encoded: false,
    }, nil
}

func main() {
    lambda.Start(hello)
}
```

- Build the program for AWS Lambda: `GOOS=linux GOARCH=amd64 go build hello.go`
- Zip the binnary: `zip hello.zip ./hello
- Go to `AWS Lambda` in AWS Console and create a new function from scratch
- Enter function name "benchmark-go-hello" and select "Go 1.x" as runtime
- Choose the execution role created above
- Upload the zip file we created as function code
- Input "hello" in "Handler" (it should be the name of the executable)
- Click "Save"
- Test the function by clicking `Test` in the top right corner


## Ruby 2.5

- Create a `Function from Scratch` and choose a name for the function, e.g. `benchmark-python-hello`
- Choose the correct runtime `Ruby 2.5`
- Select the `execution role` that you just created above
- Click `Create the function`
- Paste the following code

```
require 'json'

def lambda_handler(event:, context:)
    { statusCode: 200, body: JSON.generate('Hello') }
end
```

- Input the `Handler`
`lambda_function.lambda_handler`
- `Save` and Test the event by clicking `Test` in the top right corner


## C# .NET 2.1

- Install [.NET](https://dotnet.microsoft.com/) in your machine
- Install `C#` extension for VSCode
- Change directory to `examples/hello-world/dotnet21`
- Build the project, `dotnet build`
- Zip the project to upload it to AWS Lambda, `dotnet lambda package -c Release -o ./HelloWorldLambda.zip -f netcoreapp2.1`
- Go to `AWS Lambda` in AWS Console and create a new function from scratch
- Enter function name "benchmark-csharp-hello" and select ".NET Core 2.1 (C#/PowerShell)" as runtime
- Choose the execution role created above
- Upload the zip file we created as function code
- Input "dotnet21::dotnet21.Hello::Handler" in "Handler"
- Click "Save"
- Test the function by clicking `Test` in the top right corner


## F# .NET 2.1

- Install [.NET](https://dotnet.microsoft.com/) in your machine
- Install Amazon.Lambda.Tools Global Tools `dotnet tool install -g Amazon.Lambda.Tools`
- Install `C#` extension for VSCode
- Change directory to `examples/hello-world/fsharp`
- Build the project, `dotnet build`
- Deploy function, `dotnet lambda deploy-function <your-function-name>`
