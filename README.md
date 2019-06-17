# aws-lambda-benchmark
A project that contains AWS Lambda function implementations for several runtimes e.g. Nodejs, Haskell, Python, Go, Rust, Java, etc.

## Examples
[Hello World](examples/hello-world/setup.md)

## Triggering your function through API Gateway

First of all, we will need to create a few resources before we can trigger our Lambda Function. Go to `API Gateway` in the AWS Console.

- Create a new API, select `Rest` and from `New API` and choose a name for your API.
- Then create a `stage` named e.g. `dev`
- Create a `resource` named e.g. `nodejs-hello` and enable `CORS`
- Within that resource, create a `method` `GET`, enable `Use Lambda Proxy Integration` and type the name of your function under `Lambda Function`
- Finally click `save` and click `deploy` under the dropdown menu of `Actions`
- Your endpoint URL will be: <Method> <stage-invoke-url>/<resource-name>, e.g. GET https://0c9lfg7004.execute-api.us-east-1.amazonaws.com/dev/nodejs-hello

## Using Artillery for testing

If you don't have artillery install,

`yarn global add artillery`
or
`npm install --global artillery`

and then run a quick test that will perform 10 rps per second during 10 seconds coming from 10 different sources each second

`artillery quick --duration 10 --rate 10 -n 1 https://0c9lfg7004.execute-api.us-east-1.amazonaws.com/dev/nodejs-hello`

## Creating a Dashboard using AWS CloudWatch
AWS CloudWatch is the service where you could find Analytics about your Lambda function. Information about execution time, # invocations, # errors or # throttles. Go to `AWS CloudWatch` in the AWS Console.

- Click on `Dashboards` and `Create Dashboard`
- Create the first `Widget` of type `Number`
- Select `Lambda` as the source for your metric
- Select `By Function Name`
- And click on all the metrics that you want to track for you would like to track e.g. `Duration`, `Errors`
- Click `Create Widget` and you will see the metrics being displayed

![](assets/cloudwatch/cloudwatch-lambda-metrics.png)

In this example, we selected the following metrics:

![](assets/cloudwatch/cloudwatch-lambda-metrics-selection.png)

