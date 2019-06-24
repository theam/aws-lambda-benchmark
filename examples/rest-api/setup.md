# CRUD Example
In this example we will deploy functions to be able to read/write/delete/update DynamoDB table items.

# Examples
- [Java](#java)
- [Python](#python)
- [Haskell](#haskell)
- [C#](#c)

## Java

## Python
- First of all, install Serverless Framework if you dont have it yet
```bash
npm install --global serverless
```
or
```bash
yarn global add serverless
```
- Deploy the stack, `sls deploy`

## Haskell
Building and deploying the Haskell CRUD is quite straight forward, however, there are a set of pre-requisites:
- First of all, install [Stack](https://docs.haskellstack.org/en/stable/README/)
- Install [Docker](https://docs.docker.com/docker-for-mac/install/)
- Download Docker Stack image `docker pull fpco/stack-build:lts-13.25`

Then
- Go to the Haskell RestAPI directory, `cd examples/rest-api/haskell`
- run `./deploy`
> `Deploy` will build all the functions of the project and then deploy the stack through serverless framework.

> Troubleshooting: If you experience some errors while building, try restarting Docker.

## C#
- Install DotNet 2.1 if you don't have it
- Install Serverless Framework and configure it
```bash
npm install --global serverless
```
or
```bash
yarn add global serverless
```
- Change directory to `examples/rest-api/csharp`
- Run the following script:
```bash
./deploy.sh
```
alternativelly, you could also run
```bash
./build.sh
sls deploy
```