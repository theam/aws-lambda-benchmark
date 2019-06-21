# CRUD Example
In this example we will deploy functions to be able to read/write/delete/update DynamoDB table items.

# Examples
- [Java](#java)
- [Python](#python)
- [Haskell](#haskell)

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
