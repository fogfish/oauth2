import { IaaC, iaac, wrap, use } from 'aws-cdk-pure'
import * as api from '@aws-cdk/aws-apigateway'
import * as lambda from '@aws-cdk/aws-lambda'
import { Auth } from './method'

const gateway = iaac(api.RestApi)
const service = iaac(lambda.Function)
const method  = wrap(api.LambdaIntegration)

function OAuth2(): api.RestApiProps {
  return {
    endpointTypes: [api.EndpointType.REGIONAL],
    deploy: true,
    deployOptions: {
      stageName: 'oauth2'
    },
    failOnWarnings: true
  }
}

export function RestApi(): IaaC<api.RestApi> {
  return use({
    restapi: gateway(OAuth2),
    auth: method(service(Auth))
  })
  .effect(x => {
    x.restapi.root.addResource('signin').addMethod('POST', x.auth)
    x.restapi.root.addResource('signup').addMethod('POST', x.auth)
  })
  .yield('restapi')
}
