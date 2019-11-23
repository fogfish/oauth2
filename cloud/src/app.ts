import * as cdk from '@aws-cdk/core'
import * as pure from 'aws-cdk-pure'
import * as lambda from '@aws-cdk/aws-lambda'
import { staticweb, gateway } from 'aws-cdk-pure-hoc'
import { Auth } from './auth'
import { Token } from './token'
import { Client } from './client'
import { DDB } from './storage'

//-----------------------------------------------------------------------------
//
// Config
//
//-----------------------------------------------------------------------------
const app = new cdk.App()
const vsn = app.node.tryGetContext('vsn') || 'dev'
const stack = {
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region: process.env.CDK_DEFAULT_REGION
  }
}

//-----------------------------------------------------------------------------
//
// API Gateway
//
//-----------------------------------------------------------------------------
const oauth2 = new cdk.Stack(app, `oauth2-api-${vsn}`, { ...stack })

//
const api = staticweb.Gateway({
  domain: 'fog.fish',
  subdomain: `${vsn}.auth`,
  sites: [
    {
      origin: 'signin',
      site: 'api/oauth2/authorize'
    },
    {
      origin: 'account',
      site: 'api/oauth2/account'
    }
  ]
})

//
const Layer = (): pure.IPure<lambda.ILayerVersion> => {
  const LAYER='erlang-serverless:4'
  const iaac = pure.include(lambda.LayerVersion.fromLayerVersionArn)
  const AuthLayer= (): string => `arn:aws:lambda:${cdk.Aws.REGION}:${cdk.Aws.ACCOUNT_ID}:layer:${LAYER}`
  return iaac(AuthLayer)
}


pure.join(oauth2,
  pure.use({ api, runtime: Layer() })
    .flatMap(x => ({
      auth: Auth([x.runtime]),
      token: Token([x.runtime]),
      client: Client([x.runtime])
    }))
    .effect(x => {
      const oauth2 = x.api.root.getResource('oauth2')
      oauth2.addResource('signin').addMethod('POST', x.auth)
      oauth2.addResource('signup').addMethod('POST', x.auth)
      
      oauth2.addResource('token').addMethod('POST', x.token)

      const client = gateway.CORS(oauth2.addResource('client'))
      client.addMethod('GET', x.client)
      client.addMethod('POST', x.client)
      gateway.CORS(client.addResource('{id}')).addMethod('DELETE', x.client)
    })
)

//-----------------------------------------------------------------------------
//
// Storage
//
//-----------------------------------------------------------------------------
const dev = new cdk.Stack(app, `oauth2-db-dev`, { ...stack })
pure.join(dev, DDB())

const live = new cdk.Stack(app, `oauth2-db-live`, { ...stack })
pure.join(live, DDB())

app.synth()