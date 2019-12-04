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
const vsn: string = app.node.tryGetContext('vsn') || 'latest'
const domain: string = app.node.tryGetContext('domain')
const email: string = app.node.tryGetContext('email')
const host: string = `${vsn}.${domain}`
const stack = {
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region: process.env.CDK_DEFAULT_REGION
  }
}

//-----------------------------------------------------------------------------
//
// Storage Backend
//
//-----------------------------------------------------------------------------
const storage = (vsn.startsWith('pr') || vsn === 'latest')
  ? `oauth2-db-${vsn}`
  : `oauth2-db-live`

const dev = new cdk.Stack(app, storage, { ...stack })
const ddb = pure.join(dev, DDB())

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
      auth: Auth(host, email, ddb, [x.runtime]),
      token: Token(host, ddb, [x.runtime]),
      client: Client(host, ddb, [x.runtime]),
    }))
    .effect(x => {
      const oauth2 = x.api.root.getResource('oauth2')
      if (oauth2) {
        oauth2.addResource('signin').addMethod('POST', x.auth)
        oauth2.addResource('signup').addMethod('POST', x.auth)
        oauth2.addResource('password').addMethod('POST', x.auth)
        
        oauth2.addResource('token').addMethod('POST', x.token)
        oauth2.addResource('introspect').addMethod('POST', x.token)

        const client = gateway.CORS(oauth2.addResource('client'))
        client.addMethod('GET', x.client)
        client.addMethod('POST', x.client)
        gateway.CORS(client.addResource('{id}')).addMethod('DELETE', x.client)
      }
    })
)

app.synth()