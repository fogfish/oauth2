import * as lambda from '@aws-cdk/aws-lambda'
import * as cdk from '@aws-cdk/core'
import * as pure from 'aws-cdk-pure'
import { gateway, staticweb } from 'aws-cdk-pure-hoc'
import * as auth from './auth'
import * as client from './client'
import { DDB } from './storage'
import * as token from './token'

// ----------------------------------------------------------------------------
//
// Config
//
// ----------------------------------------------------------------------------
const app = new cdk.App()
const vsn: string = app.node.tryGetContext('vsn') || 'latest'
const domain: string = app.node.tryGetContext('domain')
const subdomain: string = `${vsn}.auth`
const email: string = app.node.tryGetContext('email')
const tlsCertificate: string = app.node.tryGetContext('cert')
const host: string = `${subdomain}.${domain}`
const stack = {
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region: process.env.CDK_DEFAULT_REGION
  }
}

// ----------------------------------------------------------------------------
//
// Storage Backend
//
// ----------------------------------------------------------------------------
const storage = (vsn.startsWith('pr') || vsn === 'latest')
  ? `oauth2-db-${vsn}`
  : `oauth2-db-live`

const dev = new cdk.Stack(app, storage, { ...stack })
const ddb = pure.join(dev, DDB())

// ----------------------------------------------------------------------------
//
// API Gateway
//
// ----------------------------------------------------------------------------
const oauth2 = new cdk.Stack(app, `oauth2-api-${vsn}`, { ...stack })

//
const api = staticweb.Gateway({
  domain,
  sites: [
    {
      origin: 'signin',
      site: 'api/oauth2/authorize',
    },
    {
      origin: 'account',
      site: 'api/oauth2/account',
    }
  ],
  tlsCertificate,
  subdomain,
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
      auth: auth.Function(host, email, ddb, [x.runtime]),
      client: client.Function(host, ddb, [x.runtime]),
      token: token.Function(host, ddb, [x.runtime]),
    }))
    .effect(x => {
      const apiOAuth2 = x.api.root.getResource('oauth2')
      if (apiOAuth2) {
        apiOAuth2.addResource('signin').addMethod('POST', x.auth)
        apiOAuth2.addResource('signup').addMethod('POST', x.auth)
        apiOAuth2.addResource('password').addMethod('POST', x.auth)
        
        apiOAuth2.addResource('token').addMethod('POST', x.token)
        apiOAuth2.addResource('introspect').addMethod('POST', x.token)

        const apiClient = gateway.CORS(apiOAuth2.addResource('client'))
        apiClient.addMethod('GET', x.client)
        apiClient.addMethod('POST', x.client)
        gateway.CORS(apiClient.addResource('{id}')).addMethod('DELETE', x.client)
      }
    })
)

app.synth()