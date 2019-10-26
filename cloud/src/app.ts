import * as cdk from '@aws-cdk/core'
import * as pure from 'aws-cdk-pure'
import { staticweb } from 'aws-cdk-pure-hoc'
import { Auth } from './auth'

//
const api = staticweb.Gateway({
  domain: 'fog.fish',
  subdomain: 'auth',
  siteRoot: 'oauth2/authorize',
})

const auth = Auth()

pure.use({ api, auth })
  .effect(x => x.api.root.addResource('signin').addMethod('POST', x.auth))
  .effect(x => x.api.root.addResource('signup').addMethod('POST', x.auth))

//
const Stack = (): cdk.StackProps => ({
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region: process.env.CDK_DEFAULT_REGION
  }
})
const stack = pure.iaac(cdk.Stack)(Stack)
  .effect(x => pure.join(x, api)
)

//
const app = new cdk.App()
pure.join(app, stack)
