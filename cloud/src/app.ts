import * as cdk from '@aws-cdk/core'
import * as pure from 'aws-cdk-pure'
import { staticweb } from 'aws-cdk-pure-hoc'
import { Auth } from './auth'
import { DDB } from './storage'

const app = new cdk.App()
const vsn = process.env.VSN || 'local'

//-----------------------------------------------------------------------------
//
// API Gateway
//
//-----------------------------------------------------------------------------
const gateway = new cdk.Stack(app, `oauth2-api-${vsn}`, {
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region: process.env.CDK_DEFAULT_REGION
  }
})

//
const api = staticweb.Gateway({
  domain: 'fog.fish',
  subdomain: `${vsn}.auth`,
  siteRoot: 'api/oauth2/authorize',
})
const auth = Auth()

pure.join(gateway,
  pure.use({ api, auth })
    .effect(x => {
      const oauth2 = x.api.root.getResource('oauth2')
      oauth2.addResource('signin').addMethod('POST', x.auth)
      oauth2.addResource('signup').addMethod('POST', x.auth)
    })
)

//-----------------------------------------------------------------------------
//
// Storage
//
//-----------------------------------------------------------------------------
const dev = new cdk.Stack(app, `oauth2-db-dev`, {
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region: process.env.CDK_DEFAULT_REGION
  }
})
pure.join(dev, DDB())

const live = new cdk.Stack(app, `oauth2-db-live`, {
  env: {
    account: process.env.CDK_DEFAULT_ACCOUNT,
    region: process.env.CDK_DEFAULT_REGION
  }
})
pure.join(live, DDB())

app.synth()