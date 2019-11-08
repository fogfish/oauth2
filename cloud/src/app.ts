import * as cdk from '@aws-cdk/core'
import * as pure from 'aws-cdk-pure'
import { staticweb } from 'aws-cdk-pure-hoc'
import { Auth } from './auth'
import { Token } from './token'
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
const gateway = new cdk.Stack(app, `oauth2-api-${vsn}`, { ...stack })

//
const api = staticweb.Gateway({
  domain: 'fog.fish',
  subdomain: `${vsn}.auth`,
  siteRoot: 'api/oauth2/authorize',
})
const auth = Auth()
const token = Token()

pure.join(gateway,
  pure.use({ api, auth, token })
    .effect(x => {
      const oauth2 = x.api.root.getResource('oauth2')
      oauth2.addResource('signin').addMethod('POST', x.auth)
      oauth2.addResource('signup').addMethod('POST', x.auth)
      oauth2.addResource('token').addMethod('POST', x.token)
      
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