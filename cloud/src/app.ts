import * as cdk from '@aws-cdk/core'
import * as pure from 'aws-cdk-pure'
import { staticweb } from 'aws-cdk-pure-hoc'
import { Auth } from './auth'

//
const vsn = process.env.VSN || 'local'

//
const app = new cdk.App()
const stack = new cdk.Stack(app, `oauth2-${vsn}`, {
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

pure.join(stack,
  pure.use({ api, auth })
    .effect(x => {
      const oauth2 = x.api.root.getResource('oauth2')
      oauth2.addResource('signin').addMethod('POST', x.auth)
      oauth2.addResource('signup').addMethod('POST', x.auth)
    })
)
app.synth()