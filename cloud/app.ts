import * as cdk from '@aws-cdk/core'
import { root, join, flat } from 'aws-cdk-pure'
import { RestApi } from './restapi'

function OAuth2(stack: cdk.Construct): cdk.Construct {
  join(stack, flat(RestApi))
  return stack
}

const app = new cdk.App()
root(app, OAuth2, `OAuth2-${process.env.VSN || 'local'}`)
app.synth()