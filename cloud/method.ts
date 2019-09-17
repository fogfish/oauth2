import * as cdk from '@aws-cdk/core'
import * as logs from '@aws-cdk/aws-logs'
import * as lambda from '@aws-cdk/aws-lambda'
import * as security from './security'

const LAYER='erlang-serverless:3'

//
// https://tools.ietf.org/html/rfc6749
//   Section 4.1.1.  Authorization Request
export function Auth(parent: cdk.Construct): lambda.FunctionProps {
  return {
    runtime: lambda.Runtime.PROVIDED,
    code: new lambda.AssetCode('../apps/auth/_build/default/bin'),
    handler: 'index.main',
    timeout: cdk.Duration.seconds(10),
    memorySize: 256,
    role: security.Writer()(parent),
    logRetention: logs.RetentionDays.FIVE_DAYS,
    layers: [
      lambda.LayerVersion.fromLayerVersionArn(parent, 'Layer', 
        `arn:aws:lambda:${cdk.Aws.REGION}:${cdk.Aws.ACCOUNT_ID}:layer:${LAYER}`)
    ]
  }
}