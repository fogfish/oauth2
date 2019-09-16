import * as cdk from '@aws-cdk/core'
import * as logs from '@aws-cdk/aws-logs'
import * as lambda from '@aws-cdk/aws-lambda'
import * as security from './security'

const LAYER='erlang-serverless:3'

export function SignIn(parent: cdk.Construct): lambda.FunctionProps {
  return {
    runtime: lambda.Runtime.PROVIDED,
    code: new lambda.AssetCode('../rel'),
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