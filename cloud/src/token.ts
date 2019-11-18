import * as cdk from '@aws-cdk/core'
import * as logs from '@aws-cdk/aws-logs'
import * as lambda from '@aws-cdk/aws-lambda'
import * as iam from '@aws-cdk/aws-iam'
import * as pure from 'aws-cdk-pure'
import * as api from '@aws-cdk/aws-apigateway'

// TODO: global config
const LAYER='erlang-serverless:4'

// 
//   
export const Token = (): pure.IPure<api.LambdaIntegration> =>
  pure.wrap(api.LambdaIntegration)(
    pure.use({ layer: Layer(), role: Role()})
      .flatMap(x => ({ lambda: Lambda(x.role, [x.layer]) }))
      .yield('lambda')
  )

//
const Lambda = (role: iam.IRole, layers: lambda.ILayerVersion[]): pure.IPure<lambda.Function> => {
  const iaac = pure.iaac(lambda.Function)
  const Token = (): lambda.FunctionProps => ({
    runtime: lambda.Runtime.PROVIDED,
    code: new lambda.AssetCode('../apps/token/_build/default/bin'),
    handler: 'index.main',
    timeout: cdk.Duration.seconds(10),
    memorySize: 256,
    role,
    logRetention: logs.RetentionDays.FIVE_DAYS,
    reservedConcurrentExecutions: 5,
    layers,
    environment: {
      'PERMIT_ISSUER': 'https://auth.fog.fish',
      'PERMIT_AUDIENCE': 'any',
      'PERMIT_CLAIMS': 'uid=true',
      'PERMIT_KEYPAIR': 'permit_config_ddb',
      'PERMIT_STORAGE': `ddb+https://dynamodb.${cdk.Aws.REGION}.amazonaws.com:443/oauth2-db-dev-pubkey`
    }
  })
  return iaac(Token)
}

//
const Layer = (): pure.IPure<lambda.ILayerVersion> => {
  const iaac = pure.include(lambda.LayerVersion.fromLayerVersionArn)
  const TokenLayer= (): string => `arn:aws:lambda:${cdk.Aws.REGION}:${cdk.Aws.ACCOUNT_ID}:layer:${LAYER}`
  return iaac(TokenLayer)
}

//
const Role = (): pure.IPure<iam.IRole> => {
  const role = pure.iaac(iam.Role)
  const TokenRole = (): iam.RoleProps => ({
    assumedBy: new iam.ServicePrincipal('lambda.amazonaws.com')
  })

  const ReadWrite = (): iam.PolicyStatement => (
    new iam.PolicyStatement({
      actions: ['*'],
      resources: ['*'],
    })
  )

  return role(TokenRole).effect(x => x.addToPolicy(ReadWrite()))
}