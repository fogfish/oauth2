// @doc
//   Authorization Request endpoint(s)
//   See https://tools.ietf.org/html/rfc6749
//      Section 4.1.1.  Authorization Request
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
export const Client = (): pure.IPure<api.LambdaIntegration> =>
  pure.wrap(api.LambdaIntegration)(
    pure.use({ layer: Layer(), role: Role()})
      .flatMap(x => ({ lambda: Lambda(x.role, [x.layer]) }))
      .yield('lambda')
  )

//
const Lambda = (role: iam.IRole, layers: lambda.ILayerVersion[]): pure.IPure<lambda.Function> => {
  const iaac = pure.iaac(lambda.Function)
  const Client = (): lambda.FunctionProps => ({
    runtime: lambda.Runtime.PROVIDED,
    code: new lambda.AssetCode('../apps/token/_build/default/bin'),
    handler: 'index.main',
    timeout: cdk.Duration.seconds(10),
    memorySize: 256,
    role,
    logRetention: logs.RetentionDays.FIVE_DAYS,
    layers,
    environment: {
      'PERMIT_ISSUER': 'https://auth.fog.fish',
      'PERMIT_AUDIENCE': 'any',
      'PERMIT_CLAIMS': 'uid=true',
      'PERMIT_KEYPAIR': 'permit_config_ddb',
      'PERMIT_STORAGE': `ddb+https://dynamodb.${cdk.Aws.REGION}.amazonaws.com:443/oauth2-db-dev-pubkey`
    }
  })
  return iaac(Client)
}

//
const Layer = (): pure.IPure<lambda.ILayerVersion> => {
  const iaac = pure.include(lambda.LayerVersion.fromLayerVersionArn)
  const ClientLayer= (): string => `arn:aws:lambda:${cdk.Aws.REGION}:${cdk.Aws.ACCOUNT_ID}:layer:${LAYER}`
  return iaac(ClientLayer)
}

//
const Role = (): pure.IPure<iam.IRole> => {
  const role = pure.iaac(iam.Role)
  const ClientRole = (): iam.RoleProps => ({
    assumedBy: new iam.ServicePrincipal('lambda.amazonaws.com')
  })

  const ReadWrite = (): iam.PolicyStatement => (
    new iam.PolicyStatement({
      actions: ['*'],
      resources: ['*'],
    })
  )

  return role(ClientRole).effect(x => x.addToPolicy(ReadWrite()))
}