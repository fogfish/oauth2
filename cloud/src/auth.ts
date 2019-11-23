import * as cdk from '@aws-cdk/core'
import * as logs from '@aws-cdk/aws-logs'
import * as lambda from '@aws-cdk/aws-lambda'
import * as iam from '@aws-cdk/aws-iam'
import * as pure from 'aws-cdk-pure'
import * as api from '@aws-cdk/aws-apigateway'

// 
//   
export const Auth = (host: string, layers: lambda.ILayerVersion[]): pure.IPure<api.LambdaIntegration> =>
  pure.wrap(api.LambdaIntegration)(
    Role().flatMap(x => Lambda(host, x, layers))
  )

//
const Lambda = (host: string, role: iam.IRole, layers: lambda.ILayerVersion[]): pure.IPure<lambda.Function> => {
  const iaac = pure.iaac(lambda.Function)
  const Auth = (): lambda.FunctionProps => ({
    runtime: lambda.Runtime.PROVIDED,
    code: new lambda.AssetCode('../apps/authorize/_build/default/bin'),
    handler: 'index.main',
    timeout: cdk.Duration.seconds(10),
    memorySize: 256,
    role,
    logRetention: logs.RetentionDays.FIVE_DAYS,
    reservedConcurrentExecutions: 5,
    layers,
    environment: {
      'PERMIT_ISSUER': `https://${host}`,
      'PERMIT_AUDIENCE': 'any',
      'PERMIT_CLAIMS': 'uid=true',
      'PERMIT_KEYPAIR': 'permit_config_ddb',
      'PERMIT_STORAGE': `ddb+https://dynamodb.${cdk.Aws.REGION}.amazonaws.com:443/oauth2-db-dev-pubkey`
    }
  })
  return iaac(Auth)
}

//
const Role = (): pure.IPure<iam.IRole> => {
  const role = pure.iaac(iam.Role)
  const AuthRole = (): iam.RoleProps => ({
    assumedBy: new iam.ServicePrincipal('lambda.amazonaws.com')
  })

  const ReadWrite = (): iam.PolicyStatement => (
    new iam.PolicyStatement({
      actions: ['*'],
      resources: ['*'],
    })
  )

  return role(AuthRole).effect(x => x.addToPolicy(ReadWrite()))
}