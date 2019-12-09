import * as api from '@aws-cdk/aws-apigateway'
import * as ddb from '@aws-cdk/aws-dynamodb'
import * as iam from '@aws-cdk/aws-iam'
import * as lambda from '@aws-cdk/aws-lambda'
import * as logs from '@aws-cdk/aws-logs'
import * as cdk from '@aws-cdk/core'
import * as pure from 'aws-cdk-pure'
import * as permit from './permit' 

// 
//   
export const Function = (host: string, email: string, db: ddb.Table, layers: lambda.ILayerVersion[]): pure.IPure<api.LambdaIntegration> =>
  pure.wrap(api.LambdaIntegration)(
    Role(db).flatMap(x => Lambda(host, email, db, x, layers))
  )

//
const Lambda = (host: string, email: string, db: ddb.Table, role: iam.IRole, layers: lambda.ILayerVersion[]): pure.IPure<lambda.Function> => {
  const iaac = pure.iaac(lambda.Function)
  const Auth = (): lambda.FunctionProps => ({
    code: new lambda.AssetCode('../apps/authorize/_build/default/bin'),
    environment: {
      'OAUTH2_EMAIL': email,
      'OAUTH2_EMAIL_SIGNATURE': 'Service',
      'PERMIT_AUDIENCE': 'any',
      'PERMIT_CLAIMS': '',
      'PERMIT_ISSUER': `https://${host}`,
      'PERMIT_KEYPAIR': 'permit_config_ddb',
      'PERMIT_STORAGE': `ddb+https://dynamodb.${cdk.Aws.REGION}.amazonaws.com:443/${db.tableName}`,
    },
    handler: 'index.main',
    layers,
    logRetention: logs.RetentionDays.FIVE_DAYS,
    memorySize: 256,
    reservedConcurrentExecutions: 5,
    role,
    runtime: lambda.Runtime.PROVIDED,
    timeout: cdk.Duration.seconds(10),
  })
  return iaac(Auth)
}

//
const Role = (db: ddb.Table): pure.IPure<iam.IRole> => {
  const role = pure.iaac(iam.Role)
  const AuthRole = (): iam.RoleProps => ({
    assumedBy: new iam.ServicePrincipal('lambda.amazonaws.com')
  })

  return role(AuthRole).effect(x => {
    x.addToPolicy(permit.LambdaLogging())
    x.addToPolicy(permit.DynamoDbReadWrite(db.tableArn))
  })
}