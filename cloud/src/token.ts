import * as cdk from '@aws-cdk/core'
import * as ddb from '@aws-cdk/aws-dynamodb'
import * as logs from '@aws-cdk/aws-logs'
import * as lambda from '@aws-cdk/aws-lambda'
import * as iam from '@aws-cdk/aws-iam'
import * as pure from 'aws-cdk-pure'
import * as api from '@aws-cdk/aws-apigateway'
import * as permit from './permit' 

// 
//   
export const Token = (host: string, db: ddb.Table, layers: lambda.ILayerVersion[]): pure.IPure<api.LambdaIntegration> =>
  pure.wrap(api.LambdaIntegration)(
    Role(db).flatMap(x => Lambda(host, db, x, layers))
  )

//
const Lambda = (host: string, db: ddb.Table, role: iam.IRole, layers: lambda.ILayerVersion[]): pure.IPure<lambda.Function> => {
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
      'PERMIT_ISSUER': `https://${host}`,
      'PERMIT_AUDIENCE': 'any',
      'PERMIT_CLAIMS': '',
      'PERMIT_KEYPAIR': 'permit_config_ddb',
      'PERMIT_STORAGE': `ddb+https://dynamodb.${cdk.Aws.REGION}.amazonaws.com:443/${db.tableName}`
    }
  })
  return iaac(Token)
}

//
const Role = (db: ddb.Table): pure.IPure<iam.IRole> => {
  const role = pure.iaac(iam.Role)
  const TokenRole = (): iam.RoleProps => ({
    assumedBy: new iam.ServicePrincipal('lambda.amazonaws.com')
  })

  return role(TokenRole).effect(x => {
    x.addToPolicy(permit.LambdaLogging())
    x.addToPolicy(permit.DynamoDbReadOnly(db.tableArn))
  })
}