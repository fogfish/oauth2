import * as ddb from '@aws-cdk/aws-dynamodb'
import * as cdk from '@aws-cdk/core'
import * as pure from 'aws-cdk-pure'

export const DDB = (vsn: string): pure.IPure<ddb.Table> => {
  const iaac = pure.iaac(ddb.Table)
  const AuthPubKey = (): ddb.TableProps => ({
    partitionKey: {type: ddb.AttributeType.STRING, name: 'prefix'},
    readCapacity: 1,
    removalPolicy: vsn.startsWith('pr') ? cdk.RemovalPolicy.DESTROY : cdk.RemovalPolicy.RETAIN,
    sortKey: {type: ddb.AttributeType.STRING, name: 'suffix'},
    tableName: `${cdk.Aws.STACK_NAME}-pubkey`,
    writeCapacity: 1,
  })
  return iaac(AuthPubKey)
}
