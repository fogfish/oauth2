import * as cdk from '@aws-cdk/core'
import * as ddb from '@aws-cdk/aws-dynamodb'
import * as pure from 'aws-cdk-pure'

export const DDB = (): pure.IPure<ddb.Table> => {
  const iaac = pure.iaac(ddb.Table)
  const AuthPubKey = (): ddb.TableProps => ({
    tableName: `${cdk.Aws.STACK_NAME}-pubkey`,
    partitionKey: {type: ddb.AttributeType.STRING, name: 'prefix'},
    sortKey: {type: ddb.AttributeType.STRING, name: 'suffix'},
    readCapacity: 1,
    writeCapacity: 1
  })
  return iaac(AuthPubKey)
}
