import * as cdk from '@aws-cdk/core'
import * as ddb from '@aws-cdk/aws-dynamodb'
import { iaac, root, join, flat } from 'aws-cdk-pure'
import { RestApi } from './restapi'

const table = iaac(ddb.Table)

function Storage(): ddb.TableProps {
  return {
    tableName: `${cdk.Aws.STACK_NAME}-pubkey`,
    partitionKey: {type: ddb.AttributeType.STRING, name: 'prefix'},
    sortKey: {type: ddb.AttributeType.STRING, name: 'suffix'},
    readCapacity: 1,
    writeCapacity: 1
  }
}

function OAuth2(stack: cdk.Construct): cdk.Construct {
  join(stack, flat(RestApi))
  join(stack, table(Storage))
  return stack
}

const vsn = process.env.VSN || 'local'
const app = new cdk.App()
root(app, OAuth2, `oauth2-${vsn}`)
app.synth()