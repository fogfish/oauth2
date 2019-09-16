import * as cdk from '@aws-cdk/core'
import * as iam from '@aws-cdk/aws-iam'
import { IaaC, iaac, use } from 'aws-cdk-pure'

const role = iaac(iam.Role)

export function Writer(): IaaC<iam.Role> {
  return use({ role: role(IamWriter) })
    .effect(x => {
      x.role.addToPolicy(AllowDynamoWrite())
    })
    .yield('role')
}

function IamWriter(): iam.RoleProps {
  return {
    assumedBy: new iam.ServicePrincipal('lambda.amazonaws.com')
  }
}

function AllowDynamoWrite(): iam.PolicyStatement {
  return new iam.PolicyStatement({
    resources: ['*'],
    actions: ['*']
  })
}