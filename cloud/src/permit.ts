import * as iam from '@aws-cdk/aws-iam'

export const LambdaLogging = (): iam.PolicyStatement =>
  new iam.PolicyStatement({
    actions: [
      'logs:CreateLogGroup',
      'logs:CreateLogStream',
      'logs:PutLogEvents'
    ],
    resources: ['*'],
  })

export const DynamoDbReadWrite = (arn: string): iam.PolicyStatement =>
  new iam.PolicyStatement({
    actions: [
      'dynamodb:PutItem',
      'dynamodb:UpdateItem',
      'dynamodb:GetItem',
      'dynamodb:Query',
      'dynamodb:DeleteItem',
    ],
    resources: [arn],
  })

export const DynamoDbReadOnly = (arn: string): iam.PolicyStatement =>
new iam.PolicyStatement({
  actions: [
    'dynamodb:GetItem',
    'dynamodb:Query',
  ],
  resources: [arn],
})