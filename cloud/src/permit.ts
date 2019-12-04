import * as iam from '@aws-cdk/aws-iam'

export const LambdaLogging = (): iam.PolicyStatement =>
  new iam.PolicyStatement({
    resources: ['*'],
    actions: [
      'logs:CreateLogGroup',
      'logs:CreateLogStream',
      'logs:PutLogEvents'
    ],
  })

export const DynamoDbReadWrite = (arn: string): iam.PolicyStatement =>
  new iam.PolicyStatement({
    actions: [
      'dynamodb:PutItem',
      'dynamodb:UpdateItem',
      'dynamodb:GetItem',
      'dynamodb:Query',
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