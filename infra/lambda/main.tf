variable "tags" {
}

variable "vpc_id" {
  type = string
}

variable "subnet_ids" {
  type = list(string)
}

variable "database_security_group_id" {
  type = string
}

variable "api_gateway_execution_arn" {
  type = string
}

variable "database" {
  type = object({
    host     = string
    port     = string
    name     = string
    user     = string
    password = string
  })
}

locals {
  filename = "todo-function.zip"
}

resource "local_file" "bootstrap" {
  content  = <<EOF
#/bin/sh

echo Obviusly this command will fail as a lambda
EOF
  filename = "${path.module}/bootstrap"
}

data "archive_file" "todo-function" {
  type        = "zip"
  source_file = "${path.module}/bootstrap"
  output_path = "${path.module}/${local.filename}"
  depends_on  = [local_file.bootstrap]
}

resource "aws_s3_bucket" "deploy" {
  bucket = "todo-lambda-deploy"
  acl    = "private"

  tags = merge(var.tags, {
    Name = "Deployments of the todo lambda function"
  })
}

resource "aws_s3_bucket_object" "code" {
  bucket     = aws_s3_bucket.deploy.bucket
  key        = local.filename
  source     = "${path.module}/${local.filename}"
  etag       = md5(local_file.bootstrap.content)
  depends_on = [data.archive_file.todo-function]

  lifecycle {
    ignore_changes = [
      etag
    ]
  }
}

resource "aws_lambda_function" "lambda" {
  function_name = "todo-lambda"
  role          = aws_iam_role.lambda.arn
  handler       = "todos"

  memory_size = 256
  runtime     = "provided.al2"
  s3_bucket   = aws_s3_bucket.deploy.bucket
  s3_key      = local.filename

  vpc_config {
    subnet_ids         = var.subnet_ids
    security_group_ids = [aws_security_group.lambda.id]
  }

  environment {
    variables = {
      "DB_HOST"      = var.database.host
      "DB_PORT"      = var.database.port
      "DB_NAME"      = var.database.name
      "DB_USER"      = var.database.user
      "DB_PASSWORD"  = var.database.password
      "POOL_SIZE"    = "2"
      "POOL_TIMEOUT" = "1"
    }
  }

  depends_on = [aws_s3_bucket_object.code]

  tags = var.tags
}

resource "aws_iam_role" "lambda" {
  name = "todo-lambda-role"

  assume_role_policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": "sts:AssumeRole",
      "Principal": {
        "Service": "lambda.amazonaws.com"
      },
      "Effect": "Allow",
      "Sid": ""
    }
  ]
}
EOF

  tags = var.tags
}

resource "aws_cloudwatch_log_group" "log" {
  name              = "/aws/lambda/${aws_lambda_function.lambda.function_name}"
  retention_in_days = 3

  tags = var.tags
}

resource "aws_iam_policy" "logging" {
  name        = "lambda_logging"
  path        = "/"
  description = "IAM policy for logging from a lambda"

  policy = <<EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Action": [
        "logs:CreateLogGroup",
        "logs:CreateLogStream",
        "logs:PutLogEvents"
      ],
      "Resource": "arn:aws:logs:*:*:*",
      "Effect": "Allow"
    }
  ]
}
EOF

  tags = var.tags
}

resource "aws_iam_role_policy_attachment" "lambda_logs" {
  role       = aws_iam_role.lambda.name
  policy_arn = aws_iam_policy.logging.arn
}

data "aws_iam_policy" "vpc" {
  name = "AWSLambdaVPCAccessExecutionRole"
}

resource "aws_iam_role_policy_attachment" "lambda_vpc" {
  role       = aws_iam_role.lambda.name
  policy_arn = data.aws_iam_policy.vpc.arn
}

resource "aws_security_group" "lambda" {
  name   = "todo-lambda-security-group"
  vpc_id = var.vpc_id
  tags   = var.tags

  egress {
    from_port       = var.database.port
    to_port         = var.database.port
    protocol        = "tcp"
    security_groups = [var.database_security_group_id]
  }
}

resource "aws_security_group_rule" "database" {
  security_group_id        = var.database_security_group_id
  type                     = "ingress"
  from_port                = var.database.port
  to_port                  = var.database.port
  protocol                 = "tcp"
  source_security_group_id = aws_security_group.lambda.id
}

output "invoke_arn" {
  value = aws_lambda_function.lambda.invoke_arn
}

output "function_name" {
  value = aws_lambda_function.lambda.function_name
}
