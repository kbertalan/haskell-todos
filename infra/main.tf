data "aws_availability_zones" "available" {
  state = "available"
}

locals {
  tags = {
    "project" = "todos"
  }
  azs = data.aws_availability_zones.available.names
}


module "vpc" {
  source     = "./vpc"
  tags       = local.tags
  zone_names = local.azs
}

module "db" {
  source     = "./db"
  tags       = local.tags
  zone_names = local.azs
  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.subnets[*].id
}

module "lambda" {
  source                     = "./lambda"
  tags                       = local.tags
  vpc_id                     = module.vpc.vpc_id
  subnet_ids                 = module.vpc.subnets[*].id
  database_security_group_id = module.db.security_group_id
  api_gateway_execution_arn  = ""
  database = {
    host     = module.db.host
    port     = module.db.port
    name     = module.db.name
    user     = module.db.user
    password = module.db.password
  }
}

module "gateway" {
  source = "./gw"
  tags   = local.tags

  lambda_invoke_arn    = module.lambda.invoke_arn
  lambda_function_name = module.lambda.function_name
}
