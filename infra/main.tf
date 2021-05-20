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
  subnet_ids = module.vpc.subnets[*].id
}
