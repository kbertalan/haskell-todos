variable "tags" {
}

variable "zone_names" {
  type = list(string)
}

variable "vpc_id" {
  type = string
}

variable "subnet_ids" {
}

locals {
  port = 5432
}

resource "random_password" "password" {
  length           = 16
  special          = true
  override_special = "_@%"
}

resource "aws_db_subnet_group" "default" {
  name       = "todo-db"
  subnet_ids = var.subnet_ids

  tags = merge(var.tags, {
    Name = "Todo database subnet group"
  })
}

resource "aws_rds_cluster" "postgresql" {
  cluster_identifier      = "todos"
  engine                  = "aurora-postgresql"
  engine_mode             = "serverless"
  port                    = local.port
  availability_zones      = var.zone_names
  db_subnet_group_name    = aws_db_subnet_group.default.name
  database_name           = "todos"
  master_username         = "todos"
  master_password         = random_password.password.result
  backup_retention_period = 1
  preferred_backup_window = "07:00-09:00"
  vpc_security_group_ids  = [aws_security_group.database.id]

  scaling_configuration {
    auto_pause               = true
    max_capacity             = 2
    min_capacity             = 2
    seconds_until_auto_pause = 300
    timeout_action           = "ForceApplyCapacityChange"
  }

  tags = merge(var.tags, { Name = "todos" })
}

resource "aws_security_group" "database" {
  name   = "todos database security group"
  vpc_id = var.vpc_id
  tags   = var.tags
}

output "host" {
  value = aws_rds_cluster.postgresql.endpoint
}

output "port" {
  value = aws_rds_cluster.postgresql.port
}

output "name" {
  value = aws_rds_cluster.postgresql.database_name
}

output "user" {
  value = aws_rds_cluster.postgresql.master_username
}

output "password" {
  value = random_password.password.result
}

output "security_group_id" {
  value = aws_security_group.database.id
}
