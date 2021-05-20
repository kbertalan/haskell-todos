variable "tags" {
}

variable "zone_names" {
  type = list(string)
}

variable "subnet_ids" {
}

resource "random_password" "password" {
  length = 16
  special = true
  override_special = "_@%"
}

resource "aws_db_subnet_group" "default" {
  name       = "todo-db"
  subnet_ids = var.subnet_ids

  tags = merge(var. tags, {
    Name = "Todo database subnet group"
  })
}

resource "aws_rds_cluster" "postgresql" {
  cluster_identifier      = "todos"
  engine                  = "aurora-postgresql"
  engine_mode             = "serverless"
  availability_zones      = var.zone_names
  db_subnet_group_name    = aws_db_subnet_group.default.name
  database_name           = "todos"
  master_username         = "todos"
  master_password         = random_password.password.result
  backup_retention_period = 1
  preferred_backup_window = "07:00-09:00"

  scaling_configuration {
    auto_pause               = true
    max_capacity             = 8
    min_capacity             = 2
    seconds_until_auto_pause = 300
    timeout_action           = "ForceApplyCapacityChange"
  }

  tags = merge(var.tags, { Name = "todos" })
}
