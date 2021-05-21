variable "tags" {
}

variable "zone_names" {
  type = list(string)
}

resource "aws_vpc" "main" {
  cidr_block = "10.0.0.0/16"
  tags       = merge(var.tags, { Name = "todos" })
}

resource "aws_subnet" "subnet" {
  count             = length(var.zone_names)
  vpc_id            = aws_vpc.main.id
  availability_zone = var.zone_names[count.index]
  cidr_block        = cidrsubnet("10.0.0.0/16", length(var.zone_names), count.index)
  tags              = merge(var.tags, { Name = "Subnet for AZ ${var.zone_names[count.index]}" })
}

output "vpc_id" {
  value = aws_vpc.main.id
}

output "subnets" {
  value = aws_subnet.subnet
}
