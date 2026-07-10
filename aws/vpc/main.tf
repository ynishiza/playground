data "aws_availability_zones" "available" {
  state = "available"
}

locals {
  common_tags = {
    Project   = var.project_name
    ManagedBy = "terraform"
  }

  subnet_count = max(length(var.public_subnet_cidrs), length(var.private_subnet_cidrs))
  azs          = slice(data.aws_availability_zones.available.names, 0, local.subnet_count)
}

#  ========== Main VPC ==========

resource "aws_vpc" "main" {
  cidr_block           = var.vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = {
    Name = "${var.project_name}-vpc"
  }
}

resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id

  tags = {
    Name = "${var.project_name}-igw"
  }
}

# Public Subnets
#
# Has access to everything. i.e.
# - Internet Gateway: inbound & outbound
# - Peering connection to sub VPC: inbound & outbound
resource "aws_subnet" "main_public" {
  count = length(var.public_subnet_cidrs)

  vpc_id                  = aws_vpc.main.id
  cidr_block              = var.public_subnet_cidrs[count.index]
  availability_zone       = local.azs[count.index]
  map_public_ip_on_launch = true

  tags = {
    Name = "${var.project_name}-public-${count.index + 1}"
    Tier = "public"
  }
}

# Private subnets 
#
# Not accessible from the outside.
# However, has access to everything from the inside:
# - Internet, via public NAT
# - Peering connection to sub VPC, via a private NAT
resource "aws_subnet" "main_private" {
  count = length(var.private_subnet_cidrs)

  vpc_id            = aws_vpc.main.id
  cidr_block        = var.private_subnet_cidrs[count.index]
  availability_zone = local.azs[count.index]

  tags = {
    Name = "${var.project_name}-private-${count.index + 1}"
    Tier = "private"
  }
}

resource "aws_default_route_table" "main" {
  default_route_table_id = aws_vpc.main.default_route_table_id

  tags = {
    Name = "${var.project_name}-default"
  }
}

resource "aws_route_table" "main_public" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id             = aws_internet_gateway.main.id
  }

  # IMPORTANT: MUST have route in both VPCs for peering connections
  # Otherwise, the connection will not work.
  route {
    cidr_block                 = "160.0.0.0/16"
    vpc_peering_connection_id  = aws_vpc_peering_connection.main_to_private.id
  }

  tags = {
    Name = "${var.project_name}-public-rt"
  }
}

resource "aws_route_table_association" "main_public" {
  count = length(aws_subnet.main_public)

  subnet_id      = aws_subnet.main_public[count.index].id
  route_table_id = aws_route_table.main_public.id
}

# public NAT: access internet from private subnet 
resource "aws_nat_gateway" "private_to_public" {
  vpc_id            = aws_vpc.main.id
  availability_mode = "regional"

  tags = {
    Name = "${var.project_name}-internet"
  }
}

# private NAT: access peering connection from private subnet
resource "aws_nat_gateway" "private_to_sub" {
  connectivity_type = "private"
  subnet_id         = aws_subnet.main_public[0].id

  tags = {
    Name = "${var.project_name}-subvpc"
  }
}

resource "aws_route_table" "main_private" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    nat_gateway_id = aws_nat_gateway.private_to_public.id
  }

  route {
    cidr_block = aws_vpc.private.cidr_block
    nat_gateway_id = aws_nat_gateway.private_to_sub.id
  }

  tags = {
    Name = "${var.project_name}-private-rt"
  }
}

resource "aws_route_table_association" "main_private" {
  count = length(aws_subnet.main_private)

  subnet_id      = aws_subnet.main_private[count.index].id
  route_table_id = aws_route_table.main_private.id
}

resource "aws_security_group" "main" {
  name = "main"
  vpc_id = aws_vpc.main.id

  tags = {
    Name = "${var.project_name}-public"
  }
}

resource "aws_vpc_security_group_ingress_rule" "allow_ssh" {
  security_group_id = aws_security_group.main.id
  cidr_ipv4         = "0.0.0.0/0"
  ip_protocol       = "tcp"
  from_port         = 22
  to_port           = 22
}

resource "aws_vpc_security_group_egress_rule" "main_default" {
  security_group_id = aws_security_group.main.id
  cidr_ipv4         = "0.0.0.0/0"
  ip_protocol       = -1
}


#  ========== Private VPC ==========
#
# A completely private VPC
#
# Only accessible through peering connections.

resource "aws_vpc" "private" {
  cidr_block           = var.private_vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = {
    Name = "${var.project_name}-privatevpc"
  }
}

resource "aws_subnet" "privatevpc" {
  count = length(var.private_vpc_subnet_cidrs)

  vpc_id            = aws_vpc.private.id
  cidr_block        = var.private_vpc_subnet_cidrs[count.index]
  availability_zone = local.azs[count.index]

  tags = {
    Name = "${var.project_name}-privatevpc-${count.index + 1}"
    Tier = "private"
  }
}

resource "aws_default_route_table" "private" {
  default_route_table_id = aws_vpc.private.default_route_table_id

  # IMPORTANT: MUST have route in both VPCs for peering connections
  # Otherwise, the connection will not work.
  route {
    cidr_block                 = "10.0.0.0/16"
    vpc_peering_connection_id  = aws_vpc_peering_connection.main_to_private.id
  }

  tags = {
    Name = "${var.project_name}-private-default"
  }
}

resource "aws_vpc_peering_connection" "main_to_private" {
  peer_vpc_id   = aws_vpc.main.id
  vpc_id        = aws_vpc.private.id

  tags = {
    Name = "${var.project_name}-main-private-vpc-peer"
  }
}

resource "aws_security_group" "private" {
  name = "main"
  vpc_id = aws_vpc.private.id

  tags = {
    Name = "${var.project_name}-private"
  }
}

resource "aws_vpc_security_group_ingress_rule" "allow_ssh_sub" {
  security_group_id = aws_security_group.private.id
  cidr_ipv4         = "0.0.0.0/0"
  ip_protocol       = "tcp"
  from_port         = 22
  to_port           = 22
}

resource "aws_vpc_security_group_egress_rule" "private_default" {
  security_group_id = aws_security_group.private.id
  cidr_ipv4         = "0.0.0.0/0"
  ip_protocol       = -1
}


#  ========== Sample EC2 instance ==========

data "aws_ami" "base" {
  most_recent = true

  filter {
    name = "image-id"
    values = ["ami-081c90bfbab2929da"]
  }
}

# the default TokyoBase.pem
data "aws_key_pair" "main" {
  key_pair_id = "key-00333c6a055d19286"
}

# EC2 instance in the main VPC: public subnet
#
# Accessible from internet
resource "aws_instance" "test_public" {
  ami = data.aws_ami.base.id
  instance_type = "t2.nano"
  subnet_id = aws_subnet.main_public[0].id
  key_name = data.aws_key_pair.main.key_name
  vpc_security_group_ids = [aws_security_group.main.id]

  tags = {
    Name = "${var.project_name}-public"
  }
}

# EC2 instance in the main VPC: private subnet
#
# Accessible within VPC
resource "aws_instance" "test_private" {
  ami = data.aws_ami.base.id
  instance_type = "t2.nano"
  subnet_id = aws_subnet.main_private[0].id
  key_name = data.aws_key_pair.main.key_name
  vpc_security_group_ids = [aws_security_group.main.id]

  tags = {
    Name = "${var.project_name}-private"
  }
}

# EC2 instance in the private VPC
# 
# Inaccessible except through Peering connection
resource "aws_instance" "test_sub" {
  ami = data.aws_ami.base.id
  instance_type = "t2.nano"
  subnet_id = aws_subnet.privatevpc[0].id
  key_name = data.aws_key_pair.main.key_name
  vpc_security_group_ids = [aws_security_group.private.id]

  tags = {
    Name = "${var.project_name}-sub"
  }
}
