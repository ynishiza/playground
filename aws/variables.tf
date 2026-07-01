variable "aws_region" {
  description = "AWS region where resources will be created."
  type        = string
  default     = "ap-northeast-1"
}

variable "project_name" {
  description = "Name prefix used for resource names and tags."
  type        = string
  default     = "basic-aws"

  validation {
    condition     = can(regex("^[a-zA-Z][a-zA-Z0-9-]{1,38}[a-zA-Z0-9]$", var.project_name))
    error_message = "project_name must start with a letter, end with a letter or number, and contain only letters, numbers, and hyphens."
  }
}

variable "vpc_cidr" {
  description = "CIDR block for the VPC."
  type        = string
  default     = "10.0.0.0/16"

  validation {
    condition     = can(cidrnetmask(var.vpc_cidr))
    error_message = "vpc_cidr must be a valid IPv4 CIDR block."
  }
}

variable "public_subnet_cidrs" {
  description = "CIDR blocks for public subnets. At least two are recommended."
  type        = list(string)
  default     = ["10.0.0.0/24", "10.0.1.0/24"]

  validation {
    condition     = length(var.public_subnet_cidrs) >= 2 && alltrue([for cidr in var.public_subnet_cidrs : can(cidrnetmask(cidr))])
    error_message = "public_subnet_cidrs must contain at least two valid IPv4 CIDR blocks."
  }
}

variable "private_subnet_cidrs" {
  description = "CIDR blocks for private subnets. At least two are recommended."
  type        = list(string)
  default     = ["10.0.10.0/24", "10.0.11.0/24"]

  validation {
    condition     = length(var.private_subnet_cidrs) >= 2 && alltrue([for cidr in var.private_subnet_cidrs : can(cidrnetmask(cidr))])
    error_message = "private_subnet_cidrs must contain at least two valid IPv4 CIDR blocks."
  }
}
