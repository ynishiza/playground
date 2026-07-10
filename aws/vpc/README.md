# Basic AWS Terraform Setup

This Terraform project creates a small AWS VPC in `ap-northeast-1` with two public subnets, two private subnets, an Internet Gateway, and route tables. It does not create NAT Gateways or compute resources.

## Usage

```sh
terraform init
terraform fmt
terraform validate
terraform plan
```

To customize values:

```sh
cp terraform.tfvars.example terraform.tfvars
terraform plan
```

`terraform.tfvars` is ignored so local values do not get committed.
