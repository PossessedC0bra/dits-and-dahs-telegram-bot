terraform {
  required_version = ">=1.5.0"

  backend "s3" {
    region = "eu-central-2"

    bucket               = "terraform.ykl.ch"
    workspace_key_prefix = "dits-and-dahs-telegram-bot"
    key                  = "terraform.tfstate"

    dynamodb_table = "TerraformStateLocks"
  }

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~>5.46.0"
    }
    archive = {
      source  = "hashicorp/archive"
      version = "~>2.4.0"
    }
  }
}

module "dits-and-dahs-telegram-bot-api" {
  source = "../../modules/dits-and-dahs-telegram-bot-api"

  environment               = substr(sha256("pr-${var.environment_suffix}"), 0, 16)
  telegram_bot_secret_token = var.telegram_bot_secret_token
}

output "webhook_url" {
  value = module.dits-and-dahs-telegram-bot-api.post_updates_resource_url
}
