variable "environment" {
  description = "The environment in which the infrastructure is deployed"
  type        = string
}

variable "telegram_bot_secret_token" {
  description = "Secret token used for validating requests from the Telegram Bot API"
  type        = string
  sensitive   = true
}
