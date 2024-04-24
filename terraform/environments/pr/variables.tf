variable "environment_suffix" {
  description = "Suffix to be added to the environment name"
  type        = string
}

variable "telegram_bot_secret_token" {
  description = "Secret token used for validating requests from the Telegram Bot API"
  type        = string
}
