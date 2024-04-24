
resource "aws_api_gateway_rest_api" "dits_and_dahs_telegram_bot" {
  name = "dits-and-dahs-telegram-bot-${var.environment}"
}

resource "aws_api_gateway_resource" "updates" {
  rest_api_id = aws_api_gateway_rest_api.dits_and_dahs_telegram_bot.id
  parent_id   = aws_api_gateway_rest_api.dits_and_dahs_telegram_bot.root_resource_id

  path_part = "updates"
}

resource "aws_api_gateway_method" "post_updates" {
  rest_api_id = aws_api_gateway_rest_api.dits_and_dahs_telegram_bot.id
  resource_id = aws_api_gateway_resource.updates.id

  http_method   = "POST"
  authorization = "NONE"

  request_parameters = {
    "method.request.header.X-Telegram-Bot-Api-Secret-Token" = true
  }
}

resource "aws_api_gateway_integration" "dits_and_dahs_telegram_bot_lambda" {
  rest_api_id = aws_api_gateway_rest_api.dits_and_dahs_telegram_bot.id
  resource_id = aws_api_gateway_resource.updates.id

  type                    = "AWS_PROXY"
  http_method             = aws_api_gateway_method.post_updates.http_method
  integration_http_method = "POST"

  uri = aws_lambda_function.dits_and_dahs_telegram_bot.invoke_arn
}

resource "aws_api_gateway_deployment" "stage" {
  depends_on = [aws_api_gateway_integration.dits_and_dahs_telegram_bot_lambda]

  rest_api_id = aws_api_gateway_rest_api.dits_and_dahs_telegram_bot.id
  stage_name  = var.environment

  lifecycle {
    create_before_destroy = true
  }
}

resource "aws_lambda_permission" "invoce_dits_and_dahs_telegram_bot_lambda" {
  statement_id  = "AllowInvokeDitsAndDahsTelegramBotLambdaFromAPIGateway"
  action        = "lambda:InvokeFunction"
  function_name = aws_lambda_function.dits_and_dahs_telegram_bot.function_name
  principal     = "apigateway.amazonaws.com"

  # The /* part allows invocation from any stage, method and resource path
  # within API Gateway.
  source_arn = "${aws_api_gateway_rest_api.dits_and_dahs_telegram_bot.execution_arn}/*"
}

data "archive_file" "lambda_function_payload" {
  type        = "zip"
  source_file = "files/bootstrap"
  output_path = "files/lambda_function_payload.zip"
}

resource "aws_lambda_function" "dits_and_dahs_telegram_bot" {
  function_name = "dits-and-dahs-telegram-bot-${var.environment}"
  role          = aws_iam_role.execute_lambda.arn

  runtime          = "provided.al2023"
  handler          = "provided"
  filename         = data.archive_file.lambda_function_payload.output_path
  source_code_hash = data.archive_file.lambda_function_payload.output_base64sha256

  environment {
    variables = {
      SECRET_TOKEN = var.telegram_bot_secret_token
    }
  }
}

resource "aws_iam_role" "execute_lambda" {
  name               = "DitsAndDahsTelegramBotLambdaExecutionRole-${var.environment}"
  assume_role_policy = data.aws_iam_policy_document.assume_role.json
}

data "aws_iam_policy_document" "assume_role" {
  statement {
    effect = "Allow"

    principals {
      type        = "Service"
      identifiers = ["lambda.amazonaws.com"]
    }

    actions = ["sts:AssumeRole"]
  }
}

resource "aws_iam_role_policy_attachment" "lambda_basic_execution" {
  role       = aws_iam_role.execute_lambda.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
}
