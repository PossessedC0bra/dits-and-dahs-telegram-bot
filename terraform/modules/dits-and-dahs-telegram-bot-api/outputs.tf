output "post_updates_resource_url" {
  description = "URL of the resource to send updates"
  value       = "${aws_api_gateway_deployment.stage.invoke_url}/${aws_api_gateway_resource.updates.path_part}"
}
