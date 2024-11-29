(
  Logger.configure(level: :none)
  eclient = Anoma.Client.Examples.EClient.create_example_client
  IO.puts("#{eclient.client.grpc_port} #{eclient.node.node_id}")
)
