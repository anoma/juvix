(
  Logger.configure(level: :none)
  eclient = Anoma.Client.Examples.EClient.create_example_client
  IO.puts("#{eclient.client.grpc_port} #{eclient.node.node_id}")
  Anoma.Node.Utility.Consensus.start_link(node_id: eclient.node.node_id, interval: 500)
)
