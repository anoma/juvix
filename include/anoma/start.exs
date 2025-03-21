(
  Logger.configure(level: :none)
  eclient = Anoma.Client.Examples.EClient.create_example_client
  IO.puts("#{eclient.client.grpc_port} #{eclient.node.node_id}")
  # Logger.configure(level: :debug) # uncomment this for debug messages
  Anoma.Node.Utility.Consensus.start_link(node_id: eclient.node.node_id, interval: 500)
)
