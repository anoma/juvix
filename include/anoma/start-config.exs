eclient = Anoma.Client.Examples.EClient.create_example_client
IO.puts("#{eclient.client.grpc_port} #{eclient.node.node_id}")
Logger.configure(level: :debug)
Anoma.Node.Utility.Consensus.start_link(node_id: eclient.node.node_id, interval: 5000)
File.write("config.yaml", "url: localhost\nport: #{eclient.client.grpc_port}\nnodeid: \"\"")
