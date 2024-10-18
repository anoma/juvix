## Anoma Resource Machine Standard Library

The file `stdlib.nockma` is obtained from the [Anoma Node repository](https://github.com/anoma/anoma).

Follow the compilation instructions for Anoma and run the Elixir interactive
shell in the root of the Anoma clone:

``` sh
iex -S mix
iex(1)> File.write("./stdlib.nockma", Nock.rm_core |> Noun.Format.print)
```
