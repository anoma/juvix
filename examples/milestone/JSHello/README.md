## Example of calling between JS and WASM via MiniJuvix

### Build

`minijuvix compile --target c JSHello.mjuvix`

`clang -Oz -fno-builtin --no-standard-libraries --target=wasm32 -Wl,--export=render_6 -Wl,--no-entry -Wl,--allow-undefined -o out.wasm .minijuvix-build/JSHello.c`

Check the `render` function was exported, and the `displayString` function is imported. You may have to change the name id suffixes of these functions in the `clang` command and in `jshello.js` to match those shown by `wasm-objdump` (this command is available from https://github.com/WebAssembly/wabt):

```shell
wasm-objdump -x out.wasm

out.wasm:	file format wasm 0x1

Section Details:

Type[1]:
 - type[0] (i32) -> nil
Import[1]:
 - func[0] sig=0 <env.displayString_5> <- env.displayString_5
Function[1]:
 - func[1] sig=0 <render_6>
Memory[1]:
 - memory[0] pages: initial=2
Export[2]:
 - memory[0] -> "memory"
 - func[1] <render_6> -> "render_6"
Code[1]:
 - func[1] size=7 <render_6>
Data[1]:
 - segment[0] memory=0 size=5 - init i32=1024
  - 0000400: 6865 6c6c 6f                             hello
Custom:
 - name: "producers"
```

### Run:

Serve the files:

`python3 -m http.server`

And browse to http://localhost:8000
