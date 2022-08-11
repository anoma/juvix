const fs = require('fs');

const wasmBuffer = fs.readFileSync("Input.wasm");

WebAssembly.instantiate(wasmBuffer, {}).then((wasmModule) => {
    console.log(wasmModule.instance.exports._validate_tx(0n,0n,0n,0n,0n,0n,0n,0n));
});
