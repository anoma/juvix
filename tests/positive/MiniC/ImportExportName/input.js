const fs = require('fs');

let wasmModule = null;

function cstrlen(mem, ptr) {
    let len = 0;
    while (mem[ptr] != 0) {
        len++;
        ptr++;
    }
    return len;
}

function ptrToCstr(ptr) {
    const wasmMemory = wasmModule.instance.exports.memory.buffer;
    const mem = new Uint8Array(wasmMemory);
    const len = cstrlen(mem, ptr);
    const bytes = new Uint8Array(wasmMemory, ptr, len);
    return new TextDecoder().decode(bytes);
}

function hostDisplayString(strPtr) {
    const text = ptrToCstr(strPtr);
    console.log(text);
}

const wasmBuffer = fs.readFileSync("Input.wasm");
WebAssembly.instantiate(wasmBuffer, {
    env: {
        hostDisplayString,
    }
}).then((w) => {
    wasmModule = w;
    wasmModule.instance.exports.juvixRender();
});
