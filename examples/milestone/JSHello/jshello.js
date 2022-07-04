let wasm = null;

function cstrlen(mem, ptr) {
    let len = 0;
    while (mem[ptr] != 0) {
        len++;
        ptr++;
    }
    return len;
}

function ptr_to_cstr(wasm_memory, ptr) {
    const mem = new Uint8Array(wasm_memory);
    const len = cstrlen(mem, ptr);
    const bytes = new Uint8Array(wasm_memory, ptr, len);
    return new TextDecoder().decode(bytes);
}

function displayString_5(str_ptr) {
    const wasm_memory = wasm.instance.exports.memory.buffer;
    const text = ptr_to_cstr(wasm_memory, str_ptr);
    console.log(text);
}

WebAssembly.instantiateStreaming(fetch('out.wasm'), {
    env: {
        displayString_5,
    }
}).then((w) => {
    wasm = w;
    wasm.instance.exports.render_6(0);
});
