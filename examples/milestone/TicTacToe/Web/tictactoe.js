let app = document.getElementById("app");
let ctx = app.getContext("2d");
let wasmModule = null;

function cstrlen(mem, ptr) {
    let len = 0;
    while (mem[ptr] != 0) {
        len++;
        ptr++;
    }
    return len;
}

const ALIGN_NAMES = ["left", "right", "center"];
const CANVAS_PADDING = 8;

function ptrToCstr(ptr) {
    const wasmMemory = wasmModule.instance.exports.memory.buffer;
    const mem = new Uint8Array(wasmMemory);
    const len = cstrlen(mem, ptr);
    const bytes = new Uint8Array(wasmMemory, ptr, len);
    return new TextDecoder().decode(bytes);
}

function hostFillRect(x, y, w, h, colorHexPtr) {
    const colorHex = ptrToCstr(colorHexPtr);
    ctx.fillStyle = colorHex;
    ctx.fillRect(x, y, w, h);
}

function hostFillText(x, y, textPtr, size, colorHexPtr, align) {
    const text = ptrToCstr(textPtr);
    const colorHex = ptrToCstr(colorHexPtr);
    ctx.fillStyle = colorHex;
    ctx.font = size+"px sans";
    ctx.textAlign = ALIGN_NAMES[align];
    ctx.fillText(text, x, y);
}

function hostLog(messagePtr) {
    const message = ptrToCstr(messagePtr);
    console.log(message);
}

WebAssembly.instantiateStreaming(fetch('TicTacToe.wasm'), {
    env: {
        hostFillRect,
        hostFillText,
        hostLog
    }
}).then((w) => {
    wasmModule = w;
    let state = wasmModule.instance.exports.initGame();
    document.addEventListener('mouseup', (e) => {
        ctx.clearRect(0, 0, app.width, app.height);
        state = wasmModule.instance.exports.move(state, e.x - CANVAS_PADDING, e.y - CANVAS_PADDING);
    });
});
