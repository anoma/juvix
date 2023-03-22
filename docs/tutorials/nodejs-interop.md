# NodeJS Interop

A Juvix module can be compiled to a Wasm module. When a Wasm module is
instantiated by a host, functions from the host can be injected into a
Wasm module and functions from the Wasm module can be called by the
host.

In this tutorial you will see how to call host functions in Juvix and
call Juvix functions from the host using the Wasm mechanism.

## The Juvix module

The following Juvix module has two functions.

The function `hostDisplayString` is an `axiom` with no corresponding
`compile` block that implements it. We will inject an implementation for
this function when we instantiate the module from NodeJS.

The function `juvixRender` is a normal Juvix function. We will call this
from NodeJS.

    -- NodeJsInterop.juvix
    module NodeJsInterop;

    open import Stdlib.Prelude;

    axiom hostDisplayString : String → IO;

    juvixRender : IO;
    juvixRender := hostDisplayString "Hello World from Juvix!";

    end;

## Compiling the Juvix module

The Juvix module can be compiled using the following command:

    juvix compile -t wasm -r standalone NodeJsInterop.juvix

This will create a file containing a Wasm module called
`NodeJsInterop.wasm`.

## The NodeJS module

The following NodeJS module demonstrates both calling a Juvix function
from NodeJS and injecting a NodeJS function into a Juvix module.

The NodeJS function `hostDisplayString` is passed to the Wasm module
`NodeJSInterop.wasm` when it is instantiated. After instantiation the
Juvix function `juvixRender` is called.

The functions `ptrToCstr` and `cstrlen` are necessary to convert the
`char` pointer passed from Juvix to a JS `String`.

    // NodeJSInterop.js
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

    const wasmBuffer = fs.readFileSync("NodeJsInterop.wasm");
    WebAssembly.instantiate(wasmBuffer, {
        env: {
            hostDisplayString,
        }
    }).then((w) => {
        wasmModule = w;
        wasmModule.instance.exports.juvixRender();
    });

## Running the Wasm module

Now you should have the files `NodeJsInterop.wasm` and
`NodeJsInterop.js` in the same directory. Run the following command to
execute the module:

    node NodeJsInterop.js

You should see the following output:

    Hello World from Juvix!
