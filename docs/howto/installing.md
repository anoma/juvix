# Dependencies

You need [Clang / LLVM](https://releases.llvm.org/download.html) version
13 or later. Note that on macOS the preinstalled clang does not support
the wasm target, so use e.g. `brew install llvm` instead.

If you want to compile to WebAssembly, you also need:

- [wasmer](https://wasmer.io)
- [wasi-sdk](https://github.com/WebAssembly/wasi-sdk/releases)
- [wasm-ld](https://lld.llvm.org) - the LLVM linker for WASM (NB: On
  Linux you may need to install the `lld` package; on macOS this is
  installed as part of `llvm`).

See [below](./installing.md#installing-dependencies) for instructions on
how to install the dependencies.

# Installing Juvix

### MacOS

The easiest way to install Juvix on MacOS is by using
[Homebrew](https://brew.sh).

To install the [homebrew-juvix
tap](https://github.com/anoma/homebrew-juvix), run:

```shell
brew tap anoma/juvix
```

To install Juvix, run:

```shell
brew install juvix
```

Helpful information can also be obtained by running:

```shell
brew info juvix
```

### Linux x86<sub>64</sub>

A Juvix compiler binary executable for Linux x86<sub>64</sub> is
available on the [Juvix release
page](https://github.com/anoma/juvix/releases/latest).

To install this executable, download and unzip the linked file and move
it to a directory on your shell's `PATH`.

For example if `~/.local/bin` is on your shell's `PATH`, you can install
Juvix as follows:

```shell
cd /tmp
curl -OL https://github.com/anoma/juvix/releases/download/v0.3.0/juvix-linux_x86_64-v0.3.0.zip
unzip juvix-linux_x86_64-v0.3.0.zip
mv juvix-linux_x86_64-v0.3.0 ~/.local/bin/juvix
```

### Building Juvix from source

To install Juvix from source you must clone the [Github
repository](https://github.com/anoma/juvix.git). Then Juvix can be
installed with the following commands. We assume you have
[Stack](https://haskellstack.org) and [GNU
Make](https://www.gnu.org/software/make/) installed.

```shell
git clone --recursive https://github.com/anoma/juvix.git
cd juvix
make install
```

The C compiler and linker paths can be specified as options to the
`make install` command, e.g.

```shell
make install CC=path/to/clang LIBTOOL=path/to/llvm-ar
```

On MacOS, you can alternatively run the following command for Homebrew.
The flag `--HEAD` used below is optional – use it to build the latest
version of Juvix in the `main` branch on Github.

```shell
brew install --build-from-source --HEAD juvix --verbose
```

### Building the project with `cabal`

We recommend to use the `stack` build tool with this project.

If you prefer the `cabal` build tool instead, then you need to generate
the `juvix.cabal` file using [hpack](https://github.com/sol/hpack)
before running `cabal build`.

You also need to compile the runtime first:

```shell
make runtime
cabal build
```

# Installing dependencies

To install `wasi-sdk` you need to download `libclang_rt` and
`wasi-sysroot` precompiled archives from the [wasi-sdk release
page](https://github.com/WebAssembly/wasi-sdk/releases/) and:

1.  Extract the `libclang_rt.builtins-wasm32-wasi-*.tar.gz` archive in
    the `clang` installation root (for example `/usr/lib/clang/13` on
    Ubuntu or `` `brew --prefix llvm` `` on macos).

    For example on macos with homebrew clang:

    ```shell
    cd `brew --prefix llvm`
    curl https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-15/libclang_rt.builtins-wasm32-wasi-15.0.tar.gz -OL
    tar xf libclang_rt.builtins-wasm32-wasi-15.0.tar.gz
    ```

2.  Extract the `wasi-sysroot-*.tar.gz` archive on your local system and
    set `WASI_SYSROOT_PATH` to its path.

    For example:

    ```shell
    cd ~
    curl https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-15/wasi-sysroot-15.0.tar.gz -OL
    tar xf wasi-sysroot-15.0.tar.gz
    export WASI_SYSROOT_PATH=~/wasi-sysroot
    ```
