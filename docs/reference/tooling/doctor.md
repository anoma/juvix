# Juvix Doctor

The `juvix doctor` command can help you to troubleshoot problems with
your development environment. For each problem the doctor finds they'll
be a link to a section on this page to help you fix it.

## Could not find the clang command

The Juvix compiler uses the [Clang compiler](https://clang.llvm.org)
version 13 or later to generate binaries. You need to have Clang
available on your system `$PATH`.

Recommended installation method:

### MacOS

Use [Homebrew](https://brew.sh):

```shell
brew install llvm
```

NB: The distribution of Clang that comes with XCode does not support the
`Wasm` target so you must install the standard Clang distribution.

### Debian / Ubuntu Linux

```shell
sudo apt install clang lldb lld
```

### Arch Linux

```shell
sudo pacman -S llvm lld
```

## Could not find the wasm-ld command

The Juvix compiler required `wasm-ld` (the Wasm linker) to produce
`Wasm` binaries.

Recommended installation method:

### MacOS

`wasm-ld` is included in the [Homebrew](https://brew.sh) llvm
distribution:

```shell
brew install llvm
```

### Debian / Ubuntu Linux

```shell
sudo apt install lldb lld
```

### Arch Linux

```shell
sudo pacman -S lld
```

## Newer Clang version required

Juvix requires Clang version 13 or above. See the documentation on
[installing Clang](./doctor.md#could-not-find-the-clang-command).

## Clang does not support the wasm32 target

Juvix requires Clang version 13 or above. See the documentation on
[installing Clang](./doctor.md#could-not-find-the-clang-command).

## Clang does not support the wasm32-wasi target

Juvix uses [WASI - The Wasm System Interface](https://wasi.dev) to
produce binaries that can be executed using a Wasm runtime. The files
necessary to setup Clang with `wasm32-wasi` support are available at
[wasi-sdk](https://github.com/WebAssembly/wasi-sdk/releases).

To install the `wasm32-wasi` target for Clang you need to do two things:

### Install `libclang_rt.builtins-wasm32.a` into your Clang distribution

1.  Obtain `libclang_rt.builtins-wasm32-wasi-16.0.tar.gz` from the
    [wasi-sdk
    releases](https://github.com/WebAssembly/wasi-sdk/releases) page.

2.  Untar the file and place the file
    `lib/wasi/libclang_rt.builtins-wasm32.a` into your Clang
    distribution directory.

    On MacOS, if you installed llvm using homebrew you can find the
    Clang distribution directory using `brew --prefix llvm`. You should
    then place the builtins file at
    `` `brew --prefix llvm`/lib/wasi/libclang_rt.builtins-wasm32.a ``.

    On Linux the Clang distribution directory will be something like
    `/usr/lib/clang/13.0.1` where `13.0.1` is the version of Clang that
    you have installed. You should then place the builtins file at
    `/usr/lib/clang/13.0.1/lib/wasi/libclang_rt.builtins-wasm32`.

### Download the WASI sysroot and set `WASI_SYSROOT_PATH`

1.  Obtain `wasi-sysroot-16.0.tar.gz` from the [wasi-sdk
    releases](https://github.com/WebAssembly/wasi-sdk/releases) page.
2.  Untar the file and set the environment variable `WASI_SYSROOT_PATH`
    to that location.

## Environment variable `WASI_SYSROOT_PATH` is not set

Set the `WASI_SYSROOT_PATH` to the directory where you installed the
`wasi-sdk` sysroot files. See [installing the WASI
sysroot](./doctor.md#download-the-wasi-sysroot-and-set-wasi_sysroot_path).

## Could not find the wasmer command

The Juvix test suite uses [Wasmer](https://wasmer.io) as a Wasm runtime
to execute compiled Wasm binaries. See [the Wasmer
documentation](https://docs.wasmer.io/ecosystem/wasmer/getting-started)
to see how to install it.
