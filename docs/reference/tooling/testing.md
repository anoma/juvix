# Testing

### Dependencies

See [Installing dependencies](./doctor.html) for instructions on how to
setup the testing environment for the WASM compiler tests.

### Running

Run tests using:

```shell
stack test
```

To run tests, ignoring all the WASM tests:

```shell
stack test --ta '-p "! /slow tests/"'
```
