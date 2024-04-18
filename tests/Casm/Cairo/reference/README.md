This directory contains reference Cairo0 programs used to obtain the
output values for corresponding Juvix programs.

The programs should be compiled with
```
cairo-compile --proof_mode program.cairo > program.json
```
where `cairo-compile` is the Cairo0 compiler (the Python version).

Then run with:
```
run_cairo_vm.sh program.json
```
