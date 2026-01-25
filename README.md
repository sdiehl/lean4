# Lean 4 Rust Backend

This is a fork of Lean 4 with an experimental Rust code generator.

## Build

```bash
make -j -C build/release
```

## Usage

```bash
./build/release/stage1/bin/lean --rust-bin=output input.lean
./output
```

The runtime is automatically cloned and built to `~/.lean/lean-runtime` on first use.

To use a custom runtime location:

```bash
export LEAN_RUNTIME_PATH=/path/to/lean-runtime
```

## Generate Rust Source Only

```bash
./build/release/stage1/bin/lean --rust=output.rs input.lean
```
