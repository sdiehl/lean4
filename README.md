# Lean 4 Rust Backend

Fork of Lean 4 with an experimental Rust code generator.

## Build

```bash
make -j -C build/release
```

The compiler is in `./build/release/stage1/bin/lean` put this directory in your
`PATH` so that our bespoke `lake` and `lean` binaries are available.

Then create a minimal` Lean project with `lakefile.lean`:

```lean
import Lake
open Lake DSL

package my_app

@[default_target]
lean_exe my_app where
  root := `Main
  backend := .rust
```

Clone the Rust runtime to some path `/path/to/lean-runtime`. Then with our
custom compiler and lake in your `PATH`, build and run the project:

```bash
export LEAN_RUST_RUNTIME=/path/to/lean-runtime
lake build
.lake/build/bin/my_app
```

To generate the source code only:

```bash
./build/release/stage1/bin/lean --rust=output.rs input.lean
```

## Environment Variables

| Variable                   | Description                                |
| -------------------------- | ------------------------------------------ |
| `LEAN_RUST_RUNTIME`        | Path to lean-runtime crate (required)      |
| `LAKE_RUST_FMT`            | Set to `0` to disable rustfmt              |
| `LAKE_RUST_KEEP_ARTIFACTS` | Set to `1` to keep `.rust-build` directory |


## Multi-Module Projects

```lean
lean_lib MyLib where
  backend := .rust

lean_exe my_app where
  root := `Main
  backend := .rust
```
