#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEAN4_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
RUNTIME_DIR="${LEAN_RUST_RUNTIME:-$LEAN4_DIR/../lean-runtime}"

# Build runtime if needed
if [[ ! -f "$RUNTIME_DIR/target/release/liblean_runtime.rlib" ]]; then
    echo "Building lean-runtime..."
    cd "$RUNTIME_DIR"
    cargo build --release
fi

export LEAN_RUST_RUNTIME="$RUNTIME_DIR"
LAKE="$LEAN4_DIR/build/release/stage1/bin/lake"

run_test() {
    local name="$1"
    local exe="$2"
    local expected="$3"

    echo "Testing $name..."
    cd "$SCRIPT_DIR/$name"
    rm -rf .lake
    $LAKE build 2>&1 | grep -v "^info:" || true
    OUTPUT=$(".lake/build/bin/$exe")

    if [[ "$OUTPUT" == "$expected" ]]; then
        echo "$name: ok"
    else
        echo "$name: FAILED"
        echo "Expected: $expected"
        echo "Got: $OUTPUT"
        exit 1
    fi
}

run_test "hello" "hello" "Hello, world!"

run_test "fib" "fib" "Hello from Rust backend!
fib 10 = 55
fib 20 = 6765"

run_test "multi" "multi" "Hello, World!
42
55"

echo "All tests passed"
