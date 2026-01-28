#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEAN4_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
VM_DIR="${LEAN4_VM:-$LEAN4_DIR/../lean4-vm}"

# Build VM if needed
if [[ ! -f "$VM_DIR/target/release/lean4-vm" ]]; then
    echo "Building lean4-vm..."
    cd "$VM_DIR"
    cargo build --release
fi

export PATH="$VM_DIR/target/release:$PATH"

LAKE="$LEAN4_DIR/build/release/stage1/bin/lake"

run_test() {
    local name="$1"
    local exe="$2"
    local expected="$3"

    echo "Testing $name..."
    cd "$SCRIPT_DIR/$name"
    rm -rf .lake
    $LAKE build 2>&1 | grep -v "^info:" || true
    OUTPUT=$($LAKE exe "$exe" 2>&1)

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

run_test "fib" "fib" "Hello from VM backend!
fib 10 = 55
fib 20 = 6765"

run_test "multi" "multi" "Hello, World!
42
55"

echo "All VM backend tests passed"
