#!/usr/bin/env bash
# Test VM backend Lake integration (build only, no exe)
# This tests that lake build produces .leanbc bytecode files correctly
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEAN4_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"

LAKE="$LEAN4_DIR/build/release/stage1/bin/lake"

# Fallback to build/stage1 if build/release doesn't exist
if [[ ! -f "$LAKE" ]]; then
    LAKE="$LEAN4_DIR/build/stage1/bin/lake"
fi

run_build_test() {
    local name="$1"

    echo "Testing $name (build only)..."
    cd "$SCRIPT_DIR/$name"
    rm -rf .lake
    $LAKE build 2>&1 | grep -v "^info:" || true

    # Verify .leanbc files were created
    local bc_count=$(find .lake/build/ir -name "*.leanbc" 2>/dev/null | wc -l)
    if [[ "$bc_count" -eq 0 ]]; then
        echo "$name: FAILED - no .leanbc files generated"
        exit 1
    fi

    echo "$name: ok ($bc_count bytecode files)"
}

run_build_test "hello"
run_build_test "fib"
run_build_test "multi"

echo "All VM backend build tests passed"
