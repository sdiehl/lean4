#!/usr/bin/env bash
# Compile Init modules to Rust
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEAN="$SCRIPT_DIR/build/release/stage1/bin/lean"
RUNTIME_DIR="${LEAN_RUNTIME_PATH:-$SCRIPT_DIR/../lean-runtime}"
OUTPUT_DIR="$SCRIPT_DIR/build/release/stage1/lib/lean/rust"

mkdir -p "$OUTPUT_DIR"

export LEAN_RUNTIME_PATH="$RUNTIME_DIR"

# Find all Init .lean files and compile them
find "$SCRIPT_DIR/src/Init" -name "*.lean" | while read -r lean_file; do
    # Convert path to module name: src/Init/Data/Nat.lean -> Init_Data_Nat
    rel_path="${lean_file#$SCRIPT_DIR/src/}"
    rel_path="${rel_path%.lean}"
    mod_name="${rel_path//\//_}"

    rs_file="$OUTPUT_DIR/${mod_name}.rs"

    echo "Compiling $mod_name..."
    if ! "$LEAN" --rust="$rs_file" "$lean_file" 2>/dev/null; then
        echo "  Failed: $mod_name (skipping)"
        rm -f "$rs_file"
    fi
done

echo "Done. Output in $OUTPUT_DIR"
ls "$OUTPUT_DIR" | wc -l
