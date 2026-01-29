#!/bin/bash
# Compile all Std modules to bytecode with correct module names
#
# Usage: ./compile-std-bytecode.sh [output_dir]
#
# This script compiles all Std/*.lean files from the Lean source tree
# to .leanbc bytecode files.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEAN="$SCRIPT_DIR/build/release/stage1/bin/lean"
SRC_DIR="$SCRIPT_DIR/src"
OUTPUT_DIR="${1:-/tmp/std-bytecode}"

if [ ! -d "$SRC_DIR/Std" ]; then
    echo "Error: Std source directory not found at $SRC_DIR/Std"
    exit 1
fi

export LEAN_PATH="$SCRIPT_DIR/build/release/stage1/lib/lean"

if [ ! -x "$LEAN" ]; then
    echo "Error: Lean compiler not found at $LEAN"
    echo "Run 'make -j -C build/release' first"
    exit 1
fi

mkdir -p "$OUTPUT_DIR"

echo "Compiling Std modules to bytecode..."
echo "Source: $SRC_DIR"
echo "Output: $OUTPUT_DIR"
echo ""

success=0
failed=0
skipped=0

# Find all Std .lean files
while IFS= read -r -d '' lean_file; do
    # Get relative path from src dir
    rel_path="${lean_file#$SRC_DIR/}"
    # Convert to module name (Std/Data/HashMap.lean -> Std.Data.HashMap)
    mod_name="${rel_path%.lean}"
    mod_name="${mod_name//\//.}"
    # Output file
    bc_file="$OUTPUT_DIR/${mod_name}.leanbc"

    # Skip if output is newer than input
    if [ -f "$bc_file" ] && [ "$bc_file" -nt "$lean_file" ]; then
        ((skipped++))
        continue
    fi

    if "$LEAN" --root="$SRC_DIR" -Y "$bc_file" "$lean_file" 2>/dev/null; then
        ((success++))
        # Show progress every 20 files
        if (( success % 20 == 0 )); then
            echo "  Compiled $success files..."
        fi
    else
        ((failed++))
        echo "  FAILED: $mod_name"
    fi
done < <(find "$SRC_DIR/Std" -name "*.lean" -print0 | sort -z)

echo ""
echo "Done!"
echo "  Success: $success"
echo "  Failed:  $failed"
echo "  Skipped: $skipped (up to date)"
echo ""

if [ "$success" -gt 0 ] || [ "$skipped" -gt 0 ]; then
    # Calculate total size
    total_size=$(du -sh "$OUTPUT_DIR" | cut -f1)
    file_count=$(find "$OUTPUT_DIR" -name "*.leanbc" | wc -l | tr -d ' ')
    echo "Output: $file_count bytecode files ($total_size)"
fi
