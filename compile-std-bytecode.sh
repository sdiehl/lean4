#!/bin/bash
# Compile all Std modules to bytecode with correct module names
#
# Usage: ./compile-std-bytecode.sh [std_source_dir] [output_dir]
#
# This script compiles all Std/*.lean files to .leanbc bytecode files.
# The Std source can be obtained from: https://github.com/leanprover/std4
#
# Example:
#   git clone https://github.com/leanprover/std4 /tmp/std4
#   ./compile-std-bytecode.sh /tmp/std4 /tmp/std-bytecode

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LEAN="$SCRIPT_DIR/build/release/stage1/bin/lean"
STD_DIR="${1:-}"
OUTPUT_DIR="${2:-/tmp/std-bytecode}"

if [ -z "$STD_DIR" ]; then
    echo "Usage: $0 <std_source_dir> [output_dir]"
    echo ""
    echo "The Std source can be obtained with:"
    echo "  git clone https://github.com/leanprover/std4 /tmp/std4"
    echo ""
    echo "Then run:"
    echo "  $0 /tmp/std4 /tmp/std-bytecode"
    exit 1
fi

if [ ! -d "$STD_DIR/Std" ]; then
    echo "Error: Std source directory not found at $STD_DIR/Std"
    echo "Expected directory structure: $STD_DIR/Std/*.lean"
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
echo "Source: $STD_DIR"
echo "Output: $OUTPUT_DIR"
echo ""

success=0
failed=0
skipped=0

# Find all Std .lean files
while IFS= read -r -d '' lean_file; do
    # Get relative path from std dir
    rel_path="${lean_file#$STD_DIR/}"
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

    if "$LEAN" --root="$STD_DIR" -Y "$bc_file" "$lean_file" 2>/dev/null; then
        ((success++))
        # Show progress every 20 files
        if (( success % 20 == 0 )); then
            echo "  Compiled $success files..."
        fi
    else
        ((failed++))
        echo "  FAILED: $mod_name"
    fi
done < <(find "$STD_DIR/Std" -name "*.lean" -print0 | sort -z)

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
