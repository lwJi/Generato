#!/bin/bash
#
# run_tests.sh - Run all Generato tests
#
# Usage: ./test/run_tests.sh [options]
#   --generate    Regenerate all test outputs
#   --compare     Compare outputs against golden files only
#   --help        Show this help message

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GENERATO_DIR="$(dirname "$SCRIPT_DIR")"

export GENERATO="$GENERATO_DIR"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Parse arguments
GENERATE=false

for arg in "$@"; do
  case $arg in
    --generate)
      GENERATE=true
      shift
      ;;
    --help)
      echo "Usage: $0 [options]"
      echo "  --generate    Update golden files with new outputs"
      echo "  --help        Show this help message"
      exit 0
      ;;
  esac
done

echo ""
echo "========================================"
echo "  Generato Test Suite"
echo "========================================"
echo ""

# Function to run a test
run_test() {
  local backend=$1
  local test_name=$2

  echo -e "${YELLOW}Running: ${backend}/${test_name}.wl${NC}"

  cd "$SCRIPT_DIR/$backend"

  if "$GENERATO_DIR/Generato" "${test_name}.wl" 2>&1; then
    echo -e "${GREEN}  OK${NC}"
    return 0
  else
    echo -e "${RED}  FAILED${NC}"
    return 1
  fi
}

# Function to compare with golden file
compare_golden() {
  local backend=$1
  local test_name=$2
  local ext=$3

  local current="$SCRIPT_DIR/$backend/${test_name}${ext}"
  local golden="$SCRIPT_DIR/golden/$backend/${test_name}${ext}.golden"

  if [ ! -f "$current" ]; then
    echo -e "${YELLOW}  SKIP: $current not found${NC}"
    return 0
  fi

  if [ ! -f "$golden" ]; then
    echo -e "${YELLOW}  SKIP: Golden file not found${NC}"
    return 0
  fi

  if diff -q "$current" "$golden" > /dev/null 2>&1; then
    echo -e "${GREEN}  PASS: ${backend}/${test_name}${ext}${NC}"
    return 0
  else
    echo -e "${RED}  FAIL: ${backend}/${test_name}${ext} differs from golden${NC}"
    return 1
  fi
}

# Function to update golden file
update_golden() {
  local backend=$1
  local test_name=$2
  local ext=$3

  local current="$SCRIPT_DIR/$backend/${test_name}${ext}"
  local golden_dir="$SCRIPT_DIR/golden/$backend"
  local golden="$golden_dir/${test_name}${ext}.golden"

  if [ ! -f "$current" ]; then
    echo -e "${YELLOW}  SKIP: $current not found${NC}"
    return 0
  fi

  mkdir -p "$golden_dir"
  cp "$current" "$golden"
  echo -e "${GREEN}  UPDATED: ${backend}/${test_name}${ext}${NC}"
}

# Function to cleanup generated outputs
cleanup_outputs() {
  for entry in "${BACKENDS[@]}"; do
    IFS=':' read -r backend test_name ext <<< "$entry"
    local output_file="$SCRIPT_DIR/$backend/${test_name}${ext}"
    if [ -f "$output_file" ]; then
      rm "$output_file"
    fi
  done
}

# Track results
PASSED=0
FAILED=0

# Load test cases from config file
CONFIG_FILE="$SCRIPT_DIR/test_cases.txt"
if [ ! -f "$CONFIG_FILE" ]; then
  echo -e "${RED}ERROR: Config file not found: $CONFIG_FILE${NC}"
  exit 1
fi

# Read config file into BACKENDS array (skip comments and empty lines)
BACKENDS=()
while IFS= read -r line || [ -n "$line" ]; do
  # Skip comments and empty lines
  [[ "$line" =~ ^#.*$ || -z "$line" ]] && continue
  BACKENDS+=("$line")
done < "$CONFIG_FILE"

echo "--- Integration Tests ---"
echo ""

for entry in "${BACKENDS[@]}"; do
  IFS=':' read -r backend test_name ext <<< "$entry"

  # Skip if test file doesn't exist
  if [ ! -f "$SCRIPT_DIR/$backend/${test_name}.wl" ]; then
    echo -e "${YELLOW}SKIP: ${backend}/${test_name}.wl not found${NC}"
    continue
  fi

  if run_test "$backend" "$test_name"; then
    ((PASSED++)) || true
  else
    ((FAILED++)) || true
  fi
done

echo ""

if [ "$GENERATE" = true ]; then
  echo "--- Updating Golden Files ---"
  echo ""

  for entry in "${BACKENDS[@]}"; do
    IFS=':' read -r backend test_name ext <<< "$entry"
    update_golden "$backend" "$test_name" "$ext"
  done
else
  echo "--- Golden File Comparison ---"
  echo ""

  for entry in "${BACKENDS[@]}"; do
    IFS=':' read -r backend test_name ext <<< "$entry"
    compare_golden "$backend" "$test_name" "$ext"
  done
fi

# Cleanup generated outputs
cleanup_outputs

echo ""
echo "========================================"
echo "  Summary"
echo "========================================"
echo ""

echo "Integration: Passed: $PASSED, Failed: $FAILED"

if [ "$FAILED" -gt 0 ]; then
  echo -e "${RED}TESTS FAILED${NC}"
  exit 1
else
  echo -e "${GREEN}ALL TESTS PASSED${NC}"
  exit 0
fi
