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
COMPARE_ONLY=false

for arg in "$@"; do
  case $arg in
    --generate)
      GENERATE=true
      shift
      ;;
    --compare)
      COMPARE_ONLY=true
      shift
      ;;
    --help)
      echo "Usage: $0 [options]"
      echo "  --generate    Regenerate all test outputs"
      echo "  --compare     Compare outputs against golden files only"
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

# Track results
PASSED=0
FAILED=0

if [ "$COMPARE_ONLY" = false ]; then
  echo "--- Integration Tests ---"
  echo ""

  # Run tests for each backend
  BACKENDS=(
    "CarpetX:test:.hxx"
    "CarpetX:testGPU:.hxx"
    "CarpetXPointDesc:test:.hxx"
    "Carpet:test:.hxx"
    "AMReX:test:.hxx"
    "Nmesh:test:.c"
    "Nmesh:GHG_rhs:.c"
  )

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
fi

echo "--- Golden File Comparison ---"
echo ""

# Compare all outputs
BACKENDS=(
  "CarpetX:test:.hxx"
  "CarpetX:testGPU:.hxx"
  "CarpetXPointDesc:test:.hxx"
  "Carpet:test:.hxx"
  "AMReX:test:.hxx"
  "Nmesh:test:.c"
  "Nmesh:GHG_rhs:.c"
)

for entry in "${BACKENDS[@]}"; do
  IFS=':' read -r backend test_name ext <<< "$entry"
  compare_golden "$backend" "$test_name" "$ext"
done

echo ""
echo "========================================"
echo "  Summary"
echo "========================================"
echo ""

if [ "$COMPARE_ONLY" = false ]; then
  echo "Integration: Passed: $PASSED, Failed: $FAILED"
fi

if [ "$FAILED" -gt 0 ]; then
  echo -e "${RED}TESTS FAILED${NC}"
  exit 1
else
  echo -e "${GREEN}ALL TESTS PASSED${NC}"
  exit 0
fi
