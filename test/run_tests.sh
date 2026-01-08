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

# Quiet mode support
quiet_print() {
  [[ "$QUIET" != "1" ]] && echo -e "$@" || true
}

quiet_print_always() {
  echo -e "$@"
}

# Run a phase with output capture in quiet mode
run_phase() {
  local phase_name=$1
  shift

  if [[ "$QUIET" == "1" ]]; then
    local output
    local exit_code
    output=$("$@" 2>&1) && exit_code=$? || exit_code=$?

    if [[ $exit_code -eq 0 ]]; then
      echo -e "${GREEN}PASS: ${phase_name}${NC}"
    else
      echo "$output"
      echo -e "${RED}FAIL: ${phase_name}${NC}"
    fi
    return $exit_code
  else
    "$@"
  fi
}

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

quiet_print ""
quiet_print "========================================"
quiet_print "  Generato Test Suite"
quiet_print "========================================"
quiet_print ""

# Function to run a test
run_test() {
  local backend=$1
  local test_name=$2

  echo -e "${YELLOW}Running: ${backend}/${test_name}.wl${NC}"

  cd "$SCRIPT_DIR/$backend"

  if QUIET=1 "$GENERATO_DIR/Generato" "${test_name}.wl" 2>&1; then
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

# Function to run integration tests phase
run_integration_tests() {
  local passed=0
  local failed=0

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
      ((passed++)) || true
    else
      ((failed++)) || true
    fi
  done

  echo ""
  echo "Integration: Passed: $passed, Failed: $failed"

  # Set global counts for summary
  PASSED=$passed
  FAILED=$failed

  [[ $failed -eq 0 ]]
}

# Function to run golden file comparison phase
run_golden_comparison() {
  local failed=0

  echo "--- Golden File Comparison ---"
  echo ""

  for entry in "${BACKENDS[@]}"; do
    IFS=':' read -r backend test_name ext <<< "$entry"
    if ! compare_golden "$backend" "$test_name" "$ext"; then
      ((failed++)) || true
    fi
  done

  [[ $failed -eq 0 ]]
}

# Function to update golden files
run_update_golden() {
  echo "--- Updating Golden Files ---"
  echo ""

  for entry in "${BACKENDS[@]}"; do
    IFS=':' read -r backend test_name ext <<< "$entry"
    update_golden "$backend" "$test_name" "$ext"
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

# Run integration tests
INTEGRATION_RESULT=0
run_phase "integration" run_integration_tests || INTEGRATION_RESULT=$?

# Run golden comparison or update
GOLDEN_RESULT=0
if [ "$GENERATE" = true ]; then
  run_phase "golden-update" run_update_golden || GOLDEN_RESULT=$?
else
  run_phase "golden" run_golden_comparison || GOLDEN_RESULT=$?
fi

# Cleanup generated outputs
cleanup_outputs

# Show summary (only in verbose mode)
quiet_print ""
quiet_print "========================================"
quiet_print "  Summary"
quiet_print "========================================"
quiet_print ""

quiet_print "Integration: Passed: $PASSED, Failed: $FAILED"

if [ "$INTEGRATION_RESULT" -ne 0 ] || [ "$GOLDEN_RESULT" -ne 0 ]; then
  quiet_print "${RED}TESTS FAILED${NC}"
  exit 1
else
  quiet_print "${GREEN}ALL TESTS PASSED${NC}"
  exit 0
fi
