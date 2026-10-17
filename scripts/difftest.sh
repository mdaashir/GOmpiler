#!/usr/bin/env bash
#
# Differential test for the AST front-end.
#
# For every program in testcases/, this compares the output of `go run` with the
# output of the binary produced by compiling go2cpp's generated C++20.
#
# Requirements: go, a C++20 compiler (g++), bash. Run from anywhere.
set -u

cd "$(dirname "$0")/.."
ROOT=$(pwd)
CXX="${CXX:-g++}"

go build -o go2cpp ./cmd/go2cpp || exit 1

tests=(
  multiline_string var_string var_multi string_args defer iota map_struct
  for_range_map_key_value for_range_map_value for_range_map_key string_map struct
  var2 for_range_both for_endless for_range_single for_range_list for_regular
  trimspace printf multiple prefix contains if var output_char2 var_zero
  output_char const continue types if_minus_one goto switch hello sprintf
  strings_index strings_replace strings_suffix strings_split methods
  chan_unbuffered chan_buffered goroutine select_chan select_default
)

# Programs whose output word order is unspecified (map iteration order).
unordered=(string_map for_range_map_key_value for_range_map_value for_range_map_key)

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

pass=0
fail=0
failed=()

for t in "${tests[@]}"; do
  src="testcases/$t.go"
  if ! ./go2cpp -S -o "$tmp/$t.cpp" "$src" 2>"$tmp/err"; then
    echo "TRANSPILE FAIL: $t"; head -5 "$tmp/err"; fail=$((fail+1)); failed+=("$t"); continue
  fi
  if ! "$CXX" -std=c++2a -O2 -fpermissive -Wfatal-errors -pthread -o "$tmp/$t" "$tmp/$t.cpp" 2>"$tmp/err"; then
    echo "COMPILE FAIL: $t"; head -5 "$tmp/err"; fail=$((fail+1)); failed+=("$t"); continue
  fi

  goout=$(go run "$src" 2>&1)
  cppout=$("$tmp/$t" 2>&1)

  if [[ " ${unordered[*]} " == *" $t "* ]]; then
    a=$(echo "$cppout" | tr -s '[:space:]' '\n' | sort | tr '\n' ',')
    b=$(echo "$goout" | tr -s '[:space:]' '\n' | sort | tr '\n' ',')
    if [ "$a" = "$b" ]; then pass=$((pass+1)); else fail=$((fail+1)); failed+=("$t"); echo "DIFF (unordered): $t"; fi
  elif [ "$goout" = "$cppout" ]; then
    pass=$((pass+1))
  else
    fail=$((fail+1)); failed+=("$t")
    echo "DIFF: $t"
    echo "  go : $goout"
    echo "  cpp: $cppout"
  fi
done

echo ""
echo "==== $pass passed, $fail failed ===="
if [ "${#failed[@]}" -gt 0 ]; then
  echo "failed: ${failed[*]}"
  exit 1
fi
