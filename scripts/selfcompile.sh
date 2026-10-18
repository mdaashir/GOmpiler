#!/usr/bin/env bash
#
# Self-compilation check.
#
# Transpiles go2cpp's own core (pkg/ast + pkg/lexer + pkg/parser + pkg/codegen,
# reached through pkg/selfhost) into a single C++ translation unit, builds that
# unit with g++, and then verifies that the resulting native binary produces
# byte-identical C++ output to the Go-built go2cpp for every testcase.
#
# Requirements: go, a C++20 compiler (g++), bash.
set -u

cd "$(dirname "$0")/.."
CXX="${CXX:-g++}"
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

echo "== building go2cpp (Go) =="
go build -o "$TMP/go2cpp" ./cmd/go2cpp || exit 1

echo "== transpiling the core to C++ =="
"$TMP/go2cpp" -S -o "$TMP/selfhost.cpp" pkg/selfhost/selfhost.go || exit 1
echo "   generated $(wc -l < "$TMP/selfhost.cpp") lines"

# A driver that runs the transpiled core over stdin.
cat >> "$TMP/selfhost.cpp" <<'EOF'

// ---- self-compilation driver ----
int main() {
    std::stringstream _in;
    _in << std::cin.rdbuf();
    auto _res = selfhost::Transpile(_in.str());
    std::cout << std::get<0>(_res);
    return 0;
}
EOF

echo "== compiling the transpiled core with $CXX =="
"$CXX" -std=c++2a -O2 -fpermissive -pthread -o "$TMP/selfhost_core" "$TMP/selfhost.cpp" || exit 1

echo "== comparing output: go-built vs self-compiled core =="
same=0
diffn=0
failed=0
for f in testcases/*.go; do
  if ! "$TMP/go2cpp" -S "$f" > "$TMP/from_go.cpp" 2>/dev/null; then
    failed=$((failed + 1)); echo "  FAIL(go): $f"; continue
  fi
  if ! "$TMP/selfhost_core" < "$f" > "$TMP/from_core.cpp" 2>/dev/null; then
    failed=$((failed + 1)); echo "  FAIL(core): $f"; continue
  fi
  if diff -q "$TMP/from_go.cpp" "$TMP/from_core.cpp" >/dev/null; then
    same=$((same + 1))
  else
    diffn=$((diffn + 1)); echo "  DIFF: $f"
  fi
done

echo ""
echo "==== self-compilation: $same identical, $diffn different, $failed failed ===="
[ "$diffn" -eq 0 ] && [ "$failed" -eq 0 ]
