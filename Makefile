# go2cpp — build, test and verification targets.

GO  ?= go
CXX ?= g++

.PHONY: all build test vet difftest selfcompile clean

all: build test

# Build the command-line compiler into ./go2cpp.
build:
	$(GO) build -o go2cpp ./cmd/go2cpp

test:
	$(GO) test ./...

vet:
	$(GO) vet ./...

# Differential test: compare `go run` output with the compiled C++ output of the
# same program, for every program in testdata/. Requires $(CXX).
difftest:
	./scripts/difftest.sh

# Self-compilation check: transpile go2cpp's own core to C++, rebuild it, and
# verify it produces identical output. Requires $(CXX).
selfcompile:
	./scripts/selfcompile.sh

clean:
	rm -rf go2cpp build
