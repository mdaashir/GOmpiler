# Downloads

Every release ships five targets — Linux, macOS and Windows, on amd64 and arm64 — plus a
`checksums.txt` you can verify against.

## GOmpiler

**Latest: [v2.0.0](https://github.com/mdaashir/GOmpiler/releases/tag/v2.0.0)** — the C++ compiler.

| Platform | Binary |
| --- | --- |
| Linux amd64 | [`gompiler_linux_amd64`](https://github.com/mdaashir/GOmpiler/releases/download/v2.0.0/gompiler_linux_amd64) |
| Linux arm64 | [`gompiler_linux_arm64`](https://github.com/mdaashir/GOmpiler/releases/download/v2.0.0/gompiler_linux_arm64) |
| macOS amd64 | [`gompiler_darwin_amd64`](https://github.com/mdaashir/GOmpiler/releases/download/v2.0.0/gompiler_darwin_amd64) |
| macOS arm64 | [`gompiler_darwin_arm64`](https://github.com/mdaashir/GOmpiler/releases/download/v2.0.0/gompiler_darwin_arm64) |
| Windows amd64 | [`gompiler_windows_amd64.exe`](https://github.com/mdaashir/GOmpiler/releases/download/v2.0.0/gompiler_windows_amd64.exe) |

## go2cpp

**Latest: [v0.1.0](https://github.com/mdaashir/GOmpiler/releases/tag/v0.1.0)** — the Go → C++20
transpiler.

| Platform | Binary |
| --- | --- |
| Linux amd64 | [`go2cpp_linux_amd64`](https://github.com/mdaashir/GOmpiler/releases/download/v0.1.0/go2cpp_linux_amd64) |
| Linux arm64 | [`go2cpp_linux_arm64`](https://github.com/mdaashir/GOmpiler/releases/download/v0.1.0/go2cpp_linux_arm64) |
| macOS amd64 | [`go2cpp_darwin_amd64`](https://github.com/mdaashir/GOmpiler/releases/download/v0.1.0/go2cpp_darwin_amd64) |
| macOS arm64 | [`go2cpp_darwin_arm64`](https://github.com/mdaashir/GOmpiler/releases/download/v0.1.0/go2cpp_darwin_arm64) |
| Windows amd64 | [`go2cpp_windows_amd64.exe`](https://github.com/mdaashir/GOmpiler/releases/download/v0.1.0/go2cpp_windows_amd64.exe) |

## Verify a download

```bash
curl -LO https://github.com/mdaashir/GOmpiler/releases/download/v2.0.0/checksums.txt
curl -LO https://github.com/mdaashir/GOmpiler/releases/download/v2.0.0/gompiler_linux_amd64
sha256sum -c checksums.txt --ignore-missing
chmod +x gompiler_linux_amd64
```

On macOS use `shasum -a 256 -c checksums.txt --ignore-missing`.

## Build from source instead

Both projects are plain Go and build with a single command. Go 1.24 or newer is required.

```bash
git clone https://github.com/mdaashir/GOmpiler.git
cd GOmpiler

go build -o gompiler .            # GOmpiler, on master

git switch go2cpp
go build -o go2cpp ./cmd/go2cpp   # the transpiler
```

Every release is also reproducible from its tag: the release workflow builds the same five targets
with `-trimpath` and a stamped version.

## Runtime requirements

- **GOmpiler** needs `as` and `ld` (binutils) to assemble and link.
- **go2cpp** needs a C++20 compiler — `g++` or `clang++` — to compile its output.
