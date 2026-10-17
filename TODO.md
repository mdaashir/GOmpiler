# Plans

- [x] Slicing (string via `substr`, slices via `_slice`)
- [ ] Look into using libgolang.h: https://lab.nexedi.com/kirr/pyglang/master/golang/libgolang.h
- [x] Generate the sprintf function programatically, but only for the needed amount of arguments.
- [x] `chan` / `go` / `select` (runtime: `channel<T>`, `go()`, try-based select)
- [x] `interface{}` with type assertions and type switches
- [x] Named `interface` dispatch (abstract base classes + virtual methods)
- [ ] Cross-package `import` resolution
- [ ] Stdlib primitives for the core (unicode, utf8, fmt, strings)
- [ ] Self-compile the whole core (ast/lexer/parser/codegen) end to end
