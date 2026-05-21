## DeltaQ Oscilloscope

The DeltaQ Oscilloscope is a C++ graphical interface to observe running Erlang programs, giving real-time insights about the instrumented system under test. It needs to be paired with the [dqsd_otel](https://github.com/fnieri/dqsd_otel) Erlang wrapper, which sends execution data over a TCP socket. The oscilloscope processes that data in real time and performs statistical computations to give detailed insights about the running system.

This project is part of a master thesis.

---

## Dependencies

The following must be installed before building. Everything else is fetched automatically by [CPM](https://github.com/cpm-cmake/CPM.cmake) at configure time.

| Dependency | Purpose |
|---|---|
| CMake ≥ 3.30 | Build system |
| GCC / Clang (C++17) | Compiler |
| Qt 6 | GUI framework |
| ANTLR4 C++ runtime | Parser runtime (generated files are committed; only needed if you modify the grammar) |

---

## Installing dependencies

### macOS (Homebrew)

```bash
brew install cmake qt6 antlr4-cpp-runtime
```

### Ubuntu / Debian

```bash
sudo apt install cmake g++ qt6-base-dev libantlr4-runtime-dev
```

### Fedora

```bash
sudo dnf install cmake gcc-c++ qt6-qtbase-devel antlr4-cpp-runtime-devel
```

### Windows

Install [CMake](https://cmake.org/download/) and [Qt 6](https://doc.qt.io/qt-6/get-and-install-qt.html), then install the ANTLR4 C++ runtime via vcpkg:

```bash
vcpkg install antlr4
```

---

## Building

### 1. Configure

**Linux** (Qt6 installed system-wide):
```bash
make setup
```

**macOS** (Homebrew):
```bash
make setup QT_PREFIX=$(brew --prefix qt6)
```

**Custom Qt path** (any platform):
```bash
make setup QT_PREFIX=/path/to/qt6
```

### 2. Compile

```bash
make build
```

The binary is placed at `build/bin/DQOscilloscope`.

### 3. Run

```bash
./build/bin/DQOscilloscope
```

---

## Tests

```bash
make test
```

---

## Modifying the grammar

The ANTLR-generated parser files are committed under `src/parser/generated/` so most contributors do not need the `antlr4` tool installed. If you change `src/parser/DQGrammar.g4`, regenerate them by re-running `make setup` with the `antlr4` tool available in your `PATH` — CMake will detect it and rebuild only the affected files.
