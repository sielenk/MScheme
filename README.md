# MScheme

MScheme is a small, educational Scheme interpreter and runtime written primarily in Kotlin for the JVM, with a few Java interop remnants. It aims to be clear, hackable, and well‑tested. The project includes a bootstrap pipeline that turns Scheme sources into a generated Java string used at startup.

- Language/runtime: Kotlin (JVM), Kotlin 2.2.x
- Build: Gradle Wrapper (use the provided gradlew/gradlew.bat)
- Tests: JUnit 4
- License: GPLv2 (see COPYING)

## Quick Start

Prerequisites:
- A modern JDK compatible with Kotlin 2.2.x (JDK 17+ recommended)
- No global Gradle installation is required; use the wrapper scripts in this repo

Clone and build:
```bash
./gradlew build
```

Run the full test suite:
```bash
./gradlew check
```

Run the application (REPL or main program if configured):
```bash
./gradlew run
```

### Focused tests
Gradle supports filtering by class and method names.
- Single class:
  - Windows: `./gradlew.bat check --tests "mscheme.tests.TestValue"`
  - Any OS (example wildcard): `./gradlew check --tests "mscheme.tests.*"`
- Single method (JUnit 4):
  - `./gradlew check --tests "mscheme.tests.TestValue.someMethod"`

If Gradle shows tests up-to-date but you need to rerun, use `--rerun-tasks` or delete the `build` directory.

## Generated Sources Pipeline (Project‑Specific)
MScheme includes a custom generation step that normalizes Scheme sources into a single Java string used during bootstrap.

- Source input: `MScheme/src/main/scheme/*.scm`
- Generated output: `build/generated/mscheme/main/kotlin/mscheme/Init.kt`
- The `createInit` Gradle task performs textual normalization:
  - Removes line comments beginning with `;`
  - Escapes quotes and backslashes
  - Flattens to a single line and squeezes whitespace
  - Strips spaces around parentheses
- The generated directory is added to the main source set. `compileKotlin` depends on `createInit`, so generation happens automatically as part of the build.
- To regenerate after modifying `.scm` files: run `./gradlew[.bat] clean build`.

Practical tips:
- If a `.scm` file contains unusual quoting or very large forms, validate that the generated `Init.kt` compiles (mismatched delimiters or exotic characters can cause failures).
- To debug generation specifically: `./gradlew[.bat] createInit --stacktrace` and inspect `build/generated/mscheme/main/kotlin/mscheme/Init.kt`.

## Project Layout
- `MScheme/` — Main interpreter and runtime (Kotlin-first; some Java remains)
- `MScheme-ksp/` — Kotlin Symbol Processing (KSP) utilities for MScheme
- Tests live under `MScheme/src/test/java` (JUnit 4) and `MScheme/src/test/kotlin` where applicable

### Kotlin-first codebase (with Java interop)
The project is transitioning from Java to Kotlin. Prefer Kotlin for new code and refactors while maintaining Java interop.

Notable components:
- Builtins discovery via KSP: A Kotlin Symbol Processor scans builtin declarations and generates `mscheme.values.functions.getBuiltins()` at build time. The runtime imports and iterates that sequence to populate the Scheme report environment. The `Builtins` object only contains implementations; discovery/registration is generated.
- Name mangling for Scheme-visible names is handled by the KSP generator (encoding non-identifier characters as `_xx` hex when needed).

## Running and Hacking
- Build everything: `./gradlew[.bat] build`
- Assemble only: `./gradlew[.bat] assemble`
- Run: `./gradlew[.bat] run`

If you rely on the original formatting of the Scheme bootstrap strings (e.g., for pretty‑print tests), remember the build’s generated‑sources pipeline uses normalized, single‑line Scheme text. Tests that read those strings should not expect exact whitespace. Keep a copy in test resources if you need preserved formatting.

## Contributing
Contributions are welcome! A few pragmatic conventions:
- Prefer Kotlin for new code; keep Java interop friendly.
- Follow existing code style (concise functions, immutability where reasonable).
- Use the existing helpers for arity and functions (e.g., `mscheme.util.Arity`, `mscheme.values.functions.*`) instead of bespoke checks.
- Prefer throwing/propagating specific exceptions under `mscheme.exceptions.*` for consistent error reporting.
- Add tests under `MScheme/src/test/java` (JUnit 4). Use `org.junit.Test` and `org.junit.Assert.*`.

## License
GPLv2 — see [COPYING](COPYING).

## Status
This README was added/updated on 2025‑09‑06. If something looks out of date, please open an issue or PR.
