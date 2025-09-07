MScheme Project Guidelines (for contributors)

Scope: This document captures project-specific knowledge that helps advanced contributors build,
test, and extend MScheme safely and efficiently. It intentionally omits generic Gradle/JUnit basics.

1. Build and Configuration

- Tooling
    - Gradle Wrapper is provided; use gradlew.bat on Windows shells. Do not require a global Gradle
      installation.
    - Kotlin JVM plugin: 2.2.0
    - Tests: JUnit 4.13.1
- Generated sources pipeline (project-specific)
    - A custom Gradle task createInit consumes Scheme sources from src\main\scheme and generates
      build\generated\mscheme\main\kotlin\mscheme\Init.kt.
    - The generated directory is added to the main source set. compileKotlin dependsOn(createInit),
      so generation happens automatically as part of the build.
    - The generator performs textual normalization:
        - Removes line comments beginning with ';'
        - Escapes quotes and backslashes
        - Flattens to a single line and squeezes whitespace
        - Strips spaces around parentheses
    - Practical implications:
        - If a .scm file contains unusual quoting or very large forms, validate that the generated
          Init.kt compiles. Misbalanced delimiters or exotic characters may cause generation or
          compile failures.
        - To regenerate after modifying .scm files: run gradlew clean build.
- Verified build commands
    - Full build: .\gradlew build (verified successful on this repository)
    - Assemble app (if you only need the binary): .\gradlew assemble
- Running the application
    - The application plugin main class is mscheme.Main. You can run via: .\gradlew run

2. Testing

- Framework and layout
    - JUnit 4 is used (org.junit.Test, org.junit.Assert.*). Tests live under src\test\java with Java
      packages aligned to mscheme.*. Kotlin tests are not currently configured.
- Running tests
    - All tests: .\gradlew check (verified)
    - Single class: .\gradlew check --tests "mscheme.tests.TestValue"
    - Class wildcard: .\gradlew check --tests "mscheme.tests.*"
    - Single method (JUnit 4, Gradle supports test filters by method): .\gradlew check --tests "
      mscheme.tests.TestValue.someMethod"
    - Rerun failed tests only: .\gradlew check --tests @failed-tests.txt (Gradle test filtering
      accepts @file syntax if created; otherwise prefer --tests patterns)
- Adding new tests
    - Place test classes under src\test\java in an appropriate package (e.g., package
      mscheme.values;), and annotate methods with @Test.
    - Use Assert.* from JUnit 4. If you need fixtures, standard @Before/@After apply.
    - If your test interacts with Scheme bootstrap strings, remember the build’s generated-sources
      pipeline uses normalized Scheme text; tests that read those strings should not expect exact
      whitespace.
- Demo test (process that was validated)
    - A demonstration test was created temporarily to validate instructions:
        - File: src\test\java\mscheme\DemoGuidelinesTest.java
        - Content snippet: assertEquals("MScheme demo test should pass", 2, 1 + 1);
        - Executed via: .\gradlew check --tests "mscheme.DemoGuidelinesTest" (passed)
    - The file has been removed to keep the repository clean; the command was verified beforehand as
      required by this guide.

3. Project Architecture and Development Notes

- Kotlin-first codebase (Java remnants)
    - The project is actively transitioning from Java to Kotlin. Most new and migrated modules are
      in Kotlin under src\main\java\mscheme\..., while some Java files remain as transitional
      remnants (e.g., parts of exceptions and certain reflection utilities like Builtins).
    - Prefer Kotlin for all new code and refactors; maintain interop with remaining Java until
      migration completes. Kotlin 2.2.0 targets the JVM and interoperates with Java seamlessly.
- Builtins and reflection specifics
    - mscheme.values.functions.Builtins performs two reflection-based registrations when
      getBuiltins(Environment) is called:
        1) Kotlin reflection: Scans KProperty0 public members on Builtins and registers values that
           are Function instances. The Scheme-visible name is derived by parseName (see below).
        2) Java reflection: Scans public static methods with specific parameter shapes returning
           Object; defines them into the Environment as well.
    - Name mangling (parseName): A leading "__" is skipped; substrings of the form _XX are decoded
      where XX is a two-digit hex code. This allows encoding Scheme symbols that aren’t legal Java
      identifiers. Keep this in mind when adding new builtin functions or properties; encode
      non-identifier characters as _xx hex.
- Scheme bootstrap
    - The content of src\main\scheme\*.scm is normalized into a single-line Kotlin string in
      Init.kt. If you rely on original formatting (e.g., for pretty-print tests), keep a copy in
      test resources and avoid coupling to the generated string formatting.
- Code style & conventions (pragmatic guidance)
    - Java: Conventional Google/Oracle style is used informally; keep methods small and prefer
      final/immutability where applicable.
    - Kotlin: Prefer expression bodies and data classes where applicable but remain Java-interop
      friendly (avoid complex inline/reified APIs in cross-boundary code unless needed).
    - Exceptions: Project defines fine-grained mscheme.exceptions.* types. Prefer
      throwing/propagating these over generic RuntimeException to keep the interpreter’s error
      reporting consistent.
    - Arity and functions: Many Scheme functions are modeled via mscheme.util.Arity and
      mscheme.values.functions.* helpers (UnaryFunction, BinaryFunction, etc.). Reuse these instead
      of bespoke arity checks.
- Debugging tips
    - When builtin registration fails, Builtins.getBuiltins catches and logs the error with the
      Scheme name; check stderr messages.
    - If Gradle shows test up-to-date but you need to re-execute: use --rerun-tasks or delete build\
      to force execution.
    - Generation failures in createInit often stem from unmatched parentheses or quoting in .scm;
      reproduce by running: .\gradlew createInit --stacktrace and inspect
      build\generated\mscheme\main\kotlin\mscheme\Init.kt.

4. Common Tasks Cheat Sheet

- Clean build (force regeneration and compilation):
    - .\gradlew clean build
- Run all tests with full logs:
    - .\gradlew check --info
- Run a focused test class or method:
    - .\gradlew check --tests "mscheme.machine.MachineTest"
    - .\gradlew check --tests "mscheme.tests.TestValue.testNumbers"
- Launch the REPL/application (if configured in Main):
    - .\gradlew run

5. Environment Assumptions

- JDK: A modern JDK compatible with Kotlin 2.2.0 is required (JDK 17+ recommended). The build does
  not fix a specific toolchain, so ensure JAVA_HOME points to a compatible JDK.
- OS: Repository uses Windows-friendly paths in docs; Gradle wrapper works cross-platform (use
  ./gradlew on Unix).

Change verification performed for this guide

- Built successfully with: .\gradlew build
- Ran full test suite successfully with:
    .\gradlew check
- Created and executed a demo test with:
    .\gradlew check --tests "mscheme.DemoGuidelinesTest" 
  which passed, then removed the file.
