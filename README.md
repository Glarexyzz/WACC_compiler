# WACC Compiler

This repository contains a compiler for the [WACC language](https://teaching.doc.ic.ac.uk/languages/wacc/), developed as part of an academic project. The compiler is written in Scala and supports the full WACC language specification, including parsing, semantic analysis, and code generation.

## Features

- **Lexer and Parser:** Converts WACC source code into an abstract syntax tree (AST).
- **Semantic Checker:** Ensures type safety and correct program structure.
- **Code Generation:** Outputs ARM assembly code from valid WACC programs.
- **Comprehensive Test Suite:** Includes both valid and invalid WACC programs for testing.

## Directory Structure

- **compile**  
  Script to build and/or run the compiler.

- **LICENSE**  
  Project license file.

- **Makefile**  
  Makefile for building the project.

- **project.scala**  
  Scala project configuration.

- **wacc.target**  
  Build artifact/target file.

- **src/**
  - **main/wacc/**
    - **back-end/**
      - **classes/**
        - `condition.scala` — Condition handling for code generation.
        - `IR.scala` — Intermediate representation definitions.
        - `registers.scala` — Register management for code generation.
      - `CodeGen.scala` — Main code generation logic.
      - `helpers.scala` — Helper functions for the backend.
    - **front-end/**
      - **classes/**
        - `expr.scala` — Expression AST definitions.
        - `stmt.scala` — Statement AST definitions.
        - `types.scala` — Type system definitions.
      - `errorBuilder.scala` — Error reporting utilities.
      - `lexer.scala` — Lexical analysis (tokenizer).
      - `parser.scala` — Syntax analysis (parser).
      - `semanticChecker.scala` — Semantic analysis.
    - `Constants.scala` — Language constants and configuration.
    - `Main.scala` — Compiler entry point.
  - **test/wacc/**
    - `backend-tests.scala` — Backend unit tests.
    - `frontend-tests.scala` — Frontend unit tests.
    - **invalid_semantics/** — WACC programs with semantic errors.
    - **invalid_syntax/** — WACC programs with syntax errors.
    - **valid/** — Valid WACC programs for testing.

## Build & Run Instructions

### Prerequisites

- [Scala](https://www.scala-lang.org/) (version X.Y.Z)
- [sbt](https://www.scala-sbt.org/) (Scala build tool)
- ARM toolchain (for assembling and running generated code, if needed)

### Build

To build the project, run:

```sh
make
```

Or, if using sbt directly:

```sh
sbt compile
```

### Run

To compile a WACC program:

```sh
./compile path/to/source.wacc
```

This will generate ARM assembly code as output.

## Testing

Test cases are located in `src/test/wacc/` and are organized into valid and invalid categories. To run the test suite:

```sh
sbt test
```

Or use any provided scripts for automated testing.

## Contributing

Contributions are welcome! Please open issues or pull requests for bug fixes or new features.

## License

This project is MIT licensed under the terms of the [LICENSE](LICENSE) file.

---

*Developed as part of the WACC course at Imperial College London.*