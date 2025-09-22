# Agent Guidelines for OmopHelpers

This document provides instructions for agentic coding agents operating in this repository.

## Build, Lint, and Test

- **Check Package**: `R CMD check .`
- **Build Package**: `R CMD build .`
- **Generate Documentation**: `roxygen2::roxygenise()`
- **Run Tests**: No dedicated test suite found. For a single file, use `devtools::test_file("tests/testthat/test-file.R")`.

## Code Style

- **Style**: Follow the tidyverse style guide. Use `styler` and `lintr` for formatting and linting.
- **Imports**: Use `library()` or `require()` at the top of scripts.
- **Formatting**: Use `styler::style_pkg()` to format the entire package.
- **Types**: Use explicit type checks where necessary.
- **Naming**: Use `snake_case` for variables and functions.
- **Error Handling**: Use `tryCatch()` for error handling.
- **Comments**: Use `roxygen2` comments for functions.
