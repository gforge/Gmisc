# Copilot / AI agent instructions for working on Gmisc

This file contains concise, project-specific guidance so an AI coding agent can be productive quickly.

## Quick operational commands

- Run package-mode tests (recommended):
  - Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat", reporter = "summary")'
  - Or use `devtools::test()` interactively.
- Run a single test file (package mode):
  - Rscript -e 'devtools::load_all(); testthat::test_file("tests/testthat/test-align-deep-path.R", reporter = "summary")'
- Run checks before push / release:
  - R CMD check --no-manual --as-cran
  - Or `devtools::check()` (requires development deps)

## Big-picture architecture

- This is an R package (top-level `DESCRIPTION`, `R/`, `src/`, `man/`, `vignettes/`).
- Major parts:
  - R/ — most R functions, grouped by feature (e.g., `boxGrob_*`, `pr*` helpers in `boxGrobs_private_helpers.R`).
  - src/ — C++ parts via Rcpp; compilation is controlled by `src/Makevars` and `src/Makevars.win`.
  - tests/testthat/ — unit tests; tests assume package-mode (see below).
  - inst/examples/ — runnable examples referenced from roxygen docs.

## Testing patterns and important conventions

- Always run tests in _package mode_ (load the package first). File-mode (calling `testthat::test_file()` without `devtools::load_all()`) does not load the package and may require `source()` calls in tests; that is discouraged.
  - Example of how the test suite is run in CI and by maintainers: `Rscript -e 'devtools::load_all(); testthat::test_dir("tests/testthat", reporter = "summary")'`.
- `tests/testthat/helper-*.R` files are _automatically sourced in package mode_; do not `source()` package R files from tests. If you see `source()` in tests, consider removing it and ensuring the package loads correctly in package mode.
- When adding tests, prefer informative error messages for user-facing checks. Example: tests expect messages like "The .subelement 'no/such' was not found" (see `tests/testthat/test-spread-move-deep-path.R`).
- Visual / heavy tests:
  - Visual tests produce artifacts (pdf/html). These are excluded from the build by `.Rbuildignore` and `.gitignore`. Avoid committing generated visual outputs unless necessary.

## Coding style & conventions

### General design principles

- Prefer short, focused functions and keep complex logic in small private helpers (use `pr` prefix for helpers).
- Follow SOLID principles where practical: single responsibility, explicit dependencies, and clear interfaces.
- Avoid duplicating logic — extract common behavior into shared helpers and utility functions (DRY).
- Minimize side effects; prefer pure functions returning values when possible. If state changes are required, keep them localized and well-tested.
- Every new helper should have a unit test and concise roxygen documentation; include small runnable examples where helpful.
- Keep functions easy to reason about — short, well-named, and focused on one task.

### Function organization and naming

- **Internal helpers use `pr` prefix** ("private"): `prComputeFanCenterBus()`, `prMakeStems()`, `prGetBoxAxisDefaults()`
- **Break down complex logic** into small, focused helper functions rather than long monolithic functions
- **Name helpers descriptively** using verb prefixes:
  - `prCompute*` — calculate values/coordinates
  - `prMake*` — create grobs or complex objects
  - `prGet*` — retrieve or extract values
  - `prCalculate*` — arithmetic/geometric calculations
  - `prValidate*` / `prCheck*` — validation logic
- **Group related helpers** in the same file (e.g., `boxGrobs_connect_pr_helpers.R` for shared connector helpers)

### Strategy pattern for connectors

- Connectors use **S3 dispatch via strategy pattern** (see `R/boxGrobs_connect.R` and `R/boxGrobs_connect_strategies.R`)
- Main dispatcher: `connectGrob()` → `prGetConnectorStrategy()` → `prCalculateConnector()` method
- Each connector type has its own strategy file: `boxGrobs_connect_pr_many_to_one_fan_in_center_boxes.R`, etc.
- Keep strategy implementation files focused on one connector type's logic

### Consistency principles

- **Symmetry**: Many-to-one and one-to-many implementations should mirror each other (e.g., bus extent calculation should be identical)
- **Reuse**: Extract common logic to shared helpers in `boxGrobs_connect_pr_helpers.R` or `boxGrobs_private_helpers.R`
- **Single responsibility**: Each helper should do one thing well (compute, validate, or create)

### Grid graphics conventions

- Use `coords()` to extract box positions, then `convertX()`/`convertY()` to get NPC values
- Bus/stem calculations should use **actual box positions**, not evenly-spaced slots
- Document whether a helper works in NPC units or grid units

## Project-specific helper functions & patterns to know

- Deep-path helpers (`get_list_element_by_path()` / `set_list_element_by_path()`) are in `R/boxGrobs_private_helpers.R`. They:
  - Accept character or numeric indices (e.g. `c("grp","sub",1)` or `list("grp","sub",1)`).
  - Return `NULL` for invalid/non-atomic path segments (e.g. a `grid::unit()` object).
- Box functions are namespaced with `box`/`boxGrob` prefixes — search for `boxGrobs_*`, `boxGrob_*` for related code.
- Connector helpers follow strategy pattern — see `boxGrobs_connect*.R` files
- Examples: several functions have `@example inst/examples/...` references — keep these runnable and small.

## Build & compilation

- The package links to Rcpp (see `LinkingTo: Rcpp` in `DESCRIPTION`); C++ headers are in `inst/include/` and `src/Makevars*` set compile flags.
- To test compiled code locally: `devtools::load_all()` will compile as needed; for release-grade checks use `R CMD build` + `R CMD check`.

## Files to ignore / `.Rbuildignore` notes

- We exclude generated visual test artifacts and large test fixture files to prevent build or CRAN rejections.
- Current additions (already committed):
  - `^tests/visual_tests$`, `^tests/visual_tests/.*`, `^tests/testthat/.*\.pdf$`
  - `^inst/extdata/.*\.html$`, `^inst/extdata/.*\.css$`

If you add new generated artifacts in `inst/` or `tests/`, add appropriate patterns to `.Rbuildignore`.

## How to debug failing tests

- Reproduce failures in package mode with `devtools::load_all()` then `testthat::test_file()` or `test_dir()`.
- Inspect the failing test file in `tests/testthat` and the referenced helpers in `R/` (e.g., `R/boxGrobs_private_helpers.R`).
- For C++ issues, run `R CMD build` and then `R CMD check` to get platform-specific compiler warnings/errors.

## Pull request / code-change guidance for AI agents

- Avoid making tests rely on `source()` of package files — instead ensure the package loads correctly in package mode.
- Keep changes minimal and local to the failing area, add tests that exercise new behavior
  and ensure no global state leakage (use `withr::local_*` if you must manipulate state).
- Add or update `.Rbuildignore` for any generated artifacts created by new tests.

## Contact / maintainers

- Package maintainer: Max Gordon <max@gforge.se>

---

If anything here is missing or unclear, please point out specific areas (commands, file locations, or conventions) you want expanded and I will iterate the doc.
