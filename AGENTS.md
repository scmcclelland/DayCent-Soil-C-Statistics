# Repository Guidelines

## Project Structure & Module Organization
This is an R project for processing DayCent soil carbon model outputs.
- `r/` contains the main R scripts (`process-output.R`) and reusable helpers (`process-output-functions.R`).
- `bash/` contains shell entrypoints for running batches (`process-model-output.sh`).
- `test-data/` holds example inputs/outputs used by the scripts (e.g., `test-data/model-output`, `test-data/uncertainty-input`).
- `DayCent-Soil-C-Statistics.Rproj` enables RStudio project settings.

## Build, Test, and Development Commands
Run scripts via Rscript or the provided bash wrapper.
- `Rscript r/process-output.R <run> <final_year> <scenario>` processes a single run (e.g., `Rscript r/process-output.R dec-23 2036 conv`).
- `bash/process-model-output.sh` runs multiple scenarios for a given run selection (update the file if you want to change the batch).

## Coding Style & Naming Conventions
- Language: R with `data.table`, `sf`, and `terra`.
- Follow existing style in `r/` (snake_case variables, dot-prefixed function args like `.dt`, minimal inline comments).
- Keep scripts runnable from the repo root; use relative paths as in `r/process-output.R`.

## Testing Guidelines
There is no automated test suite. Validate changes by running the relevant Rscript command against a small sample in `test-data/`. If you add tests in the future, document the command and location here.

## Commit & Pull Request Guidelines
Git history shows short, descriptive messages without a strict convention. Use clear, imperative summaries (e.g., “Add SOC filter for rewild scenario”).
For PRs:
- Describe the data inputs used and how to reproduce results.
- Link related issues or tickets if applicable.
- Include notes on any changes to `test-data/` or expected output files.

## Data & Configuration Notes
- Input tarballs are expected under `test-data/model-output/<run>/`.
- Outputs are written to `test-data/uncertainty-input/` with scenario-based filenames.
- Update run selections and scenario lists in `r/process-output.R` or `bash/process-model-output.sh` as needed.
