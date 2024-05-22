# Lifelihood

<br>

## Description

TODO: write a short description of the project and what it does.

<br>

## Project Structure Overview

- `data/`: contains the data to test code
- `R/`: contains source R code
- `tests/`: contains tests for the R code
- `src/`: contains both compiled and source pascal (Lazarus and Delphi) code
- `man/`: contains the documentation
- `reports/`:

<br>

## State of the project

- `R/Lifelihood.R` is the main R script that runs the project. It works well and can be tested with `tests/test_lifelihood.R`.

- Currently developing the `create_input_txt.R` script that will transform a dataframe into a `.txt` file that can be read by the pascal code.