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
- `man/`:
- `reports/`:

<br>

## State of the project

- `R/lifelihood.R` is the main R script that runs the project. It works well and can be tested with `R/tests/test_lifelihood.R`.
   - This part needs to be changed:
   ```R
   #' @examples
   #'run_lifelihood_console(infile="100%mort_Pierrick211genoparinteraction.txt", ntr=10, To=50, Tf=1, seed2=34)
   ```

- Currently developing the `create_input_txt.R` script that will transform a dataframe into a `.txt` file that can be read by the pascal code.