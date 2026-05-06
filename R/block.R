#' @title Retrieve visit masks from a lifelihood data object
#'
#' @description
#' Builds the visit-mask data frame used by [simulate_life_history()] when
#' `use_censoring = TRUE`. The returned visits are inferred from the observed
#' interval bounds in the original data.
#'
#' @param lifelihoodData Output of [as_lifelihoodData()]. It must include a
#'   valid `block` column.
#'
#' @return A data frame with one column named like `lifelihoodData$block` and
#'   one column named `visit`.
#'
#' @export
get_visits <- function(lifelihoodData) {
  check_lifelihoodData(lifelihoodData)

  block_col <- lifelihoodData$block
  if (is.null(block_col) || !(block_col %in% names(lifelihoodData$df))) {
    stop("`lifelihoodData$block` must be a valid column name.")
  }

  clutch_start_and_end <- lifelihoodData$clutchs[
    seq_along(lifelihoodData$clutchs) %% 3 != 0
  ]
  selected_cols <- c(
    block_col,
    lifelihoodData$sex_start,
    lifelihoodData$sex_end,
    lifelihoodData$maturity_start,
    lifelihoodData$maturity_end,
    clutch_start_and_end
  )

  visits <- lifelihoodData$df |>
    mutate(id = row_number()) |>
    select(all_of(c("id", selected_cols))) |>
    pivot_longer(-all_of(c("id", block_col)), values_to = "visit") |>
    filter(!is.na(visit)) |>
    filter(visit != lifelihoodData$right_censoring_date) |>
    distinct(!!as.symbol(block_col), visit) |>
    arrange(!!as.symbol(block_col), visit)

  return(visits)
}

#' @keywords internal
add_visit_masks <- function(
  simul_df,
  lifelihoodData,
  event,
  visits,
  block_values = NULL
) {
  block_col <- lifelihoodData$block
  if (is.null(block_col) || !(block_col %in% names(lifelihoodData$df))) {
    stop("`lifelihoodData$block` must be a valid column name.")
  }
  if (!(event %in% names(simul_df))) {
    stop("`event` must match a column name in `simul_df`.")
  }
  if (is.null(block_values)) {
    block_values <- lifelihoodData$df[[block_col]]
  }
  if (length(block_values) != nrow(simul_df)) {
    stop("Length of `block_values` must match number of rows in `simul_df`.")
  }

  simul_df[[block_col]] <- block_values

  # for each block, we get all visit dates
  if (!all(c(block_col, "visit") %in% names(visits))) {
    stop("`visits` must contain columns `", block_col, "` and `visit`.")
  }

  block_keys <- as.character(block_values)
  visit_block_keys <- as.character(visits[[block_col]])
  missing_blocks <- setdiff(unique(block_keys), unique(visit_block_keys))
  if (length(missing_blocks) > 0) {
    stop("`visits` must include visit times for every simulated block.")
  }

  visits_by_block <- split(visits$visit, visit_block_keys)
  visits_by_block <- lapply(visits_by_block, sort)

  # For each age of event in simulated df, get the block and the
  # last visits before and after observing the event
  simul_df <- simul_df |>
    group_by(!!as.symbol(block_col)) |>
    group_modify(
      ~ {
        block_key <- as.character(.y[[block_col]][[1]])
        v <- visits_by_block[[block_key]]
        visit_id <- findInterval(.x[[event]], v)
        visit_start <- rep(NA_real_, length(visit_id))
        visit_end <- rep(NA_real_, length(visit_id))

        has_start <- visit_id > 0
        visit_start[has_start] <- v[visit_id[has_start]]

        end_id <- visit_id + 1
        has_end <- end_id <= length(v)
        visit_end[has_end] <- v[end_id[has_end]]

        .x[[paste0(event, "_start")]] <- visit_start
        .x[[paste0(event, "_end")]] <- visit_end
        .x
      }
    ) |>
    ungroup() |>
    select(-all_of(block_col))

  return(simul_df)
}
