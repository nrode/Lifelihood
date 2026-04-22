#' @keywords internal
get_visits <- function(lifelihoodData) {
  block_col <- lifelihoodData$block
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
  if (is.null(visits)) {
    visits <- get_visits(lifelihoodData)
  }
  if (!all(c(block_col, "visit") %in% names(visits))) {
    stop("`visits` must contain columns `", block_col, "` and `visit`.")
  }

  # For each age of event in simulated df, get the block and the
  # last visits before and after observing the event
  simul_df <- simul_df |>
    group_by(block) |>
    group_modify(
      ~ {
        v <- visits |> filter(block == .y$block) |> pull(visit)
        .x |>
          mutate(
            visit_id = findInterval(!!as.symbol(event), v),
            !!paste0(event, "_start") := v[visit_id],
            !!paste0(event, "_end") := v[visit_id + 1]
          )
      }
    ) |>
    ungroup()

  return(simul_df)
}
