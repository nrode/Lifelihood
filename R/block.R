#' @export
get_visits <- function(lifelihoodData) {
  clutch_start_and_end <- lifelihoodData$clutchs[
    seq_along(lifelihoodData$clutchs) %% 3 != 0
  ]

  visits <- lifelihoodData$df |>
    mutate(id = row_number()) |>
    select(
      lifelihoodData$block,
      id,
      lifelihoodData$sex_start,
      lifelihoodData$sex_end,
      lifelihoodData$maturity_start,
      lifelihoodData$maturity_end,
      clutch_start_and_end
    ) |>
    pivot_longer(-c(id, lifelihoodData$block), values_to = "visit") |>
    filter(!is.na(visit)) |>
    filter(visit != lifelihoodData$right_censoring_date) |>
    distinct(!!as.symbol(lifelihoodData$block), visit) |>
    arrange(lifelihoodData$block, visit)

  return(visits)
}

#' @export
add_visit_masks <- function(simul_df, lifelihoodData, event, visits) {
  simul_df <- simul_df |>
    mutate(lifelihoodData$df[lifelihoodData$block])

  # for each block, we get all visit dates
  if (is.null(visits)) {
    visits <- get_visits(lifelihoodData)
  }
  visits <- visits |>
    group_by(!!as.symbol(lifelihoodData$block)) |>
    summarise(visits = list(visit), .groups = "drop")

  # For each individual, we retrieve the closest visit
  # dates (upward and downward) in their block by merging with `visits`.
  # `simul_df` will contains upper and lower bound columns.
  simul_df <- simul_df |>
    left_join(visits, by = lifelihoodData$block) |>
    rowwise() |>
    # in order to set dynamic column names (depending on `event`), we need to
    # use the !! and := operators: https://chatgpt.com/share/6970ed2f-e458-8013-9224-289d0f4ee45e
    mutate(
      !!paste0(event, "_start") := {
        v <- visits[visits <= !!as.symbol(event)]
        if (length(v) == 0) 0.000001 else max(v)
      },
      !!paste0(event, "_end") := {
        v <- visits[visits > !!as.symbol(event)]
        if (length(v) == 0) lifelihoodData$right_censoring_date else min(v)
      }
    ) |>
    ungroup() |>
    select(-visits, -!!as.symbol(lifelihoodData$block))

  return(simul_df)
}
