get_visits <- function(df_sims, lifelihoodData) {
  clutch_start_and_end <- lifelihoodData$clutchs[
    seq_along(lifelihoodData$clutchs) %% 3 != 0
  ]

  visits <- df_sims |>
    mutate(lifelihoodData$df[lifelihoodData$block]) |>
    mutate(id = row_number()) |>
    select(
      block = lifelihoodData$block,
      id,
      lifelihoodData$sex_start,
      lifelihoodData$sex_end,
      lifelihoodData$maturity_start,
      lifelihoodData$maturity_end,
      clutch_start_and_end
    ) |>
    pivot_longer(-c(id, block), values_to = "visit") |>
    filter(!is.na(visit)) |>
    filter(visit != lifelihoodData$right_censoring_date) |>
    distinct(block, visit) |>
    arrange(block, visit)

  return(visits)
}

vsi <- 0.1
visits <- 0:200000 * vsi

#Function to retrieve the interval in which the events could be observed

bornesinf <- function(visit, data) {
  ifelse(
    visit[which.min(abs(visit - as.numeric(data)))] <= as.numeric(data),
    visit[which.min(abs(visit - as.numeric(data)))],
    visit[which.min(abs(visit - as.numeric(data))) - 1]
  )
}
bornessup <- function(visit, data) {
  ifelse(
    visit[which.min(abs(visit - as.numeric(data)))] >= as.numeric(data),
    visit[which.min(abs(visit - as.numeric(data)))],
    visit[which.min(abs(visit - as.numeric(data))) + 1]
  )
}
