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
compute_visit_bounds <- function(ages, v) {
  visit_id <- findInterval(ages, v)
  visit_start <- rep(NA_real_, length(ages))
  visit_end <- rep(NA_real_, length(ages))

  has_start <- !is.na(visit_id) & visit_id > 0
  visit_start[has_start] <- v[visit_id[has_start]]

  end_id <- visit_id + 1
  has_end <- !is.na(end_id) & end_id <= length(v)
  visit_end[has_end] <- v[end_id[has_end]]

  list(start = visit_start, end = visit_end)
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
  if (event != "reproduction" && !(event %in% names(simul_df))) {
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

  # Reproduction has several paired time and size columns, so its visit masks
  # must be calculated in long form rather than from one scalar event column.
  if (event == "reproduction") {
    clutch_cols <- grep(
      "^clutch_[0-9]+$",
      names(simul_df),
      value = TRUE
    )
    clutch_cols <- clutch_cols[order(as.integer(sub(
      "^clutch_",
      "",
      clutch_cols
    )))]

    clutch_size_cols <- sub("^clutch_", "clutch_size_", clutch_cols)
    existing_size_cols <- intersect(clutch_size_cols, names(simul_df))

    # Preserve individual identity while clutch slots are reshaped and merged.
    simul_df$.visit_mask_row <- seq_len(nrow(simul_df))

    if (length(clutch_cols) > 0) {
      long_cols <- c(clutch_cols, existing_size_cols)
      clutch_long <- simul_df |>
        select(all_of(c(".visit_mask_row", block_col)), all_of(long_cols)) |>
        pivot_longer(
          all_of(long_cols),
          names_to = c(".value", ".clutch_slot"),
          names_pattern = "^(clutch|clutch_size)_([0-9]+)$"
        ) |>
        mutate(.clutch_slot = as.integer(.clutch_slot)) |>
        rename(.clutch = clutch)

      # `clutch_size` is absent only when no size columns exist for these clutches
      if ("clutch_size" %in% names(clutch_long)) {
        clutch_long <- clutch_long |> rename(.clutch_size = clutch_size)
      } else {
        clutch_long$.clutch_size <- NA_real_
      }

      clutch_long <- clutch_long |>
        filter(!is.na(.clutch))
    } else {
      # Every simulated clutch was removed, for example because all clutches
      # occurred after mortality. An empty frame flows into the single-slot NA
      # schema below, so the stable schema lives in exactly one place.
      clutch_long <- tibble(
        .visit_mask_row = integer(0),
        .clutch = numeric(0),
        .clutch_size = numeric(0)
      )
    }

    if (nrow(clutch_long) > 0) {
      # Compute visit bounds for every observed clutch age. Bounds are assigned
      # by row index, as in the scalar path (see the per-block loop below), so a
      # single strategy covers both instead of a parallel group_modify variant.
      block_key_long <- as.character(clutch_long[[block_col]])
      clutch_long$.visit_start <- NA_real_
      clutch_long$.visit_end <- NA_real_
      for (bk in unique(block_key_long)) {
        idx <- which(block_key_long == bk)
        bounds <- compute_visit_bounds(
          clutch_long$.clutch[idx],
          visits_by_block[[bk]]
        )
        clutch_long$.visit_start[idx] <- bounds$start
        clutch_long$.visit_end[idx] <- bounds$end
      }

      # Clutches between the same visits are observationally indistinguishable:
      # retain their earliest latent age and combine their offspring counts.
      merged_clutches <- clutch_long |>
        group_by(.visit_mask_row, .visit_start, .visit_end) |>
        summarise(
          .clutch = min(.clutch),
          .clutch_size = if (all(is.na(.clutch_size))) {
            .clutch_size[[1]]
          } else {
            sum(.clutch_size, na.rm = TRUE)
          },
          .groups = "drop"
        ) |>
        arrange(.visit_mask_row, .clutch) |>
        group_by(.visit_mask_row) |>
        mutate(.clutch_slot = row_number()) |>
        ungroup()

      # Restore one row per individual and pad individuals with fewer merged
      # clutches so all reconstructed columns have a consistent length.
      max_clutches <- max(merged_clutches$.clutch_slot)
      clutch_grid <- expand_grid(
        .visit_mask_row = seq_len(nrow(simul_df)),
        .clutch_slot = seq_len(max_clutches)
      ) |>
        left_join(merged_clutches, by = c(".visit_mask_row", ".clutch_slot"))
    } else {
      max_clutches <- 1L
      clutch_grid <- tibble(
        .visit_mask_row = seq_len(nrow(simul_df)),
        .clutch_slot = 1L,
        .visit_start = NA_real_,
        .visit_end = NA_real_,
        .clutch = NA_real_,
        .clutch_size = NA_integer_
      )
    }

    # Replace the original clutch pairs with chronological age/bounds/size
    # groups while retaining every non-reproduction simulation column.
    simul_df <- simul_df |>
      select(
        -all_of(c(block_col, clutch_cols, existing_size_cols)),
        -.visit_mask_row
      )

    for (i in seq_len(max_clutches)) {
      clutch_i <- clutch_grid |> filter(.clutch_slot == i)
      simul_df[[paste0("clutch_", i)]] <- clutch_i$.clutch
      simul_df[[paste0("clutch_start_", i)]] <- clutch_i$.visit_start
      simul_df[[paste0("clutch_end_", i)]] <- clutch_i$.visit_end
      simul_df[[paste0("clutch_size_", i)]] <- clutch_i$.clutch_size
    }

    return(simul_df)
  }

  # For each age of event in simulated df, get the block and the last visits
  # before and after observing the event. Bounds are assigned by row index so
  # the input row order is preserved (grouping would reorder rows by block).
  event_ages <- simul_df[[event]]
  block_key_vec <- as.character(block_values)
  visit_start <- rep(NA_real_, nrow(simul_df))
  visit_end <- rep(NA_real_, nrow(simul_df))

  for (bk in unique(block_key_vec)) {
    idx <- which(block_key_vec == bk)
    bounds <- compute_visit_bounds(event_ages[idx], visits_by_block[[bk]])
    visit_start[idx] <- bounds$start
    visit_end[idx] <- bounds$end
  }

  simul_df[[paste0(event, "_start")]] <- visit_start
  simul_df[[paste0(event, "_end")]] <- visit_end
  simul_df <- simul_df |> select(-all_of(block_col))

  return(simul_df)
}
