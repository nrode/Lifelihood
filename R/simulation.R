#' @title Simulation for a single life event
#'
#' @description Internal function used to simulate one of the
#' life history event (maturity, reproduction, mortality).
#'
#' @param object Output of [`lifelihood()`]
#' @param ev A character of the event (must be one of "mortality",
#' "reproduction" or "maturity")
#' @param newdata An optional dataset used for prediction
#' @param lifelihoodData Output of [lifelihood::as_lifelihoodData()].
#' @param visits Dataframe with 2 columns: "block" (must be the same as passed
#' in [lifelihood::as_lifelihoodData()] `block` argument) and exactly "visit". For
#' each block, "visit" corresponds to the ages where the events of individuals
#' have been recorded.
#'
#' @keywords internal
simulate_event <- function(
  object,
  ev,
  newdata,
  lifelihoodData,
  use_censoring,
  visits,
  block_values = NULL
) {
  if (ev == "mortality") {
    expt_name <- "expt_death"
    shape_name <- "survival_param2"
    fam_id <- 1
    n <- 1
  } else if (ev == "reproduction") {
    expt_name <- "expt_reproduction"
    shape_name <- "reproduction_param2"
    fam_id <- 3
  } else if (ev == "maturity") {
    expt_name <- "expt_maturity"
    shape_name <- "maturity_param2"
    fam_id <- 2
    n <- 1
  }

  family <- object$lifelihoodData$dist[[fam_id]]

  expected <- prediction(
    object,
    expt_name,
    type = "response",
    newdata = newdata
  )

  # No shape parameter for exponential
  if (family != "exp") {
    shape <- prediction(
      object,
      shape_name,
      type = "response",
      newdata = newdata
    )
  } else {
    shape <- NULL
  }

  if (ev == "reproduction") {
    expt_death <- prediction(
      object,
      "expt_death",
      type = "response",
      newdata = newdata
    )

    family_mortality <- object$lifelihoodData$dist[[1]]
    if (family_mortality != "exp") {
      survival_param2 <- prediction(
        object,
        "survival_param2",
        type = "response",
        newdata = newdata
      )
    }

    ## max longevity = value of longevity so that 99% of individuals with this shape and scale parameters die before this age
    if (family_mortality == "wei") {
      scale <- expt_death / gamma(1 + 1 / survival_param2)
      long <- qweibull(0.99999999, shape = survival_param2, scale = scale)
    } else if (family_mortality == "lgn") {
      mu <- log(expt_death) - 0.5 * log(1 + survival_param2 / (expt_death^2))
      sigma <- sqrt(log(1 + survival_param2 / (expt_death^2)))
      long <- qlnorm(0.99999999, meanlog = mu, sdlog = sigma)
    } else if (family_mortality == "gam") {
      mortality_shape <- expt_death / survival_param2
      long <- qgamma(
        0.99999999,
        shape = mortality_shape,
        scale = survival_param2
      )
    } else if (family_mortality == "exp") {
      long <- qexp(0.99999999, rate = 1 / expt_death)
    }
    max_long <- max(long) # maximum predicted longevity in the dataset

    # minimum expected reproduction interval in the dataset
    min_reproduction_interval <- min(expected)

    # Maximum number of reproduction for all individual
    n <- floor(max_long / min_reproduction_interval) + 1

    # Check that n_offspring parameter is fitted
    if (!is.null(object$formula[["n_offspring"]])) {
      n_offspring <- prediction(
        object,
        "n_offspring",
        type = "response",
        newdata = newdata
      )
      simul_n_offspring <- simulate_truncPois(expected = n_offspring, n = n)
    } else {
      simul_n_offspring <- rep(NA, length(expected))
    }
  }

  if (family == "wei") {
    simul <- simulate_weibull(expected, shape, n = n)
  } else if (family == "gam") {
    simul <- simulate_gamma(expected, scale = shape, n = n)
  } else if (family == "lgn") {
    simul <- simulate_lognormal(expected, vp1 = shape, n = n)
  } else if (family == "exp") {
    simul <- simulate_exponential(expected, n = n)
  } else {
    stop(sprintf("Unknown family '%s' for event '%s'.", family, ev))
  }

  # transpose the matrix to ensure consistent format in this specific event
  if (ev == "reproduction") {
    if (n > 1) {
      simul_t <- t(simul)
      simul_n_offspring_t <- t(simul_n_offspring)
      column_names <- paste("clutch", 1:n, sep = "_")
      n_offspring_column_names <- paste("clutch_size", 1:n, sep = "_")
    } else {
      column_names <- "clutch_1"
      n_offspring_column_names <- "clutch_size_1"
      simul_t <- simul
      simul_n_offspring_t <- simul_n_offspring
    }
    simul_df <- simul_t |> as_tibble(.name_repair = "minimal")
    colnames(simul_df) <- column_names
    simul_df_n_offspring_t <- simul_n_offspring_t |>
      as_tibble(.name_repair = "minimal")
    colnames(simul_df_n_offspring_t) <- n_offspring_column_names

    simul_df_full <- bind_cols(simul_df, simul_df_n_offspring_t)
    columns_order <- as.vector(rbind(column_names, n_offspring_column_names))
    simul_df_full <- simul_df_full[, columns_order]
  } else {
    column_names <- ev
    simul_df_full <- simul |> as_tibble(.name_repair = "minimal")
    colnames(simul_df_full) <- column_names
  }

  # add blocks, if provided
  if (!is.null(lifelihoodData$block) && use_censoring && ev != "reproduction") {
    simul_df_full <- add_visit_masks(
      simul_df = simul_df_full,
      lifelihoodData = lifelihoodData,
      event = ev,
      visits = visits,
      block_values = block_values
    )
  }

  return(simul_df_full)
}

#' @keywords internal
simulate_life_history_tradeoff <- function(
  object,
  newdata,
  lifelihoodData,
  dt = 0.1
) {
  n_ind <- if (is.null(newdata)) nrow(lifelihoodData$df) else nrow(newdata)

  family_mortality <- lifelihoodData$dist[[1]]
  family_maturity <- lifelihoodData$dist[[2]]
  family_reproduction <- lifelihoodData$dist[[3]]

  expt_death <- prediction(
    object,
    "expt_death",
    type = "response",
    newdata = newdata
  )
  survival_param2 <- tryCatch(
    prediction(object, "survival_param2", type = "response", newdata = newdata),
    error = function(e) NULL
  )
  if (is.null(survival_param2)) {
    if (family_mortality == "exp") {
      survival_param2 <- rep(1, n_ind)
    } else {
      stop(
        "Could not predict `survival_param2` required for mortality simulation."
      )
    }
  }

  expt_maturity <- prediction(
    object,
    "expt_maturity",
    type = "response",
    newdata = newdata
  )
  maturity_param2 <- tryCatch(
    prediction(object, "maturity_param2", type = "response", newdata = newdata),
    error = function(e) NULL
  )
  if (is.null(maturity_param2)) {
    if (family_maturity == "exp") {
      maturity_param2 <- rep(1, n_ind)
    } else {
      stop(
        "Could not predict `maturity_param2` required for maturity simulation."
      )
    }
  }

  expt_reproduction_link <- prediction(
    object,
    "expt_reproduction",
    type = "link",
    newdata = newdata
  )
  reproduction_param2 <- tryCatch(
    prediction(
      object,
      "reproduction_param2",
      type = "response",
      newdata = newdata
    ),
    error = function(e) NULL
  )
  if (is.null(reproduction_param2)) {
    if (family_reproduction == "exp") {
      reproduction_param2 <- rep(1, n_ind)
    } else {
      stop(
        "Could not predict `reproduction_param2` required for reproduction simulation."
      )
    }
  }

  n_offspring <- if (is_parameter_fitted(object, "n_offspring")) {
    tryCatch(
      prediction(object, "n_offspring", type = "response", newdata = newdata),
      error = function(e) rep(NA_real_, n_ind)
    )
  } else {
    rep(NA_real_, n_ind)
  }

  # check if useful to have other option than 0
  d <- predict_or_default(object, "increase_death_hazard", newdata, n_ind)
  da <- predict_or_default(object, "tof_decay", newdata, n_ind)
  dn <- predict_or_default(
    object,
    "increase_death_hazard_n_offspring",
    newdata,
    n_ind
  )
  senput <- predict_or_default(object, "lin_decrease_hazard", newdata, n_ind)

  expt_reproduction_bounds <- subset(
    object$param_bounds_df,
    param == "expt_reproduction"
  )
  expt_reproduction_min <- as.numeric(expt_reproduction_bounds$min[1])
  expt_reproduction_max <- as.numeric(expt_reproduction_bounds$max[1])

  max_long <- compute_max_longevity(
    expected = expt_death,
    shape = survival_param2,
    family = family_mortality
  )

  maturity <- rep(NA_real_, n_ind)
  mortality <- rep(NA_real_, n_ind)
  ## Empty lists for the reproduction of each of the n_ind invididuals in newdata
  clutch_times <- vector("list", n_ind)
  clutch_sizes <- vector("list", n_ind)


  for (i in seq_len(n_ind)) {
    t <- 0
    alive <- TRUE
    matured <- FALSE ## Individual born immature
    last_reproduction_time <- NA_real_
    max_iter <- ceiling(max_long[i] / dt) + 1
    iter <- 0

    sex=3 ## Undefined sex by default
    #sex<-ifelse(runif(1)<sexratiomale,1,0) ## Definition of the sex of the individual (if the sex ratio is not provided in "object")
    
    # simulate life-history while individual is alive and time interval before max longevity
    # and below maximum iteration
    while (alive && t < max_long[i] && iter <= max_iter) {
      ## Update iteration
      iter <- iter + 1

      if (length(clutch_times[[i]]) > 0) { #Compute discount only if at least one reproduction event
        #print(paste("Clutch times for individual ", i))
        #print(unlist(clutch_times[[i]]))
        # When `n_offspring` isn't fitted, clutch_sizes_i may hold NAs; the
        # offspring-dependent hazard is undefined in that case, so treat as 0.
        offspring_effect <- ifelse(is.na(clutch_sizes[[i]]), 0, clutch_sizes[[i]])
        elapsed <- t - clutch_times[[i]]
        if (da[i] == 0) {
          decay <- rep(1, length(elapsed))
        } else {
          decay <- exp(-da[i] * elapsed)
        }
        ## For each reproduction, increase in hazard rate with an intercept which increase with (d[i] and dn[i] * offspring_effect-  i.e. an constant intercept that does not depend on clutch size and a slope proportional to clutch size 
        
        discount <- sum((d[i] + dn[i] * offspring_effect) * decay)
      } else {
        discount <- 0
      }

      #Reproduction-survival trade-off for females
      #Death probability over the interval [x, x+dx] with a discount traoff
      # The discount traoff  can be null, proportional to the number of clutch or number of offspring produced before x.
      #for a female: h(t)=a t^(a-1)/(mu^a)
      #for a male: h(t)=a t^(a-1)/((mu*Rmortum)^a)
      #traoff=(d+dn*nb offspring)*exp(-dat)
      #probmort=integral between x and x+dx of [traoff +h(t)]
      
      p_die <- prob_event_interval_dt(
        t = t,
        dt = dt,
        param1 = expt_death[i],
        param2 = survival_param2[i],
        family = family_mortality
      ) +
        discount * dt
      
      ## Stop simulation if individual dies in the interval between t and t+dt ( with probability p_die)
      # probability of dying between t and t+dt
      if (runif(1) < p_die) {
        mortality[i] <- t + dt / 2
        alive <- FALSE
        #print(paste("Death of individual", i))
        #print(mortality[i])
        #print(p_die)
        break ## Exit the 'while' loop
      }

      has_matured_this_interval <- FALSE ## Indicator to avoid that maturity and reproduction occur in the same interval
      if (!matured) {
        p_maturity <- prob_event_interval_dt(
          t = t,
          dt = dt,
          param1 = expt_maturity[i],
          param2 = maturity_param2[i],
          family = family_maturity
        )

        # probability of maturing between t and t+dt
        if (runif(1) < p_maturity) {
          maturity[i] <- t + dt / 2 #Maturation occured in that interval
          last_reproduction_time <- t + dt / 2 ## Time since last reproduction or maturity 
          matured <- TRUE
          has_matured_this_interval <- TRUE
          #print(paste("Maturity for individual ", i))
          #print( maturity[i])
          }
      }

      if (matured && !has_matured_this_interval) {

        t_from_maturity <- t - maturity[i]

        # Interval between reproduction events increases as time from maturity increases
        #Reproduction hazard is calculated between t and t+dt only if lastev>0 and no maturity or reproduction event occured in this interval
        expt_reproduction <- link(
          estimate = expt_reproduction_link[i] + senput[i] * t_from_maturity,
          min = expt_reproduction_min,
          max = expt_reproduction_max
        )
        #Reproduction propensity is calculated from the age of maturity or the last reproductive event (lastev)
        p_reproduction <- prob_event_interval_dt(
          t =  t - last_reproduction_time,
          dt = dt,
          param1 = expt_reproduction,
          param2 = reproduction_param2[i],
          family = family_reproduction
        )

        # probability of reproduction between t and t+dt
        if (runif(1) < p_reproduction) {
          last_reproduction_time <- t + dt / 2 ##Update last reproduction event
          clutch_times[[i]] <- c(clutch_times[[i]], t + dt / 2) ## Append simulated clutch time
          clutch_sizes[[i]] <- c(clutch_sizes[[i]], simulate_truncPois_single(n_offspring[i])) ## Append simulated clutch size
          
        }
      }

      t <- t + dt
    } ## End of while loop (individual dead or max_iteration reached)

    # individual censored if individual didn't die before maximum longevity
    if (is.na(mortality[i])) {
      mortality[i] <- lifelihoodData$right_censoring_date
    }

  }
  ## Export smulations as tibble
  sim_df <- tibble(mortality = mortality)
  sim_df$maturity <- maturity
  
  ## Compute maximum number of clutches in the datasets
  n_clutch <- max(lengths(clutch_times))
  
  if(n_clutch>0){
    clutch_time_df <- as.data.frame(do.call(rbind, lapply(clutch_times, function(x) {
      length(x) <- n_clutch
      x
    })))
    names(clutch_time_df) <- paste0("clutch_", seq_len(ncol(clutch_time_df)))
    
    clutch_size_df <- as.data.frame(do.call(rbind, lapply(clutch_sizes, function(x) {
      length(x) <- n_clutch
      x
    })))
    names(clutch_size_df) <- paste0("clutch_size_", seq_len(ncol(clutch_size_df)))
    
    sim_df <- bind_cols(sim_df, clutch_time_df, clutch_size_df)
    #print("original simulated data with reproduction events:")
    #print(sim_df)
  }else{
    #print("original simulated data without reproduction events:")
    #print(sim_df)
    sim_df$clutch_1 <- rep(NA_real_, n_ind)
    sim_df$clutch_size_1 <- rep(NA_real_, n_ind)
  }

  
  print("original simulated data:")
  print(sim_df)
  return(sim_df)
}

#' @title Simulate outcomes from a fitted lifelihood model
#'
#' @description This function generates simulated data from a fitted lifelihood model,
#' for one or several life history events. By default, all fitted events are simulated.
#'
#' @param object A fitted `lifelihoodResults` object created either with [`lifelihood()`] or
#'   [`create_simulation_input()`].
#' @param event Character string specifying the event(s) to simulate.
#'   Must be one of `"mortality"`, `"maturity"`, or `"all"` (event=`"reproduction"` is equivalent to `"all"` as maturity and mortality are needed to simulate reproduction events).
#'   Default is `"all"`, which simulates all fitted events.
#' @param newdata Optional `data.frame` providing covariate values for prediction.
#'   If `NULL`, the original model data are used.
#' @param use_censoring Whether to retrieve censoring time intervals for scalar
#' events (`maturity`, `mortality`). For example, returns `mortality_start` and
#' `mortality_end` instead of only `mortality`. If `newdata` is provided and
#' censoring is enabled, `newdata` must include the block column.
#' When `use_censoring = TRUE`, `visits` must be provided explicitly.
#' Use [get_visits()] to derive visit data from the fitted data, or pass a
#' custom visit data frame.
#' @param visits Optional data frame with 2 columns: one column with the same
#' name as the `block` argument passed to [lifelihood::as_lifelihoodData()] and
#' one column named exactly `visit`. For each block, `visit` corresponds to the
#' ages where the events of individuals have been recorded. Required when
#' `use_censoring = TRUE`.
#' @param seed Optional integer. If provided, sets the random seed for reproducibility.
#'
#' @return A list of `data.frame` with one column per simulated event.
#'   Each column contains simulated values for that event.
#'
#' @export
simulate_life_history <- function(
  object,
  event = c("all", "mortality", "reproduction", "maturity"),
  newdata = NULL,
  use_censoring = FALSE,
  visits = NULL,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  check_lifelihoodResults(object)

  if (isTRUE(object$group_by_group)) {
    stop(
      "simulate_life_history() is not supported for group_by_group results. ",
      "Use coef() to access per-group estimates."
    )
  }

  event <- match.arg(
    event,
    c("all", "mortality", "reproduction", "maturity"),
    several.ok = TRUE
  )

  # Simulate all events if user selected either "all" or "reproduction".
  # It's required for reproduction since we need info about maturity and death.
  events <- if ("all" %in% event || "reproduction" %in% event) {
    c("maturity", "reproduction", "mortality")
  } else {
    event
  }

  # We need maturity and death parameters to simulate reproduction
  if ("reproduction" %in% events) {
    fitted_params <- object$formula |> names()
    if (
      !("expt_maturity" %in% fitted_params && "expt_death" %in% fitted_params)
    ) {
      stop(
        "To simulate reproduction, the fitted object must also include both ",
        "maturity and mortality models."
      )
    }
  }

  lifelihoodData <- object$lifelihoodData
  block_values <- NULL
  if (use_censoring) {
    if (is.null(lifelihoodData$block)) {
      stop(
        "`use_censoring = TRUE` requires `object$lifelihoodData$block`. ",
        "Set `block` when creating the lifelihoodData object."
      )
    }
    if (is.null(visits)) {
      stop(
        "`visits` cannot be NULL when `use_censoring = TRUE`. ",
        "Use `get_visits(object$lifelihoodData)` to derive visit masks from ",
        "the fitted data, or provide a custom visits data frame."
      )
    }

    block_col <- lifelihoodData$block
    if (is.null(newdata)) {
      block_values <- lifelihoodData$df[[block_col]]
    } else {
      if (!(block_col %in% names(newdata))) {
        stop(
          "`use_censoring = TRUE` with `newdata` requires a `",
          block_col,
          "` column."
        )
      }
      block_values <- newdata[[block_col]]
    }
  }

  use_tradeoff_path <- "reproduction" %in%
    events &&
    uses_tradeoff_simulation(object)

  if (use_tradeoff_path) {
    # Simulation with tradeoffs
    df_sims_up_na <- simulate_life_history_tradeoff(
      object,
      newdata = newdata,
      lifelihoodData = lifelihoodData
    )
    if (!is.null(lifelihoodData$block) && use_censoring) {
      df_sims_up_na <- df_sims_up_na |>
        add_visit_masks(
          lifelihoodData = lifelihoodData,
          event = "maturity",
          visits = visits,
          block_values = block_values
        ) |>
        add_visit_masks(
          lifelihoodData = lifelihoodData,
          event = "mortality",
          visits = visits,
          block_values = block_values
        )
    }
  } else {
    # Simulation without tradeoffs
    df_sims <- NULL
    for (ev in events) {
      sim <- simulate_event(
        object,
        ev,
        newdata,
        lifelihoodData,
        use_censoring,
        visits,
        block_values
      )
      df_sims <- bind_cols(sim, df_sims)
    }
  

  if ("maturity" %in% events && "mortality" %in% events) {
    # Convert to NA maturity that occurred after simulated death
    df_sims <- df_sims |>
      mutate(maturity = ifelse(maturity > mortality, NA, maturity))
  }

  if ("reproduction" %in% events) {
    ## Compute actual age for each reproduction event (=age at maturity + sum over all previous reproduction events)
    df_sims_up <- df_sims |>
      mutate(clutch_1 = clutch_1 + maturity) |>
      relocate(maturity, .before = clutch_1)

    clutch_cols <- grep("^clutch_[0-9]+$", names(df_sims_up), value = TRUE)

    # Sum duration between clutches to get age at which clutches are produced, only if more than one clutch
    if(ncol(df_sims_up[clutch_cols])>1){
      df_sims_up[clutch_cols] <- t(apply(df_sims_up[clutch_cols], 1, cumsum))
    }
    
    # Convert to NA clutches that occurred after simulated death
    df_sims_up_na <- df_sims_up |>
      mutate(across(matches("^clutch_[0-9]+$"), ~ ifelse(. > mortality, NA, .)))

    for (clutch_col in clutch_cols) {
      ## Colnames of column with information about clutch sizes
      n_offspring_col <- sub("^clutch_", "clutch_size_", clutch_col)
      ## Check whether some reproduction time are NA because they occured after death
      if (any(is.na(df_sims_up_na[[clutch_col]]))) {
      #if (n_offspring_col %in% names(df_sims_up_na)) {
        df_sims_up_na[[n_offspring_col]][is.na(df_sims_up_na[[
          clutch_col
        ]])] <-
          NA ## Replace by NA the number of offspring that have been produced after death
      }
    }

    # Remove clutch columns with only NA because they all occurred
    # after simulated death
    remove_cols <- colnames(df_sims_up_na[clutch_cols])[
      colSums(is.na(df_sims_up_na[clutch_cols])) == nrow(df_sims_up_na)
    ]
    remove_cols_all <- colnames(df_sims_up_na)[c(
      which(colnames(df_sims_up_na) %in% remove_cols) + 1,
      which(colnames(df_sims_up_na) %in% remove_cols)
    )]
    df_sims_up_na <- df_sims_up_na |> select(-all_of(remove_cols_all))
  }else{
    df_sims_up_na <- df_sims 
  } ## End of reproduction events

} ## End of simulation without
  
    # Compute total number of offspring during life
    df_sims_up_na <- df_sims_up_na |>
    mutate(
      total_n_offspring = rowSums(
        across(starts_with("clutch_size_")),
        na.rm = TRUE
      )
    )  
  
  
    if (object$lifelihoodData$matclutch) {
      ## Remove maturity which is not observed when matclutch is TRUE
      print("Maturity correspond to first clutch as arguement matclutch is true in the Lifehood object provided")
      df_sims_up_na <- df_sims_up_na |>
        select(-maturity) |>
        rename(
          maturity = clutch_1,
          !!as.symbol(
            object$lifelihoodData$matclutch_size
          ) := clutch_size_1
        )
    }
    
    if ("reproduction" %in% events) {
    df <- if (is.null(newdata)) object$lifelihoodData$df else newdata

    df_sims_up_na <- df_sims_up_na |>
      # Set reproduction-related columns to NA for males
      mutate(
        across(
          c(starts_with("clutch_"), starts_with("clutch_size_")),
          ~ if_else(df[[lifelihoodData$sex]] == 1, NA, .x)
        )
      ) 
  }


  if (!use_censoring) {
    if ("maturity" %in% events) {
      if ("mortality" %in% events) {
        df_sims_up_na <- df_sims_up_na |>
          mutate(
            maturity_start = if_else(is.na(maturity), mortality, maturity), ## Censoring of maturity at the age of death if the individual did not mature before
            maturity_end = if_else(
              is.na(maturity),
              lifelihoodData$right_censoring_date,
              maturity
            )
          ) |>
          select(-maturity)
      } else {
        df_sims_up_na <- df_sims_up_na |>
          mutate(maturity_start = maturity, maturity_end = maturity) |>
          select(-maturity) |>
          relocate(maturity_start, maturity_end)
      }
    }

    if ("mortality" %in% events) {
      df_sims_up_na <- df_sims_up_na |>
        mutate(death_start = mortality, death_end = mortality) |>
        select(-mortality) |>
        relocate(death_start, death_end)
    }
    ## Rename the columns for clutches and reorder them such as we have: "death_start death_end maturity_start maturity_end clutch_start_1 clutch_end_1 clutch_size_1       "maturity"        "clutch_size_1"
    if ("reproduction" %in% events) {
      ## Vector with the names of clutches: "clutch_1", etc.
      clutch_cols <- grep("^clutch_[0-9]+$", names(df_sims_up_na), value = TRUE)
      df_sims_up_na <- df_sims_up_na |>
        mutate(across(
          all_of(clutch_cols),
          list(start = identity, end = identity),
          .names = "clutch_{.fn}_{sub('clutch_', '', .col)}"
        )) |>
        relocate(matches("^clutch_(start|end|size)_"), .after = last_col()) |>
        select(-all_of(clutch_cols))
    }
  }

  df_sims_up_na

}
#' @keywords internal
simulate_weibull <- function(expected, shape, n) {
  # expected = scale * gamma(1 + 1 / shape)
  scale <- expected / gamma(1 + 1 / shape)
  return(mapply(rweibull, shape = shape, scale = scale, n = n))
}

#' @keywords internal
simulate_gamma <- function(expected, scale, n) {
  # expected = shape * scale
  shape <- expected / scale
  return(mapply(rgamma, shape = shape, scale = scale, n = n))
}

#' @keywords internal
simulate_lognormal <- function(expected, vp1, n) {
  mu <- log(expected) - 0.5 * log(1 + vp1 / (expected^2))
  sigma <- sqrt(log(1 + vp1 / (expected^2)))
  return(mapply(rlnorm, meanlog = mu, sdlog = sigma, n = n))
}

#' @keywords internal
simulate_exponential <- function(expected, n) {
  rate <- 1 / expected
  return(sapply(rate, rexp, n = n))
}

#' @keywords internal
simulate_truncPois_single <- function(expected) {
  simulate_truncPois_draws(lambda = expected, n = 1L)[1]
}

#' @keywords internal
simulate_truncPois_draws <- function(lambda, n) {
  if (!is.finite(lambda) || lambda <= 0) {
    return(rep(NA_integer_, n))
  }

  p0 <- exp(-lambda)
  u <- runif(n)
  draws <- qpois(p0 + u * (1 - p0), lambda = lambda)
  draws <- as.integer(draws)
  draws[draws < 1L] <- 1L
  draws
}

#' @keywords internal
simulate_truncPois <- function(expected, n) {
  n <- as.integer(n)
  if (length(n) != 1 || is.na(n) || n < 1) {
    stop("`n` must be a single positive integer.")
  }

  expected <- as.numeric(expected)
  if (length(expected) == 0) {
    return(integer(0))
  }
  draws <- lapply(
    expected,
    function(lambda) simulate_truncPois_draws(lambda = lambda, n = n)
  )

  if (n == 1L) {
    return(vapply(draws, function(x) x[1], integer(1)))
  }

  out <- do.call(cbind, draws)
  storage.mode(out) <- "integer"
  out
}
