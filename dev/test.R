parse_row <- function(row, covariate_names) {
  elements <- strsplit(row, " ")[[1]]
  covariates <- elements[1:length(covariate_names)]
  sex <- elements[(length(covariate_names) + 2):(length(covariate_names) + 4)]
  mat <- elements[(length(covariate_names) + 6):(length(covariate_names) + 8)]

  pon_indices <- grep("^pon$", elements)
  mor_indices <- grep("^mor$", elements)

  if (length(pon_indices) > 0) {
    pon_data <- lapply(pon_indices, function(idx) elements[(idx + 1):(idx + 3)])
  } else {
    pon_data <- c(NA, NA, NA)
  }

  mor_data <- elements[(mor_indices + 1):(mor_indices + 2)]

  list(
    covariates = covariates,
    sex_start = sex[1],
    sex_end = sex[2],
    sex = sex[3],
    mat_start = mat[1],
    mat_end = mat[2],
    mat = mat[3],
    pon = pon_data,
    mor = mor_data
  )
}

create_row <- function(parsed_row) {
  covariates <- parsed_row$covariates
  sex <- parsed_row$sex
  sex_start <- parsed_row$sex_start
  sex_end <- parsed_row$sex_end
  mat <- parsed_row$mat
  mat_start <- parsed_row$mat_start
  mat_end <- parsed_row$mat_end
  pon <- parsed_row$pon
  mor <- parsed_row$mor

  if (length(pon) > 0) {
    pon_corrected <- lapply(pon, function(p) {
      if (length(p) != 3) {
        return(c(p, rep(NA, 3 - length(p)))) # Pad with NAs if fewer than 3 elements
      }
      return(p)
    })
    padded_pon <- do.call(rbind, pon_corrected) # Combine into a matrix

    if (nrow(padded_pon) < max_clutches) {
      padded_pon <- rbind(padded_pon, matrix(NA, nrow = max_clutches - nrow(padded_pon), ncol = 3))
    }
  } else {
    padded_pon <- matrix(NA, nrow = max_clutches, ncol = 3)
  }

  c(covariates, sex_start, sex_end, sex, mat_start, mat_end, mat, as.vector(t(padded_pon)), mor)
}

txt_to_csv <- function(txt_path) {
  txt_file <- readLines(txt_path)
  model_tag_index <- which(grepl("****modele******", txt_file, fixed = TRUE))
  line_with_cov <- txt_file[model_tag_index - 2]
  covariate_names <- strsplit(line_with_cov, "\\s+")[[1]]
  data_tag_index <- which(grepl("*******data*********", txt_file, fixed = TRUE))
  data <- txt_file[(data_tag_index + 1):length(txt_file)]


  parsed_data <- lapply(data, parse_row, covariate_names)
  max_clutches <- max(sapply(parsed_data, function(row) length(row$pon)))

  print(paste("Found", length(covariate_names), "covariates"))
  print(paste("Highest clutch size:", max_clutches))

  rows <- lapply(parsed_data, create_row)
  final_data <- do.call(rbind, rows)

  column_names <- c(
    covariate_names,
    "sex_start", "sex_end", "sex",
    "mat_start", "mat_end", "mat",
    unlist(lapply(1:max_clutches, function(i) paste0(c("pon_start_", "pon_end_", "pon_"), i))),
    "mor_start", "mor_end"
  )

  colnames(final_data) <- column_names
  final_dataframe <- as.data.frame(final_data)
  write.csv(final_dataframe, sub("\\.txt$", ".csv", txt_path), row.names = FALSE)
}

txt_path <- "data/raw_data/DataLenski/DataLenski_gam_gam_gam__Rep1.txt"
txt_to_csv(txt_path)
txt_path <- "data/raw_data/DataPierrick/100%mort_Pierrick211genoparinteraction.txt"
txt_to_csv(txt_path)
