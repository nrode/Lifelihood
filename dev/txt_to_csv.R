txt_to_csv <- function(txt_path, csv_path = NULL) {
  txt_file <- readLines(txt_path)

  # get number of covariates and their names
  model_tag_index <- which(grepl("****modele******", txt_file, fixed = TRUE))
  line_with_cov <- txt_file[model_tag_index - 2]
  covariates <- strsplit(line_with_cov, "\\s+")[[1]]

  # isolate the data part of the input file
  data_tag_index <- which(grepl("*******data*********", txt_file, fixed = TRUE))
  data <- txt_file[(data_tag_index + 1):length(txt_file)]


  formatted_rows <- lapply(
    data,
    FUN = function(line) {
      line_to_row(line, covariates)
    }
  )

  if (!is.null(csv_path)) {
    csv_path <- paste(sub("\\.txt$", "", txt_path), ".out")
  }
  write.csv(formatted_rows)
  return(formatted_rows)
}

line_to_row <- function(line, covariates) {
  splitted_line <- strsplit(line, "\\s+")[[1]]

  row <- c()
  n_covariates <- length(covariates)
  for (i in 1:n_covariates) {
    row <- c(row, splitted_line[i])
  }

  sex_index <- n_covariates + 1
  sex_start <- sex_index + 1
  sex_end <- sex_start + 1
  sex_value <- sex_end + 1
  row <- c(
    row,
    splitted_line[sex_start],
    splitted_line[sex_end],
    splitted_line[sex_value]
  )

  mat_index <- sex_value + 1
  mat_start <- mat_index + 1
  mat_end <- mat_start + 1
  row <- c(
    row,
    splitted_line[mat_start],
    splitted_line[mat_end]
  )

  for (i in seq(mat_end, length(splitted_line))) {
    if (splitted_line[i] == "mor") {
      row <- c(row, splitted_line[i + 1], splitted_line[i + 2])
      return(row)
    } else if (splitted_line[i] == "pon") {
      row <- c(row, splitted_line[i + 1], splitted_line[i + 2], splitted_line[i + 3])
    }
  }

  return(row)
}


txt_to_csv("data/raw_data/DataLenski/DataLenski_gam_gam_gam__Rep1.txt")
