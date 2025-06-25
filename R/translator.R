#' @title Create a Translator Object for Factor Encoding
#'
#' @name translator
#'
#' @description
#' Creates a translator object that stores the levels of specified factor columns
#' from a data frame. This object can be used to encode and decode factors to integers.
#'
#' @param df A data frame containing the factor columns to be translated.
#' @param cols A character vector of column names in \code{df} to be translated.
#'             These columns must be factors.
#'
#' @return A named list of levels for each specified column, with class \code{"translator"}.
#'
#' @examples
#' df <- data.frame(
#'   type = factor(c("A", "B", "A")),
#'   geno = factor(c("X", "Y", "X"))
#' )
#' translator <- create_translator(df, c("type", "geno"))
#'
#' @keywords internal
create_translator <- function(df, cols) {
  translator <- list()

  for (colname in cols) {
    if (!colname %in% colnames(df)) {
      stop(paste("Column", colname, "not found in data frame"))
    }
    if (!is.factor(df[[colname]])) {
      stop(paste("Column", colname, "is not a factor"))
    }
    translator[[colname]] <- levels(df[[colname]])
  }

  class(translator) <- "translator"
  translator
}

#' @title Encode Factor Columns Using a Translator
#'
#' @name translator
#'
#' @description
#' Converts factor columns in a data frame to 0-based integers using the levels
#' stored in a translator object.
#'
#' @param translator A translator object created with \code{create_translator}.
#' @param df A data frame containing the same columns used to create the translator.
#'
#' @return A copy of the input data frame with specified factor columns encoded as integers.
#'
#' @keywords internal
encode <- function(translator, df) {
  df_encoded <- df

  for (colname in names(translator)) {
    levels <- translator[[colname]]
    df_encoded[[colname]] <- match(df[[colname]], levels) - 1
    df_encoded[[colname]] <- as.character(df_encoded[[colname]])
  }

  df_encoded
}

#' @title Decode Integer Columns to Factor Levels Using a Translator
#'
#' @name translator
#'
#' @description
#' Converts integer-encoded columns back to factors using the levels stored in
#' a translator object.
#'
#' @param translator A translator object created with \code{create_translator}.
#' @param df_encoded A data frame containing integer-encoded columns.
#'
#' @return A copy of the input data frame with specified columns converted back to factors.
#'
#' @keywords internal
decode <- function(translator, df_encoded) {
  df_decoded <- df_encoded

  for (colname in names(translator)) {
    levels <- translator[[colname]]
    codes <- df_encoded[[colname]] + 1
    df_decoded[[colname]] <- factor(levels[codes], levels = levels)
  }

  df_decoded
}

#' @title Decode Encoded Factor Labels in a Text File
#'
#' @name translator
#'
#' @description
#' Parses a text file and replaces encoded factor labels (e.g., \code{type0}, \code{geno1})
#' with their original levels using a translator object.
#'
#' @param output_path Path to the text file to decode. The file is modified in-place.
#' @param translator A translator object created with \code{create_translator}, where the names
#'                   correspond to the encoded labels to be decoded (e.g., \code{"type"}, \code{"geno"}).
#'
#' @return The new output_path
#'
#' @keywords internal
decode_file_with_translator <- function(output_path, translator) {
  lines <- readLines(output_path)
  decoded_lines <- character(length(lines))

  for (i in seq_along(lines)) {
    line <- lines[i]

    if (startsWith(line, "eff_")) {
      for (colname in names(translator)) {
        pattern <- paste0(colname, "(\\d+)")

        matches <- gregexpr(pattern, line, perl = TRUE)[[1]]

        if (matches[1] != -1) {
          matched_substrings <- regmatches(
            line,
            gregexpr(pattern, line, perl = TRUE)
          )[[1]]

          for (m in matched_substrings) {
            num_str <- sub(paste0("^", colname), "", m)
            num <- as.integer(num_str)

            levels_vec <- translator[[colname]]

            if (is.na(num) || num + 1 > length(levels_vec) || num < 0) {
              warning(paste("Invalid index for", colname, ":", num))
              next # skip replacement
            }

            replacement <- paste0(colname, "_", levels_vec[num + 1])

            # Replace this exact matched substring in line with decoded label
            line <- sub(m, replacement, line, fixed = TRUE)
          }
        }
      }
    }

    decoded_lines[i] <- line
  }

  new_output_path <- file.path(
    dirname(output_path),
    paste0(
      sub("\\.out$", "", basename(output_path)),
      "_decoded.out"
    )
  )
  writeLines(decoded_lines, new_output_path)
  return(new_output_path)
}
