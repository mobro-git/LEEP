#' library(tidyverse)
#' library(tidyr)
#' library(tibble)
#'
#' #' Read a csv file with empty cells (NA in the data frame) and autofill all empty cells with the non-empty cells above
#' #'
#' #' @param filepath to a csv file
#' #'
#' #' @return requirements template tibble
#' read_autofill <- function(filepath) {
#'   df <- readr::read_csv(filepath)
#'   # tbl_df <- as_tibble(df)
#'   df <- df %>% fill(names(df), .direction = "down")
#'   return(df)
#' }
#'
#' # read_autofill <- function(filepath) {
#' #   df <- readr::read_csv(filepath)
#' #     for (j in 1:ncol(df)){
#' #       for (i in 1:nrow(df)){
#' #         if (is.na(df[i,j])){
#' #           df[i,j] = df[i - 1,j]
#' #         }
#' #       }
#' #     }
#' #   return(df)
#' # }
#'
#'
