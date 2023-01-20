#' # library(corrplot)
#' # library(tidyverse)
#' # library(tibble)
#'
#' #' Read a csv file with empty cells (NA in the data frame) and autofill all empty cells with the non-empty cells above
#' #'
#' #' @param filepath to a csv file
#' #'
#' #' @return requirements template tibble
#' corr_plot <- function(filepath) {
#'   df <- readr::read_csv(filepath)
#'   head(df)
#'   ndf <- select(df[df$year == 2025,], c('variable'))
#'   head(ndf)
#'   M<-cor(ndf[0], as.numeric(ndf[1]))
#'   head(round(M,2))
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
