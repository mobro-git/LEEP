
#' complete_implicit_na
#' fill in implicit NAs so differences can be calculated from unreported variables that are implicitly 0s
#' @param df
#'
#' @return

complete_implicit_na = function(df) {

  # model-scenario combinations to cross reference. only add back implicit missing data for model-scenario combinations submitted
  submitted = unique(df[c("model","scenario")])

  complete_all = df %>%
    group_by(model,unit,year,datasrc) %>%
    complete(scenario, variable, region)

  complete_zeros = complete_all %>%
    mutate(value = case_when(
      is.na(value) ~ 0,
      TRUE~value))

  # joins so that zeros are only kept for submitted model-scenario combinations
  complete_join = inner_join(submitted, complete_zeros, by = c("model","scenario"))

}

#' make_clean_data()
#' manual changes to data
#' @param df
#'
#' @return

make_clean_data = function(df) {

  df = clean_data

}

