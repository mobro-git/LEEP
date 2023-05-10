##### Setup ----------------
library(targets)
library(tarchetypes)

source("packages.R")

devtools::load_all(".") # load all function definitions in /R

# Set target-specific options such as packages.
tar_option_set(
  packages = c("dplyr","readr","tidyverse","datasets"), # packages to make available to targets
  imports = "LEEP" # watch changes in these functions to invalidate targets
)

# Plotmapping: plot subject and figure type table. Cannot be used by tar_map unless outside of tar_plan()

figmap_list_csv = "plot_mapping/figmap.csv"
figmap_list = read_csv(figmap_list_csv, col_types = cols()) %>% as_tibble()

# End this file with a list of target objects.
tar_plan(

  ######################################################################################### -
  ######################################################################################### -

  ##### Config -----------------
  config = list(
    scen_mapping = read_scen_mapping(scen_mapping_csv),
    template = template,
    calculated_var = all_calculated,

    # models
    models = c("AEO2020","AEO2021","AEO2022",
               "EIA_Historic","EIA_STEO","EPA-GHGI","NCS-GHGI",
               "GCAM-CGS","EPS-EI","Haiky-RFF","IPM-NRDC","MARKAL-NETL","NEMS-RHG","ReEDS-NREL","REGEN-EPRI","RIO-REPEAT", #bistline paper
               "GCAM-EMF" # emf
               ),

    models_noaeo = c("NEMS","GCAM","IPM","USREP-ReEDS","Scout"),

    models_bistline = c("GCAM-CGS","EPS-EI","Haiku-RFF","IPM-NRDC","MARKAL-NETL","NEMS-RHG","ReEDS-NREL","REGEN-EPRI","RIO-REPEAT"),

    models_emf = c("GCAM-EMF"),

    # Use models_leep for all initial plots
    models_leep = c("USREP-ReEDS", "EPS-EI", "GCAM-CGS", "GCAM-EMF", "GCAM-USA", "Haiku-RFF", "IPM-NRDC", "MARKAL-NETL", "NEMS-RHG",
                    "OP-NEMS", "REGEN-EPRI", "RIO-REPEAT", "ReEDS-NREL", "Scout-LEEP"),

    # scenarios
    main_scenarios = c("Reference","IRA"),

    hist_mod_scenarios = c("Historic","Reference","IRA"),

    # time intervals
    usa = "United States",
    ira_2035 = c(seq(2021,2035,by=1)),
    ira_2050 = c(seq(2021,2050,by=1)),
    historic = c(seq(2010,2035,by=1))
  ),

  ######################################################################################### -
  ######################################################################################### -

  ##### Template and Metadata ---------------------------------------------------

  tar_target(template_csv, "data-raw/EMF37_data_template_R2_v2.xlsx", format = "file"),
  tar_target(template, read_emf_template_xlsx(template_csv)),

  tar_target(scen_mapping_csv, "data-raw/scenario-mapping.csv", format = "file"),

  ######################################################################################### -
  ######################################################################################### -

  #### Data Files ----------------------------------------------------------------

  tar_target(data_folder, path("data-raw", "model-runs"), format = "file"),
  tar_target(data_files, dir_ls(data_folder), format = "file"),

  ####################################################################################### -
  ######################################################################################### -

  ### Data Processing -----------------------

  # _Calculated variables ----

  tar_target(ratio_var_list, "data-raw/process_data/ratio_variables.csv", format = "file"),
  ratio_var = readr::read_csv(ratio_var_list, col_types = cols()),

  tar_target(summation_var_list, "data-raw/process_data/summation_variables.csv", format = "file"),
  summation_var = readr::read_csv(summation_var_list, col_types = cols()),

  tar_target(cumulative_var_list, "data-raw/process_data/cumulative_variables.csv", format = "file"),
  cumulative_var = readr::read_csv(cumulative_var_list, col_types = cols()),

  tar_target(annual_growth_rate_var_list, "data-raw/process_data/annualgrowthrate_variables.csv", format = "file"),
  annual_growth_rate_var = readr::read_csv(annual_growth_rate_var_list, col_types = cols()),

  tar_target(per_diff_var_list, "data-raw/process_data/per_diff_variables.csv", format = "file"),
  per_diff_var = readr::read_csv(per_diff_var_list, col_types = cols()),

  tar_target(index_var_list, "data-raw/process_data/index_variables.csv", format = "file"),
  index_var = readr::read_csv(index_var_list, col_types = cols()),

  tar_target(all_calculated, list(ratio_var = ratio_var,
                                  summation_var=summation_var,
                                  cumulative_var = cumulative_var,
                                  annual_growth_rate_var = annual_growth_rate_var,
                                  per_diff_var = per_diff_var)),

  tar_target(all_calculated_var, c(
    unique(all_calculated$ratio_var$variable),
    unique(all_calculated$summation_var$variable),
    unique(all_calculated$cumulative_var$new_variable),
    unique(all_calculated$annual_growth_rate_var$new_variable),
    unique(all_calculated$per_diff_var$new_variable))
    ),

  # _Making emf_data_long ----
  data_raw = map_dfr(data_files, read_raw_data_file),

  data_min = map_dfr(data_files, read_process_minimal_from_raw),

  unique_submissions = {
    data_min %>%
      select(datasrc,model,scenario) %>%
      distinct() %>%
      write.csv("output/unique_submissions.csv")},

  data_long_read = {
    map_dfr(data_files, ~read_process_data_file(.x, config)) %>%
    # map_variable_names() %>%
    arrange_standard()},

  data_long = make_data_long(data_long_read),

  # emf_data_long but can add in transformations or filter out models/variables
  clean_data = {
    data_long %>%
      complete_implicit_na() %>%
      make_calculated_vars(ratio_var, summation_var, cumulative_var, annual_growth_rate_var, per_diff_var)},

  # indexed version of clean_data. index_var determines which variables are indexed, only these are included
  clean_data_index = index_data_long(clean_data, index_var),

  ######################################################################################### -
  ######################################################################################### -

  ### Plot Mapping CSVs --------------

  tar_map(
    values = figmap_list,
    tar_target(figmap_csv, figmap_csv_path(fig_subject, fig_type), format = "file"),
    tar_target(figmap, import_figure_csv(figmap_csv, fig_type, config))
  ),

  ######################################################################################### -
  ######################################################################################### -

  ### Figures  -------------------

  ts = create_graph("leep", "time_series", config, clean_data, figmap_leep_timeseries),
  cone = create_graph("leep", "cone_uncertainty", config, clean_data, figmap_leep_cone),
  stackbar = create_graph("leep", "stacked_bar", config, clean_data, figmap_leep_stackbar),
  diffbar = create_graph("leep", "diff_bar", config, clean_data, figmap_leep_diffbar),

  test_diffbar = create_graph("test", "diff_bar",config,clean_data,figmap_test_diffbar)

)

# view Targets pipeline in flow-chart style with dependency links
# tar_visnetwork(targets_only=TRUE)
