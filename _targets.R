##### Setup ----------------
library(targets)
library(tarchetypes)

source("packages.R")

devtools::load_all(".") # load all function definitions in /R

# Set target-specific options such as packages.
tar_option_set(
  packages = c("dplyr","readr","tidyverse","datasets"), # packages to make available to targets
  imports = "EMF37viz" # watch changes in these functions to invalidate targets
)

# Plotmapping: plot subject and figure type table. Cannot be used by tar_map unless outside of tar_plan()

figmap_list_csv = "plot_mapping/round2/figmap.csv"
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
    models = c("ADAGE","AEO2020","AEO2021","AEO2022","AnyMOD",
               "FECM-NEMS", "EC-MSMR",
               "EIA_Historic","EIA_STEO","EPA-GHGI","NCS-GHGI","ENERGY2020", "EPA-TIMES","EPS",
               "FARM",
               "GCAM","GCAM-USA","gTech",
               "MARKAL-NETL","MER",
               "NATEM",
               "ReEDS",
               "TEMPO",
               "US-REGEN","USREP-ReEDS"),

    # scenarios

    main_scenarios = c("NT.Ref", "0by50.Ref","0by50.Adv",
                       "0by50.BSG.Adv","0by50.CMSG.Adv","0by50.ISG.Adv","0by50.TSG.Adv"),

    # time intervals
    default10 = seq(2020, 2050, by = 10),
    default5 = seq(2020, 2050, by = 5),
    default_2020_2050 = c(2020, 2050),
    hist_2005 = seq(2005, 2050, by = 5),
    # regions
    default_regions = c("United States"),
  ),

  ######################################################################################### -
  ######################################################################################### -

  ##### Template and Metadata ---------------------------------------------------

  tar_target(template_csv, "data-raw/templates/EMF37_data_template_R2_v2.xlsx", format = "file"),
  template = read_emf_template_xlsx(template_csv),

  tar_target(scen_mapping_csv, "data-raw/scenario-mapping.csv", format = "file"),

  ######################################################################################### -
  ######################################################################################### -

  #### Data Files ----------------------------------------------------------------

  tar_target(extra_data_folder, path("data-raw", "model-runs", "round2"), format = "file"),
  tar_target(extra_data_files, dir_ls(extra_data_folder), format = "file"),

  tar_target(data_files, c(extra_data_files),
             format = "file"),

  ######################################################################################### -
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

  data_long = make_emf_data_long(data_long_read,
                                     ratio_var,
                                     summation_var,
                                     cumulative_var,
                                     annual_growth_rate_var,
                                     per_diff_var),

  data_index = index_data_long(data_long, index_var),

  clean_data = write.csv(emf_usacanmex, "output/clean_data.csv"),

  ######################################################################################### -
  ######################################################################################### -

  ### Plot Mapping CSVs --------------

  tar_map(
    values = figmap_list,
    tar_target(figmap_csv, figmap_csv_path(fig_subject, fig_type, config), format = "file"),
    tar_target(figmap, import_figure_csv(figmap_csv, fig_type, config))
  ),

  # create_list_of_graphs
  graph_list = create_graph_list(config),

  ######################################################################################### -
  ######################################################################################### -

  ### Figures  -------------------

  # tar_render(
  #   overview_paper,
  #   "docs/round2/overview_paper_final.Rmd",
  #   output_dir = "output/round2/op",
  #   output_file = "overview_paper",
  #   params = list(
  #     mode = "targets"),
  # ),
  #
  # op_cu = create_graph("op", "cone_uncertainty", config, emf_data_long_temp, figmap_op_cone),
  # op_db = create_graph("op", "diff_bar", config, emf_data_long_temp, figmap_op_diffbar),
  # op_sb = create_graph("op", "stacked_bar", config, emf_data_long_temp, figmap_op_stackbar),
  # op_band = create_graph("op", "band", config, emf_data_long_temp, figmap_op_band),
  # op_band_index = create_graph("op", "band", config, emf_data_index, figmap_op_band, sub="_index"),
  # op_ts = create_graph("op", "time_series", config, emf_data_long_temp, figmap_op_timeseries),
  # op_ts_index = create_graph("op", "time_series", config, emf_data_index, figmap_op_timeseries, sub="_index"),
  # op_scatter = create_graph("op", "scatterplot", config, emf_data_long_temp, figmap_op_scatter),

)
