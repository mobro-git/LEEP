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
    round_num = 2,
    scen_mapping = read_scen_mapping(scen_mapping_csv),
    template = emf_template,
    calculated_var = all_calculated,

    shape_map = data.frame(model = c("ADAGE","AEO2020","AnyMOD","FECM-NEMS","EIA_Historic",
                                     "EIA_STEO","ENERGY2020","EPA-GHGI","EPA-TIMES","EPS","FARM","GCAM",
                                     "GCAM-USA","gTech","MARKAL-NETL","MER","NATEM","TEMPO",
                                     "US-REGEN"),
                           abbrev = c("A","B","D","C","H","I","P","T","E","F","G","S","J",
                                      "M","R","N","O","U","Z"),
                           ref_abbrev = tolower(c("A","B","D","C","H","I","P","T","E","F",
                                                  "G","S","J","M","R","N","O","U","Z"))),

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
    models_benchmark = c("AEO2021","EPA-GHGI","EIA_Historic","EIA_STEO",
                         "ADAGE","AnyMOD","FECM-NEMS","EC-MSMR","ENERGY2020","EPA-TIMES",
                         "EPS","FARM","GCAM","GCAM-USA","gTech","MA3T","MARKAL-NETL","NATEM",
                         "Pathways","ReEDS","Scout","TEMPO","TEMOA","US-REGEN","USREP-ReEDS"),
    benchmark = "AEO2021",
    models_r2 = c("EPA-TIMES","EC-MSMR","EPS","GCAM","GCAM-USA","MARKAL-NETL","NATEM","ReEDS"),
    models_main = c("ADAGE","AEO2021","AnyMOD","FECM-NEMS","EC-MSMR","EPA-TIMES","EPS","FARM","GCAM","GCAM-USA","gTech",
                    "NATEM","RIO","TEMOA","US-REGEN","USREP-ReEDS"), # "ReEDS","Scout","TEMPO","MA3T","ENERGY2020"
    models_elc = c("ADAGE","AEO2021","AnyMOD","FECM-NEMS","EC-MSMR","EPA-TIMES","EPS","FARM","GCAM","GCAM-USA","gTech",
                    "NATEM","TEMOA","US-REGEN","USREP-ReEDS", "MARKAL-NETL", "ReEDS"),
    models_op = c("ADAGE","AnyMOD","AEO2021","EC-MSMR","EPA-TIMES","EPS","FARM","FECM-NEMS","GCAM","GCAM-USA","gTech",
                  "MARKAL-NETL","NATEM","TEMOA","US-REGEN","USREP-ReEDS"), #"ENERGY2020","MA3T","RIO","ReEDS","Scout","TEMPO"
    models_co2price = c("ADAGE","EC-MSMR","FARM","GCAM","GCAM-USA","gTech","MARKAL-NETL","NATEM","USREP-ReEDS"),
    models_main_no_sectoral = c("ADAGE","AEO2021","AnyMOD","FECM-NEMS","EC-MSMR","ENERGY2020","EPA-TIMES","EPS",
                    "FARM","GCAM","GCAM-USA","gTech","MA3T","MARKAL-NETL","NATEM","RIO","TEMOA",
                    "TEMPO","US-REGEN","USREP-ReEDS"),
    models_sm = c("FECM-NEMS","ENERGY2020","EC-MSMR","EPA-TIMES","EPS", "GCAM",
                  "GCAM-USA", "MARKAL-NETL","ReEDS","US-REGEN"),

    # scenarios
    all_scenarios = c(
      ## NT
      "NT.Ref","NT.Adv","NT.BSG.Adv","NT.ISG.Adv","NT.TSG.Adv","NT.CMSG.Adv",
      ## 0by50
      "0by50.Ref","0by50.Adv","0by50.BSG.Adv","0by50.CMSG.Adv","0by50.ISG.Adv","0by50.TSG.Adv",
      # CMSG
      "0by50.CMSG1","0by50.CMSG2","0by50.CMSG3","0by50.CMSG4","0by50.CMSG6","0by50.CMSG7",
      # ISG
      "0by50.ISG1","0by50.ISG2","0by50.ISG3","0by50.ISG4",
      # TSG
      "0by50.TSG1","0by50.TSG2","0by50.TSG3","0by50.TSG4",
      # BSG
      "0by50.BSG1","0by50.BSG2","0by50.BSG3",
      ## 0GHGby50
      "0GHGby50.Ref","0GHGby50.Adv",
      ## 0by60
      "0by60.Ref","0by60.Adv","0by60.CMSG.Adv","0by60.CMSG1","0by60.CMSG2","0by60.CMSG3","0by60.CMSG4",
      ## 0by80
      "0by80.Ref","0by80.Adv","0by80.CMSG.Adv","0by80.CMSG1","0by80.CMSG2","0by80.CMSG3","0by80.CMSG4"),

    band_nz50 = c(
      "NT.Ref",
      "0by50.Ref","0by50.Adv","0by50.BSG.Adv","0by50.CMSG.Adv","0by50.ISG.Adv","0by50.TSG.Adv",
      "0by50.CMSG1","0by50.CMSG2","0by50.CMSG3","0by50.CMSG4","0by50.CMSG6","0by50.CMSG7",
      "0by50.ISG1","0by50.ISG2","0by50.ISG3","0by50.ISG4",
      "0by50.TSG1","0by50.TSG2","0by50.TSG3","0by50.TSG4",
      "0by50.BSG1","0by50.BSG2","0by50.BSG3"),

    main_scenarios = c("NT.Ref", "0by50.Ref","0by50.Adv",
                       "0by50.BSG.Adv","0by50.CMSG.Adv","0by50.ISG.Adv","0by50.TSG.Adv"),

    sm_scenarios = c("NT.Ref", "0by50.Ref","0by50.Adv"),

    min_scenarios = c("NT.Ref", "0by50.Ref"),

    scatter_scenarios = c("0by50.Ref","0by50.Adv","0by50.CMSG.Adv","0by50.ISG.Adv","0by50.TSG.Adv"),

    nz50_all = c("0by50.Ref","0by50.Adv","0by50.BSG.Adv","0by50.CMSG.Adv","0by50.ISG.Adv","0by50.TSG.Adv"),

    nz50_scenarios = c("0by50.Ref","0by50.Adv"),

    nz50_sg_scenarios = c("0by50.BSG.Adv","0by50.CMSG.Adv","0by50.ISG.Adv","0by50.TSG.Adv"),

    bsg_presentation = c("NT.Ref","NT.BSG.Adv","0by50.Ref","0by50.Adv",
                         "0by50.BSG.Adv","0by50.BSG1","0by50.BSG2"),

    # isg_scenarios = c("NT.ISG.Adv","0by50.Ref","0by50.ISG.Adv",
    #                   "0by50.ISG1","0by50.ISG2","0by50.ISG3","0by50.ISG4"),

    isg_scenarios = c("NT.Ref","0by50.Ref","0by50.ISG.Adv","0by50.Adv"),

    tsg_scenarios = c("NT.Ref", "0by50.Ref","0by50.Adv",
                      "0by50.TSG.Adv","0by50.TSG1","0by50.TSG2","0by50.TSG3","0by50.TSG4"),


    # time intervals
    default10 = seq(2020, 2050, by = 10),
    default5 = seq(2020, 2050, by = 5),
    default_2020_2050 = c(2020, 2050),
    hist_2005 = seq(2005, 2050, by = 5),
    # regions
    default_regions = c("United States"),
    usrr_regions = c("AK","CA","FL","MOUNT","NCENT","NEAST","NENGL",
                     "NY","PACIF","SCENT","SEAST","TX","USA")
  ),

  ######################################################################################### -
  ######################################################################################### -

  ##### Template and Metadata ---------------------------------------------------

  tar_target(emf_template_csv, "data-raw/templates/EMF37_data_template_R2_v2.xlsx", format = "file"),
  emf_template = read_emf_template_xlsx(emf_template_csv),

  tar_target(scen_mapping_csv, "data-raw/scen-map/scenario-mapping-emf37-round2.csv", format = "file"),

  # TODO: NEEDS UPDATING (IF WE WANT TO USE)
  tar_target(variable_info_csv, "data-raw/metadata/variable_info_round1.csv", format = "file"),
  variable_info_lookup = readr::read_csv(variable_info_csv, col_types = cols()),

  tar_target(emf_model_metadata_csv, "data-raw/metadata/model_meta.csv", format = "file"),
  emf_model_metadata = read_csv_model_metadata(emf_model_metadata_csv),

  ######################################################################################### -
  ######################################################################################### -

  #### Data Files ----------------------------------------------------------------

  # _IIASA raw data --------

  # IIASA data download not currently part of the targets pipeline, but cached in csv format.
  # Refresh DB downloaded data by re-running: scripts/iiasa_db_download.R

  # tar_target(iiasa_usa_data_csv, "data-raw/iiasa-db-data/emf37_usa.csv", format = "file"),
  # iiasa_usa_data = readr::read_csv(iiasa_usa_data_csv, col_types = cols()),
  #
  # tar_target(iiasa_can_data_csv, "data-raw/iiasa-db-data/emf37_can.csv", format = "file"),
  # iiasa_can_data = readr::read_csv(iiasa_can_data_csv, col_types = cols()),
  #
  # tar_target(iiasa_mex_data_csv, "data-raw/iiasa-db-data/emf37_mex.csv", format = "file"),
  # iiasa_mex_data = readr::read_csv(iiasa_mex_data_csv, col_types = cols()),

  tar_target(iiasa_national_data_csv, "data-raw/iiasa-db-data/emf37_usacanmex.csv", format = "file"),
  iiasa_national_data = readr::read_csv(iiasa_national_data_csv, col_types = cols()),

  tar_target(iiasa_usa_states_data_csv, "data-raw/iiasa-db-data/emf37_usa_states.csv", format = "file"),
  iiasa_usa_states_data = readr::read_csv(iiasa_usa_states_data_csv, col_types = cols()),

  tar_target(iiasa_usa_census_regions_data_csv, "data-raw/iiasa-db-data/emf37_usa_census_regions.csv", format = "file"),
  iiasa_usa_census_regions_data = readr::read_csv(iiasa_usa_census_regions_data_csv, col_types = cols()),

  tar_target(iiasa_canada_provinces_data_csv, "data-raw/iiasa-db-data/emf37_canada_provinces.csv", format = "file"),
  iiasa_canada_provinces_data = readr::read_csv(iiasa_canada_provinces_data_csv, col_types = cols()),

  # TODO: 2020 data are manually created by duplicating 2025 data, need to fix for the next round
  # _Scout data --------
  # tar_target(scout_us_csv, "data-raw/iiasa-db-data/scout_US.csv", format = "file"),
  # scout_us_data = {
  #   readr::read_csv(scout_us_csv, col_types = cols()) %>%
  #     select(model,scenario,region,variable,unit,year,value,datasrc)},

  # _External raw data ----

  tar_target(extra_data_folder, path("data-raw", "model-runs", "round2"), format = "file"),
  tar_target(extra_data_files, dir_ls(extra_data_folder), format = "file"),

  tar_target(data_files, c(iiasa_national_data_csv,
                           # iiasa_usa_data_csv,
                           # iiasa_can_data_csv,
                           # iiasa_mex_data_csv,
                           # iiasa_usa_states_data_csv,
                           iiasa_usa_census_regions_data_csv,
                           iiasa_canada_provinces_data_csv,
                           # scout_us_csv,
                           extra_data_files),
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

  emf_data_raw = map_dfr(data_files, read_raw_data_file),

  emf_data_min = map_dfr(data_files, read_process_minimal_from_raw),

  unique_submissions = {
    emf_data_min %>%
      select(datasrc,model,scenario) %>%
      distinct() %>%
      write.csv("output/round2/unique_submissions.csv")},

  emf_data_long_read = {
    map_dfr(data_files, ~read_process_data_file(.x, config)) %>%
    # map_variable_names() %>%
    arrange_standard()},

  emf_data_long = make_emf_data_long(emf_data_long_read,
                                     ratio_var,
                                     summation_var,
                                     cumulative_var,
                                     annual_growth_rate_var,
                                     per_diff_var),

  emf_data_long_temp = {emf_data_long %>% filter(!(model == "AnyMOD" & scenario == "0by50.CMSG1"))},

  emf_data_index = index_data_long(emf_data_long_temp, index_var),

  emf_usacanmex = {emf_data_long %>% filter(region %in% c("United States","Canada","Mexico"))},
  emf_subnational = {emf_data_long %>% filter(!region %in% c("United States","Canada","Mexico"))},

  emf_usacanmex_output = write.csv(emf_usacanmex, "output/round2/cleaned_emf_data_national.csv"),
  emf_subnational_output = write.csv(emf_subnational, "output/round2/cleaned_emf_data_subnational.csv"),

  ######################################################################################### -
  ######################################################################################### -

  ### Data Roll-Ups --------------

  # Summary data on the main indicators
  emf_summary_long = make_emf_summary_long(emf_data_long, variable_info_lookup),
  emf_summary = make_emf_summary(emf_summary_long, variable_info_lookup),

  ######################################################################################### -
  ######################################################################################### -

  ### Plot Mapping CSVs --------------

  # targets for each figmap csv and figmap import
  # figmap_csv and figmap are created outside of tar_plan(). top of _targets.R
  # figmap_csv targets: figmap_csv_subject_figtype
  # figmap targets: figmap_subject_figtype

  tar_map(
    values = figmap_list,
    tar_target(figmap_csv, figmap_csv_path(fig_subject, fig_type, config), format = "file"),
    tar_target(figmap, import_figure_csv(figmap_csv, fig_type, config))
  ),

  # create_list_of_graphs
  graph_list = create_graph_list(config),

  ######################################################################################### -
  ######################################################################################### -

  #### QA outputs ----------------------

  ## Text Output (for change-detection)

  tar_render_rep(
    text_rmd_outputs,
    "docs/round2/text-summary.Rmd",
    output_dir = "output/round2/text_summary",
    params = text_rmd_params_df
  ),

  text_rmd_params_df = tibble(
    model = unique(emf_data_long$model),
    output_file = glue("summary_{model}.md"),
    mode = "targets"
  ),

  ## Submission Summary

  tar_render(
    submission_summary,
    "docs/round2/submission_summary.Rmd",
    output_dir = "output/round2/audit",
    output_file = "submission summary",
    params = list(
      mode = "targets"),
  ),

  ## Benchmark Summary

  tar_render(
    benchmark_summary,
    "docs/round2/benchmark-tables.Rmd",
    output_dir = "output/round2/audit",
    output_file = "benchmark summary",
    params = list(
      mode = "targets"),
  ),

  ## Audit Summary

  # audit summation check

  # tar_target(audit_sums_csv, "plot_mapping/round2/audit_sum_var_round2.csv", format = "file"),
  # audit_sums = readr::read_csv(audit_sums_csv, col_types = cols()),
  #
  # tar_render(
  #   audit_sums_report,
  #   "docs/round2/audit_sums.Rmd",
  #   output_dir = "output/round2/audit",
  #   output_file = "audit sums report",
  #   params = list(
  #     mode = "targets"),
  # ),

  ######################################################################################### -
  ######################################################################################### -

  ### Figures  -------------------

  tar_render(
    overview_paper,
    "docs/round2/overview_paper_final.Rmd",
    output_dir = "output/round2/op",
    output_file = "overview_paper",
    params = list(
      mode = "targets"),
  ),

  op_cu = create_graph("op", "cone_uncertainty", config, emf_data_long_temp, figmap_op_cone),
  op_db = create_graph("op", "diff_bar", config, emf_data_long_temp, figmap_op_diffbar),
  op_sb = create_graph("op", "stacked_bar", config, emf_data_long_temp, figmap_op_stackbar),
  op_band = create_graph("op", "band", config, emf_data_long_temp, figmap_op_band),
  op_band_index = create_graph("op", "band", config, emf_data_index, figmap_op_band, sub="_index"),
  op_ts = create_graph("op", "time_series", config, emf_data_long_temp, figmap_op_timeseries),
  op_ts_index = create_graph("op", "time_series", config, emf_data_index, figmap_op_timeseries, sub="_index"),
  op_scatter = create_graph("op", "scatterplot", config, emf_data_long_temp, figmap_op_scatter),

  overview_sb = create_graph("overview", "stacked_bar", config, emf_data_long_temp, figmap_overview_stackbar),
  overview_db = create_graph("overview", "diff_bar", config, emf_data_long_temp, figmap_overview_diffbar),
  overview_ts = create_graph("overview", "time_series", config, emf_data_long_temp, figmap_overview_timeseries),
  overview_cu = create_graph("overview", "cone_uncertainty", config, emf_data_long_temp, figmap_overview_cone),
  overview_scatter = create_graph("overview", "scatterplot", config, emf_data_long_temp, figmap_overview_scatter),
  #overview_sankey = sankey_fn("overview", "sankey", config, emf_data_long_temp, figmap_csv_overview_sankey),
  overview_sankey_plotly = plotly_sankey_fn("overview", "sankey", config, emf_data_long_temp, figmap_csv_overview_sankey),

  # # Not using the "figmap_overview_corrplot" variable just yet, to be added
  # # overview_corrplot = corrplot_fn("overview", "corrplot", config, emf_data_long),

  # # benchmark_sb erroring out bc no data NT.Ref for variables. need to figure out how to just skip those figures instead of erroring out
  benchmark_sb = create_graph("benchmark", "stacked_bar", config, emf_data_long_temp, figmap_benchmark_stackbar),
  # # benchmark_scatter = create_graph("benchmark", "scatterplot", config, emf_data_long_temp, figmap_benchmark_scatter),

  prez_sb = create_graph("prez", "stacked_bar", config, emf_data_long_temp, figmap_prez_stackbar),
  prez_db = create_graph("prez", "diff_bar", config, emf_data_long_temp, figmap_prez_diffbar),
  prez_scatter = create_graph("prez", "scatterplot", config, emf_data_long_temp, figmap_prez_scatter),

  price_ts = create_graph("price", "time_series", config, emf_data_long_temp, figmap_price_timeseries),

  #elc_sb = create_graph("elc", "stacked_bar", config, emf_data_long_temp, figmap_elc_stackbar),
  elc_ts = create_graph("elc", "time_series", config, emf_data_long_temp, figmap_elc_timeseries),
  elc_scatter = create_graph("elc", "scatterplot", config, emf_data_long_temp, figmap_elc_scatter),
  #elc_db = create_graph("elc", "diff_bar", config, emf_data_long_temp, figmap_elc_diffbar),
  #elc_corr = create_graph("elc","corrplot", config, emf_data_long_temp, figmap_elc_corrplot),

  bsg_sb = create_graph("bsg", "stacked_bar", config, emf_data_long_temp, figmap_bsg_stackbar),
  bsg_db = create_graph("bsg", "diff_bar", config, emf_data_long_temp, figmap_bsg_diffbar),
  bsg_ts = create_graph("bsg", "time_series", config, emf_data_long_temp, figmap_bsg_timeseries),

  # isg_scatter = create_graph("isg", "scatterplot", config, emf_data_long_temp, figmap_isg_scatter, debug = FALSE),
  isg_sb = create_graph("isg", "stacked_bar", config, emf_data_long_temp, figmap_isg_stackbar),
  isg_db = create_graph("isg", "diff_bar", config, emf_data_long_temp, figmap_isg_diffbar),
  isg_cone = create_graph("isg", "cone_uncertainty", config, emf_data_long_temp, figmap_isg_cone),
  isg_ts = create_graph("isg", "time_series", config, emf_data_long_temp, figmap_isg_timeseries),

  tsg_sb = create_graph("tsg", "stacked_bar", config, emf_data_long_temp, figmap_tsg_stackbar),
  tsg_db = create_graph("tsg", "diff_bar", config, emf_data_long_temp, figmap_tsg_diffbar),
  tsg_ts = create_graph("tsg", "time_series", config, emf_data_long_temp, figmap_tsg_timeseries),

  h2_sb = create_graph("h2", "stacked_bar", config, emf_data_long_temp, figmap_h2_stackbar),
  # h2_scatter = create_graph("h2", "scatterplot", config, emf_data_long_temp, figmap_h2_scatter),
  h2_cu = create_graph("h2", "cone_uncertainty", config, emf_data_long_temp, figmap_h2_cone)

)
