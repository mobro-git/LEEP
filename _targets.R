##### Setup ----------------
library(targets)
library(tarchetypes)

source("packages.R")

tar_source()

# Set target-specific options such as packages.
tar_option_set(
  packages = c("dplyr","readr","tidyverse"), # packages to make available to targets
  error = "abridge" # prevents printing of error traceback when tar_make() errors out for user-defined function
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
    template_original = template_original,
    template = template,
    calculated_var = all_calculated,

    # models
    # "models" list should include ALL individual model names included in the full clean_data set
    all_models = c("AEO2020","AEO2021","AEO2022",
               "EIA_Historic","EIA_STEO","EPA-GHGI","NCS-GHGI",
               "GCAM-CGS","EPS-EI","Haiky-RFF","IPM-NRDC","MARKAL-NETL","NEMS-RHG","ReEDS-NREL","REGEN-EPRI","RIO-REPEAT", #bistline paper
               "GCAM-PNNL"
               ),

    models_noaeo = c("NEMS","GCAM","IPM","USREP-ReEDS","Scout"),

    models_bistline = c("GCAM-CGS","EPS-EI","Haiku-RFF","IPM-NRDC","MARKAL-NETL","NEMS-RHG","ReEDS-NREL","REGEN-EPRI","RIO-REPEAT"),

    #models_emf = c("GCAM-EMF"),
    models_ghgi = c("EPA-GHGI"),

    # Use models_leep for all initial plots
    # USREP-ReEDS, GCAM-PNNL, NEMS-OP - averages for CAP-ADD
    models_leep = c("USREP-ReEDS", "EPS-EI", "GCAM-CGS", "GCAM-PNNL", "GCAM-USA", "Haiku-RFF", "IPM-NRDC", "IPM-EPA", "MARKAL-NETL", "NEMS-RHG",
                    "NEMS-OP", "REGEN-EPRI", "RIO-REPEAT", "ReEDS-NREL", "NEMS-EIA",
                    "EIA-LTS", "EIA-STEO", "EPA-ATR", "EPA", "EPA-GHGI", "EIA"),
    models_leep1 = c("USREP-ReEDS", "EPS-EI", "GCAM-CGS", "GCAM-PNNL", "GCAM-USA", "Haiku-RFF", "IPM-NRDC", "IPM-EPA", "MARKAL-NETL", "NEMS-RHG",
                    "NEMS-OP", "REGEN-EPRI", "RIO-REPEAT", "ReEDS-NREL", "NEMS-EIA",
                    "EIA-LTS", "EIA-STEO", "EPA-ATR", "EPA", "EPA-GHGI", "EIA",
                    "ReEDS-NRELr"),
    #"Scout-LEEP" removing scout from everything except scout-specific figure in chapter 4
    models_per_elc = c("USREP-ReEDS", "EPS-EI", "GCAM-CGS", "GCAM-PNNL", "GCAM-USA", "Haiku-RFF", "IPM-NRDC", "IPM-EPA", "MARKAL-NETL", "NEMS-RHG",
                       "NEMS-OP", "REGEN-EPRI", "RIO-REPEAT", "ReEDS-NREL", "Scout-LEEP"),

    #models_leep_sens = c("USREP-ReEDS", "ReEDS", "Scout v0.8", "GCAM-USA v6.0", "NEMS-OP"),
    models_lts = c("GCAM-LTS", "NEMS-OP-LTS", "EIA-LTS"),
    models_eia_only = c("EIA"),
    models_ghgi_only = c("EPA-GHGI"),

    # scenarios
    main_scenarios = c("No IRA","IRA"),
    ira_only = c("IRA", "Historic"),
    historic_only = c("Historic"),

    hist_mod_scenarios = c("No IRA","IRA"),
    hist_mod_scenarios2 = c("Historic", "No IRA", "IRA"),

    leep_sens = c("IRA.Low","IRA.High", "Historic"),

    all_scenarios = c("Historic","No IRA","IRA","Core",
                 "IRA.Low","IRA.High",
                 "High Energy Price","High Growth","Low Energy Price","Low Growth",
                 "All Advanced","Opt-IRA.Adv","Constrained Deployment","Pess-IRA.Adv",
                 "Adv Battery/Renew","Cons Battery/Renew",
                 "No IRA.2","No IRA.3","No IRA.4","No IRA.5","No IRA.6","No IRA.7"),

    scenarios_lts = c("Historic", "LTS.High", "LTS.Low", "LTS.Mid1", "LTS.Mid2", "LTS.Mid3", "LTS.Mid4",
                      "LTS.Mid5", "LTS.Mid6", "LTS.Mid7", "LTS.Mid8", "LTS.Mid9", "LTS.Mid10"),
    # scenarios_ltsplus = c("Historic", "LTS.High", "LTS.Low", "LTS.Mid1", "LTS.Mid2", "LTS.Mid3", "LTS.Mid4",
    #                   "LTS.Mid5", "LTS.Mid6", "LTS.Mid7", "LTS.Mid8", "LTS.Mid9", "LTS.Mid10", "IRA"),



    # time intervals
    usa = "United States",
    ira_2035 = c(seq(2021,2035,by=1)),
    ira_2030_2035 = c(2030,2035),
    ira_2050 = c(seq(2021,2050,by=1)),

    fives = c(seq(2005,2020,by = 1),seq(2025,2050,by = 5)),
    historic = c(seq(2005,2035,by = 1)),
    ghgi_yrs = c(seq(1990,2021,by = 1)),
    long_history = c(seq(1950,2020,by = 1)),
    five_twentyone = c(2005, 2021),
    five_twentyone_inclusive = c(seq(2005,2021,by = 1)),
    twentyone_only = c(2021),
    thirtyfive_only = c(2035)

  ),

  ######################################################################################### -
  ######################################################################################### -

  ##### Template and Metadata ---------------------------------------------------

  tar_target(template_original_csv, "data-raw/EMF37_data_template_R2_v2.xlsx", format = "file"),
  tar_target(template_original, read_emf_template_xlsx(template_original_csv)),

  tar_target(template_additions_csv, "data-raw/template_additions.xlsx", format = "file"),
  tar_target(template_additions, read_emf_template_xlsx(template_additions_csv)),

  tar_target(template, rbind(template_original,template_additions)),

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

  # _Making data_long ----
  data_raw = map_dfr(data_files, read_raw_data_file),

  data_min = map_dfr(data_files, read_process_minimal_from_raw),

  unique_submissions = {
    data_min %>%
      select(datasrc,model,scenario) %>%
      distinct() %>%
      write.csv("output/unique_submissions.csv")},

  omitted_var = check_omissions(data_raw, data_long, template_original, template),

  omitted_data = {data_raw %>% filter(variable %in% omitted_var)},

  data_long_read = {
    map_dfr(data_files, ~read_process_data_file(.x, config)) %>%
    # map_variable_names() %>%
    arrange_standard()},

  data_long = make_data_long(data_long_read),

  # data_long but can add in transformations or filter out models/variables
  # TODO: add bistline % electricity variables back in here
  clean_data = {
    data_long %>%
      unit_conversion() %>%
      complete_implicit_na() %>%
      make_clean_data() %>%
      make_calculated_vars(ratio_var, summation_var, cumulative_var, annual_growth_rate_var, per_diff_var)},

  data_output = write_csv(clean_data, "output/data/leep_data_output.csv"),

  data_wide = {clean_data %>% pivot_wider(names_from = "year", values_from = "value")},

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

  # Plot maps

  ts = create_graph("leep", "time_series", config, clean_data, figmap_leep_timeseries),
  cone = create_graph("leep", "cone_uncertainty", config, clean_data, figmap_leep_cone),
  stackbar = create_graph("leep", "stacked_bar", config, clean_data, figmap_leep_stackbar),
  diffbar = create_graph("leep", "diff_bar", config, clean_data, figmap_leep_diffbar),

  # Final Figures

  tar_render(
    chapter1,
    "docs/final figures/chapter1.Rmd",
    output_dir = "output/final_figures/rmd",
    output_file = "chapter1",
    envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
    params = list(
      mode = "targets"),
  ),

  tar_render(
    chapter2,
    "docs/final figures/chapter2.Rmd",
    output_dir = "output/final_figures/rmd",
    output_file = "chapter2",
    envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
    params = list(
      mode = "targets"),
  ),

  tar_render(
    chapter3,
    "docs/final figures/chapter3.Rmd",
    output_dir = "output/final_figures/rmd",
    output_file = "chapter3",
    envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
    params = list(
      mode = "targets"),
  ),

  tar_render(
    chapter4,
    "docs/final figures/chapter4.Rmd",
    output_dir = "output/final_figures/rmd",
    output_file = "chapter4",
    envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
    params = list(
      mode = "targets"),
  ),

  tar_render(
    chapter5,
    "docs/final figures/chapter5.Rmd",
    output_dir = "output/final_figures/rmd",
    output_file = "chapter5",
    envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
    params = list(
      mode = "targets"),
  ),

  tar_render(
    appendix,
    "docs/final figures/appendix.Rmd",
    output_dir = "output/final_figures/rmd",
    output_file = "appendix",
    envir = rlang::global_env(), ## hacky way to get rmarkdown to render in the global environ and not create its own
    params = list(
      mode = "targets"),
  )

)

# view Targets pipeline in flow-chart style with dependency links
# tar_visnetwork(targets_only=TRUE)

# compile_all_data() # compiles all .csvs in final_figures/data into one workbook
