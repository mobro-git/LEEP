
# allows use of pipeline graphing to print one figure and return a ggplot object that can then be edited

print_graph <- function(plot_type, config, emf_data_long, figmap, figure_num, reg, factor = FALSE, fac_var = NULL, level = NULL) {

  # select the key variables, flip values, and merge with specific figure requests
  df <- preliminary_data_processing_for_plotting(emf_data_long, figmap)

  # assign color palettes
  subpalettes = create_subpalettes(figmap, config)

  # full processing based on figure requests + print plot

  if(approved_plot_type(plot_type)) {

    data_processing_fn = get(paste(plot_type, "_figure_specific_data_processing", sep = ""))

    dat = df %>%
      filter(figure_no == figure_num & region == reg) %>%
      data_processing_fn(config)

    figure = unique(dat$title_name)
    #print(figure)
    selected = reg

    if(factor) {
        if(fac_var == "variable_rename"){
          dat <- dat %>%
            mutate(variable_rename = factor(variable_rename, levels = level))
        }
        else if(fac_var == "scenario"){
          dat <- dat %>%
            mutate(scenario = factor(scenario, levels = level))
        }
    }

    if (approved_facet_type(dat) & check_dat_before_plotting(dat)) {
      plot_fn = get_plot_fn(plot_type, unique(dat$type))
      plot = call_plot_fn(dat, figure, selected, subpalettes, plot_type, plot_fn)
    }

  plot

}}
