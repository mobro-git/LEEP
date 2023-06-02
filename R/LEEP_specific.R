################################################################################################################################################
################################################################################################################################################
################################################################################################################################################



## Function to ggsave figures with defaults and file format specified once for all figures
savefig = function(figure, fig_no, format = "svg", wd = 7, ht = 5, unit = "in") {
  ggsave(filename = paste("output/final_figures/",format,"/Fig",fig_no,".",format, sep = ""),
         plot = figure,
         width = wd,
         height = ht,
         units = unit)
}


################################################################################################################################################
################################################################################################################################################
################################################################################################################################################




# This function does a bunch of stuff
# Calls the main functions to print a spagetti plot
# Then makes some dot plots to show the distributions: one for 2030, one for 2035
# The inputs are the same as for the print_graph() function, plus some new ones:
#   ymin and ymax: to manually set the y axis limits so that the axes line up across plots
#   plot_title: manually set the plot title of the combined plot
#   metric: what centrality metric to show on the dot plots: "mean" or "median"
dot_plots_sens = function(plot_type, config, emf_data_long, figmap, figure_num, reg, ymin, ymax, plot_title, metric, labels = FALSE, historic_coord = c(0,0), iralow_coord = c(0,0), irahigh_coord = c(0,0)) {

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  # make the spagetti plot
  fig = print_graph(plot_type, config, emf_data_long, figmap, figure_num, reg)
  # update the axis range
  fig = fig + scale_y_continuous(limits = c(ymin, ymax)) +
    #scale_x_continuous(breaks = c(2005, 2021, 2025, 2030, 2035), labels = c(2005, 2021, 2025, 2030, 2035)) +
    scale_alpha(range = c(0.6, 1), guide = "none") +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historic", color = "black", alpha = 1) +
    annotate("text", x = iralow_coord[1], y = iralow_coord[2], label = "IRA.Low", color = "#A1A1A1", alpha = 1) +
    annotate("text", x = irahigh_coord[1], y = irahigh_coord[2], label = "IRA.High", color = "#363636", alpha = 1) +
    theme(legend.position = "none", plot.title = element_blank())

  if (labels) {
    label_data = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
      filter(year == 2035)
    print(unique(label_data$model))
    fig = fig +
      geom_text(data = label_data, aes(x = year + 0.25, y = value, label = model, color = scenario), size = 2, hjust = 0) +
      #geom_text_repel(data = label_data, aes(label = model), size = 2, hjust = 0) +
      scale_x_continuous(limits = c(NA,2037))
  }

  # pull the data from the graph and filter/summarize for 2030/2035
  df_30 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))
  df_35 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))

  stats = 0
  # these are the summary stats we'll eventually want to overlay on the plot to highlight the efficacy of the IRA
  # preira_mean_30 = df_stat_30[df_stat_30$scenario == "No IRA","mean"]$mean
  # ira_mean_30 = df_stat_30[df_stat_30$scenario == "IRA","mean"]$mean
  # preira_mean_35 = df_stat_35[df_stat_35$scenario == "No IRA","mean"]$mean
  # ira_mean_35 = df_stat_35[df_stat_35$scenario == "IRA","mean"]$mean
  # preira_med_30 = df_stat_30[df_stat_30$scenario == "No IRA","median"]$median
  # ira_med_30 = df_stat_30[df_stat_30$scenario == "IRA","median"]$median
  # preira_med_35 = df_stat_35[df_stat_35$scenario == "No IRA","median"]$median
  # ira_med_35 = df_stat_35[df_stat_35$scenario == "IRA","median"]$median
  #
  # # calculate raw differences
  # mean_diff_30 = preira_mean_30 - ira_mean_30
  # mean_diff_35 = preira_mean_35 - ira_mean_35
  # med_diff_30 = preira_med_30 - ira_med_30
  # med_diff_35 = preira_med_35 - ira_med_35
  #
  # # calculate percent differences
  # mean_pctdiff_30 = mean_diff_30 / preira_mean_30
  # mean_pctdiff_35 = mean_diff_35 / preira_mean_35
  # med_pctdiff_30 = med_diff_30 / preira_med_30
  # med_pctdiff_35 = med_diff_35 / preira_med_35
  #
  # # stick em all in a data frame
  # # stats = 0
  # stats = data.frame("year" = c(2030,2035,2030,2035), "metric" = c("mean","mean","median","median"),
  #                    "No IRA mean" = c(preira_mean_30, preira_mean_35, preira_med_30, preira_med_35),
  #                    "IRA mean" = c(ira_mean_30, ira_mean_35, ira_med_30, ira_med_35),
  #                    "difference" = c(mean_diff_30, mean_diff_35, med_diff_30, med_diff_35),
  #                    "percent difference" = c(mean_pctdiff_30, mean_pctdiff_35, med_pctdiff_30, med_pctdiff_35))

  # still not entirely sure what this does...
  # subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  # a few of the ggplot lines of code vary based on the year or on the metric chosen, so conditionally choose the right one here
  if (metric == "mean") {
    segment_code_30 = geom_segment(data = df_stat_30,
                                   aes(x = year - 0.04, xend = year + 0.04, y = mean, yend = mean, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35,
                                   aes(x = year - 0.04, xend = year + 0.04, y = mean, yend = mean, color = scenario),
                                   size = 1)
    text_code = geom_text(data = df_stat_30, aes(x = year - 0.08, y = mean, color = scenario),
                          label = "Avg", size = 3, hjust = 0.5, angle = 90, position = position_dodge2(width = 0.05))
  } else if (metric == "median") {
    segment_code_30 = geom_segment(data = df_stat_30,
                                   aes(x = year - 0.04, xend = year + 0.04, y = median, yend = median, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35,
                                   aes(x = year - 0.04, xend = year + 0.04, y = median, yend = median, color = scenario),
                                   size = 1)
    text_code = geom_text(data = df_stat_30, aes(x = year - 0.08, y = median, color = scenario),
                          label = "Med", size = 3, hjust = 0.5, angle = 90, position = position_dodge2(width = 0.05))
  }

  # dot plot for 2030
  dots_30 = ggplot() +
    geom_point(data = df_30, aes(x = year, y = value, color = scenario), shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    segment_code_30 +
    #text_code +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029.9, 2030.1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions Sensitivity") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # dot plot for 2035
  dots_35 = ggplot() +
    geom_point(data = df_35, aes(x = year, y = value, color = scenario), shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034.9, 2035.1)) +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions Sensitivity") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # combine all the plots
  final = fig + dots_30 + dots_35 +
    plot_annotation(title = plot_title, theme = theme(plot.title = element_blank())) +
    plot_layout(widths = c(10,1,1)) +
    theme(plot.margin = margin(0,0,0,0))

  # return the full plot + the components + the summary stats
  return(list(
    full = final,
    line = fig,
    dot1 = dots_30,
    dot2 = dots_35,
    stats = stats
  ))
}

# p = dot_plots("time_series", config, clean_data, figmap_leep_timeseries, 4, "United States", 0, 2500, "Emissions|CO2|Energy|Demand|Electricity", metric = "median")
# print(p$full)
# print(p$stats)


################################################################################################################################################
################################################################################################################################################
################################################################################################################################################


# function for spaghetti plots + dot plots
dot_plots = function(plot_type, config, emf_data_long, figmap, figure_num, reg, ymin, ymax, plot_title, metric, labels = FALSE, hist_year = 2021, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0)) {
  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  # make the spagetti plot
  fig = print_graph(plot_type, config, emf_data_long, figmap, figure_num, reg)
  # update the axis range
  fig = fig + scale_y_continuous(limits = c(ymin, ymax)) +
    scale_x_continuous(breaks = c(2005,2010, hist_year, 2025, 2030, 2035), labels = c(2005, 2010, hist_year, 2025, 2030, 2035)) +
    scale_alpha(range = c(0.6, 1), guide = "none") +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historic", color = "black", alpha = 1) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = "#F28063", alpha = 1) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = "#0388B3", alpha = 1) +
    theme(legend.position = "none", plot.title = element_blank())

  if (labels) {
    label_data = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
      filter(year == 2035)
    print(unique(label_data$model))
    fig = fig +
      geom_text(data = label_data, aes(x = year + 0.25, y = value, label = model, color = scenario), size = 2, hjust = 0) +
      #geom_text_repel(data = label_data, aes(label = model), size = 2, hjust = 0) +
      scale_x_continuous(limits = c(NA,2037))
  }

  # pull the data from the graph and filter/summarize for 2030/2035
  df_30 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))
  df_35 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              median = median(value, na.rm = TRUE))

  # these are the summary stats we'll eventually want to overlay on the plot to highlight the efficacy of the IRA
  preira_mean_30 = df_stat_30[df_stat_30$scenario == "No IRA","mean"]$mean
  ira_mean_30 = df_stat_30[df_stat_30$scenario == "IRA","mean"]$mean
  preira_mean_35 = df_stat_35[df_stat_35$scenario == "No IRA","mean"]$mean
  ira_mean_35 = df_stat_35[df_stat_35$scenario == "IRA","mean"]$mean
  preira_med_30 = df_stat_30[df_stat_30$scenario == "No IRA","median"]$median
  ira_med_30 = df_stat_30[df_stat_30$scenario == "IRA","median"]$median
  preira_med_35 = df_stat_35[df_stat_35$scenario == "No IRA","median"]$median
  ira_med_35 = df_stat_35[df_stat_35$scenario == "IRA","median"]$median

  # calculate raw differences
  mean_diff_30 = preira_mean_30 - ira_mean_30
  mean_diff_35 = preira_mean_35 - ira_mean_35
  med_diff_30 = preira_med_30 - ira_med_30
  med_diff_35 = preira_med_35 - ira_med_35

  # calculate percent differences
  mean_pctdiff_30 = mean_diff_30 / preira_mean_30
  mean_pctdiff_35 = mean_diff_35 / preira_mean_35
  med_pctdiff_30 = med_diff_30 / preira_med_30
  med_pctdiff_35 = med_diff_35 / preira_med_35

  # stick em all in a data frame
  stats = 0
  # stats = data.frame("year" = c(2030,2035,2030,2035), "metric" = c("mean","mean","median","median"),
  #                    "No IRA mean" = c(preira_mean_30, preira_mean_35, preira_med_30, preira_med_35),
  #                    "IRA mean" = c(ira_mean_30, ira_mean_35, ira_med_30, ira_med_35),
  #                    "difference" = c(mean_diff_30, mean_diff_35, med_diff_30, med_diff_35),
  #                    "percent difference" = c(mean_pctdiff_30, mean_pctdiff_35, med_pctdiff_30, med_pctdiff_35))

  # still not entirely sure what this does...
  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  # a few of the ggplot lines of code vary based on the year or on the metric chosen, so conditionally choose the right one here
  if (metric == "mean") {
    segment_code_30 = geom_segment(data = df_stat_30,
                                   aes(x = year - 0.04, xend = year + 0.04, y = mean, yend = mean, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35,
                                   aes(x = year - 0.04, xend = year + 0.04, y = mean, yend = mean, color = scenario),
                                   size = 1)
    text_code = geom_text(data = df_stat_30, aes(x = year - 0.08, y = mean, color = scenario),
                          label = "Avg", size = 3, hjust = 0.5, angle = 90, position = position_dodge2(width = 0.05))
  } else if (metric == "median") {
    segment_code_30 = geom_segment(data = df_stat_30,
                                   aes(x = year - 0.04, xend = year + 0.04, y = median, yend = median, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35,
                                   aes(x = year - 0.04, xend = year + 0.04, y = median, yend = median, color = scenario),
                                   size = 1)
    text_code = geom_text(data = df_stat_30, aes(x = year - 0.08, y = median, color = scenario),
                          label = "Med", size = 3, hjust = 0.5, angle = 90, position = position_dodge2(width = 0.05))
  }

  # dot plot for 2030
  dots_30 = ggplot() +
    geom_point(data = df_30, aes(x = year, y = value, color = scenario), shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    segment_code_30 +
    #text_code +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029.9, 2030.1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # grapes
  # dot plot for 2035
  dots_35 = ggplot() +
    geom_point(data = df_35, aes(x = year, y = value, color = scenario), shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034.9, 2035.1)) +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))

  # combine all the plots
  final = fig + dots_30 + dots_35 +
    plot_annotation(title = plot_title, theme = theme(plot.title = element_blank())) +
    plot_layout(widths = c(10,1,1)) +
    theme(plot.margin = margin(0,0,0,0))

  # return the full plot + the components + the summary stats
  return(list(
    full = final,
    line = fig,
    dot1 = dots_30,
    dot2 = dots_35,
    stats = stats
  ))
}


################################################################################################################################################
################################################################################################################################################
################################################################################################################################################


duplicate_historical_data <- function(data_long, hist_model, hist_year, criteria = FALSE) {
  # short circuit
  if (hist_model == "none") {
    return(data_long)
  }

  # this is the data we'll stamp for every model
  copy_data = data_long %>% filter(model == hist_model, year == hist_year)
  # this is all the historical data, don't lose it
  hist_data = data_long %>% filter(model == hist_model & year <= hist_year)
  # temporary data_long which will be appended many times
  data_in = data_long %>% filter(year >= hist_year & model != hist_model)

  if (criteria) {
    data_25 = data_long %>% filter(model != hist_model, year == 2025) %>%
      mutate(year = hist_year)
    data_in = rbind(data_in, data_25)
  }

  data_wide = data_in %>% left_join(select(copy_data,c(value,year,variable,region)), by = c("year", "variable","region")) %>%
    mutate(value = case_when(
      year == hist_year & !is.na(value.y) ~ value.y,
      TRUE ~ value.x
    )) %>%
    select(-c(value.x,value.y))

  data_final = rbind(data_wide, hist_data)

  # the end
  return(data_final)
}

# remove all historical data and replace with EIA-LTS 2020 data for connectivity purposes
# clean_data2 = clean_data %>% duplicate_historical_data("EIA-LTS",2020)



################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

# function for coneplot with dots overlaid
lts_coneplot_with_dots = function(config, data, figmap, lts_fignum, leep_fignum, region, ymin, ymax, title, titlesize = 8, metric = "mean", facet = FALSE) {
  # update the historical data (is this even necessary anymore?)
  data = data %>% duplicate_historical_data("EIA-LTS",2020)

  # make the LTS cone plot
  fig1 = print_graph("cone_uncertainty", config, data, figmap, lts_fignum, region) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    scale_x_continuous(breaks = c(2005, 2021, 2030, 2035, 2050), labels = c(2005, 2021, 2030, 2035, 2050)) +
    #again
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5, size = titlesize))


  # extract points and summary statistics for 2030 and 2035
  df_30 = data_from_graph("cone_uncertainty", config, data, figmap, leep_fignum, region) %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))

  df_35 = data_from_graph("cone_uncertainty", config, data, figmap, leep_fignum, region) %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))

  if (facet) {
    dash_len = 2.0
  } else {
    dash_len = 0.5
  }

  if (metric == "mean") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - dash_len, xend = year + dash_len, y = mean, yend = mean,
                                                          color = variable_rename), position = position_dodge2(width = 0.5), linewidth = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - dash_len, xend = year + dash_len, y = mean, yend = mean,
                                                          color = variable_rename), position = position_dodge2(width = 0.5), linewidth = 1)
  } else if (metric == "median") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - dash_len, xend = year + dash_len, y = median, yend = median,
                                                          color = variable_rename), position = position_dodge2(width = 0.5), linewidth = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - dash_len, xend = year + dash_len, y = median, yend = median,
                                                          color = variable_rename), position = position_dodge2(width = 0.5), linewidth = 1)
  }


  fig2 = fig1 +
    geom_point(data = df_30, aes(x = year, y = value, color = variable_rename),
               shape = 1, size = 2, position = position_dodge(width = 1)) +
    segment_code_30 +
    geom_point(data = df_35, aes(x = year, y = value, color = variable_rename),
               shape = 1, size = 2, position = position_dodge(width = 1)) +
    segment_code_35 +
    ggtitle(title) +
    theme(legend.position = "none", axis.title.x = element_blank())
  #bottom1
  return(fig2)

}

################################################################################################################################################
################################################################################################################################################
################################################################################################################################################

medians_table = function(plot_type, config, data, figmap, plot_num, region, models_to_exclude = c()) {
  med_dta = data_from_graph(plot_type, config, data, figmap, plot_num, region)
  if (length(models_to_exclude) > 0) {
    med_dta = med_dta %>% filter(!(model %in% models_to_exclude))
  }

  # calculate difference in medians
  diff_med = med_dta %>%
    filter(year %in% c(2030, 2035)) %>%
    mutate(scenario = case_when(
      scenario == "No IRA" ~ "No_IRA",
      TRUE ~ scenario
    )) %>%
    group_by(year, scenario, variable, unit) %>%
    summarize(median = median(value)) %>%
    data.frame() %>%
    pivot_wider(names_from = scenario, values_from = median, names_prefix = "median_") %>%
    mutate(absolute_diff_of_medians = median_IRA - median_No_IRA,
           percent_diff_of_medians = (median_IRA - median_No_IRA) / median_No_IRA)

  # calculate median of differences
  med_diff = med_dta %>%
    data.frame() %>%
    select(model, scenario, variable, unit, year, value) %>%
    filter(year %in% c(2030, 2035)) %>%
    mutate(scenario = case_when(
      scenario == "No IRA" ~ "No_IRA",
      TRUE ~ scenario
    )) %>%
    pivot_wider(names_from = scenario, values_from = value, names_prefix = "value_") %>%
    mutate(absolute_diff = value_IRA - value_No_IRA,
           percent_diff = (value_IRA - value_No_IRA) / value_No_IRA) %>%
    group_by(variable, unit, year) %>%
    summarize(median_of_abs_diffs = median(absolute_diff),
              median_of_pct_diffs = median(percent_diff)) %>%
    data.frame()

  # stick 'em together
  full_diffs = diff_med %>% left_join(
    select(med_diff, c(year, variable, median_of_abs_diffs, median_of_pct_diffs)),
    by = c("year","variable")
  ) %>%
    arrange(variable, year)

  return(full_diffs)
}
