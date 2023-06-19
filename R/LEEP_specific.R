
## Function to ggsave figures with defaults and file format specified once for all figures
savefig = function(figure, fig_no, format = c("svg","png"), wd = 7, ht = 5, unit = "in") {

  for(item in format) {
  ggsave(filename = paste("output/final_figures/",item,"/Fig",fig_no,".",item, sep = ""),
         plot = figure,
         width = wd,
         height = ht,
         units = unit)
  }
}

savedata = function(df, fig_no) {

  write.csv(df, file = paste("output/final_figures/data/Fig",fig_no,".csv",sep=""), row.names = FALSE)

}

saveall = function(figure, df, fig_no, format = c("svg","png"), wd = 7, ht = 5, unit = "in") {

  for(item in format) {
    ggsave(filename = paste("output/final_figures/",item,"/Fig",fig_no,".",item, sep = ""),
           plot = figure,
           width = wd,
           height = ht,
           units = unit)
  }

  savedata(df, fig_no)

}


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
sens_dot_plot = function(dta, title, far_left = FALSE, single = FALSE, ymin = 0, ymax = 0, ira_coord = c(0,0), low_coord = c(0,0), high_coord = c(0,0)) {
  subpalettes = create_subpalettes(figmap_leep_timeseries, config)
  if (far_left) {
    point_code = geom_point(aes(x = year + stagger, y = value, color = scenario), shape = 1, size = 2)
  } else {
    #point_code = geom_point(aes(x = year + stagger, y = value, color = model), shape = 1, size = 2)
    point_code = geom_point(aes(x = year + stagger, y = value), shape = 1, size = 2)
  }

  if (far_left) {
    stats = dta %>%
      mutate(scenario = case_when(scenario == "Core" ~ "IRA", TRUE ~ scenario)) %>%
      group_by(year, stagger, scenario) %>%
      summarize(median = median(value))
    if (single) {
      width = 0.2
      left_margin = 11.5
    } else {
      width = 0.5
      left_margin = 0.5
    }
    segment_code = geom_segment(data = stats,
                                aes(x = year + stagger - width, xend = year + stagger + width,
                                    y = median, yend = median, color = scenario),
                                linewidth = 0.5)
  } else {
    if (!("stagger") %in% colnames(dta)) {
      dta$stagger = 0
    }
    stats = dta %>%
      mutate(scenario = case_when(scenario == "Core" ~ "IRA", TRUE ~ scenario)) %>%
      group_by(year, stagger) %>%
      summarize(median = median(value))
    segment_code = geom_segment(aes(x = year, xend = year, y = -5000, yend = -5000))
    left_margin = 0.5
  }

  stats = dta %>%
    mutate(scenario = case_when(scenario == "Core" ~ "IRA", TRUE ~ scenario)) %>%
    group_by(year, stagger, scenario) %>%
    summarize(median = median(value))

  p = dta %>% ggplot() +
    point_code +
    segment_code +
    scale_x_continuous(breaks = c(2030, 2035), labels = c(2030, 2035), limits = c(2028, 2037)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    #scale_subpalette(subpalettes,"ZZZZZ") +
    theme_emf() +
    ggtitle(title) +
    theme(axis.title.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 8), axis.ticks = element_blank(),
          plot.margin = margin(0.7,1,0.7,left_margin), legend.position = "none")

  if (!far_left) {
    p = p +
      geom_text_repel(aes(x = year + stagger, y = value + 20, label = scenario), point.padding = 1.5,
                      size = 1.5, hjust = 0, vjust = 0, max.time = 6, force_pull = 1, seed = 42) +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),)
    opt_label = "Optimistic Emis"
    pes_label = "Pessimistic Emis"
  } else {
    p = p +
      labs(y = "Emissions (Mt CO2/yr)") +
      #scale_subpalette(subpalettes, "Sensitivity Dots") +
      scale_color_manual(values = c("IRA" = "black", "IRA.Low" = "#42d4f4", "IRA.High" = "#883192"), guide = "none")
    opt_label = "Optimistic Emis"
    pes_label = "Pessimistic Emis"
  }

  if(sum(ira_coord) != 0) {
    p = p +
      annotate("text", x = ira_coord[1], y = ira_coord[2], size = 2,
               label = "Core", color = "black", alpha = 1, hjust = -0.2)
  }
  if(sum(low_coord) != 0) {
    p = p +
      annotate("text", x = low_coord[1], y = low_coord[2], size = 2,
               label = pes_label, color = "#42d4f4", alpha = 1, hjust = -0.2)
  }
  if(sum(high_coord) != 0) {
    p = p +
      annotate("text", x = high_coord[1], y = high_coord[2], size = 2,
               label = opt_label, color = "#883192", alpha = 1, hjust = -0.2)
  }

  return(list(
    "plot" = p,
    "stats" = stats
  ))
}


# function for spaghetti plots + dot plots
dot_plots = function(plot_type, config, emf_data_long, figmap, figure_num, reg, ymin, ymax, plot_title, metric, labels = FALSE, hist_year = 2021, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0), ylab = "") {
  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  # make the spagetti plot
  fig = print_graph(plot_type, config, emf_data_long, figmap, figure_num, reg)
  # update the axis range
  fig = fig + scale_y_continuous(limits = c(ymin, ymax)) +
    scale_x_continuous(breaks = c(2005,2010, hist_year, 2025, 2030, 2035), labels = c(2005, 2010, hist_year, 2025, 2030, 2035)) +
    scale_alpha(range = c(0.6, 1), guide = "none") +
    labs(y = ylab) +
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
    data_in = bind_rows(data_in, data_25)
  }

  data_wide = data_in %>% left_join(select(copy_data,c(value,year,variable,region)), by = c("year", "variable","region")) %>%
    mutate(value = case_when(
      year == hist_year & !is.na(value.y) ~ value.y,
      TRUE ~ value.x
    )) %>%
    select(-c(value.x,value.y))

  data_final = bind_rows(data_wide, hist_data)

  # the end
  return(data_final)
}

# remove all historical data and replace with EIA-LTS 2020 data for connectivity purposes
# clean_data2 = clean_data %>% duplicate_historical_data("EIA-LTS",2020)

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

#medians_table("time_series", config, clean_data, figmap_leep_timeseries, 9, "United States")

emis_stack = function(dta, title, econwide = FALSE) {
  totals = dta %>% group_by(year) %>%
    summarize(value = sum(value))

  dta = dta %>% group_by(year, variable_rename) %>%
    summarize(value = sum(value))

  if (econwide) {
    bar_code = geom_bar(aes(x = year, y = value, fill = variable_rename), position = position_stack(), stat = "identity")
  } else {
    bar_code = geom_bar(aes(x = year, y = value, fill = variable_rename,
                            color = variable_rename, alpha = variable_rename), position = position_stack(reverse = TRUE), stat = "identity")
  }

  p = dta %>% ggplot() +
    bar_code +
    #geom_text(aes(x = year, y = value, label = round(value, 0), fill = variable_rename), size = 2.5, position = position_stack(vjust = 0.5, reverse = FALSE)) +
    #geom_text(data = totals, aes(x = year, y = value, label = round(value, 0)), vjust = -0.4, stat = "identity", size = 2.5) +
    scale_x_continuous(breaks = c(2005, 2021), labels = c(2005, 2021)) +
    labs(y = "Emissions (Mt CO2/yr)", title = title) +
    scale_subpalette(subpalettes, "Emissions Stack") +
    theme_emf() +
    theme(axis.ticks = element_blank(), axis.title.x = element_blank(), plot.title = element_blank()) +
    bottom1

  return(p)
}


# A function to write all csvs in the final_figures/data/folder to a single excel document
# need to have installed package "openxlsx"
compile_all_data = function() {
  all_data = list.files("./output/final_figures/data", full.names = FALSE)
  #print(all_data)

  data_list = list()
  for (file in all_data) {
    fignum = strsplit(file, "_")[[1]][1]
    if(grepl(".csv",fignum)) {
      fignum = substr(fignum,1,nchar(fignum) - 4)
    }
    #print(fignum)
    if (substr(fignum,1,3) == "Fig") {
      df = read.csv(paste0("./output/final_figures/data/",file))
      data_list[[fignum]] = df
    }
  }

  openxlsx::write.xlsx(data_list, "./output/final_figures/data/ALL_DATA.xlsx")
  return(data_list)
}

dot_plots_but_with_arrows = function(plot_type, config, emf_data_long, figmap, figure_num, reg, ymin, ymax, plot_title, metric, labels = FALSE, hist_year = 2021, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0)) {
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

  df_lines_30 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2030)) %>%
    group_by(model, year) %>%
    summarize(min = min(value),
              max = max(value)) %>%
    mutate(diff = max - min) %>%
    mutate(median = median(diff)) %>%
    arrange(desc(max)) %>%
    data.frame() %>%
    mutate(stagger = row_number()) %>%
    mutate(stagger = stagger - 1) %>%
    mutate(stagger = stagger / max(stagger)) %>%
    mutate(stagger = stagger - 0.5)

  df_arrow_30 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2030), scenario == "IRA") %>%
    select(model, value) %>%
    rename(IRA_value = value)

  df_lines_30 = df_lines_30 %>% left_join(df_arrow_30, by = "model") %>%
    mutate(arrow_direction = case_when(
      max == IRA_value ~ "up",
      min == IRA_value ~ "down",
      TRUE ~ "uh-oh"
    )) %>%
    mutate(arrow_end = case_when(arrow_direction == "up" ~ max, TRUE ~ min),
           arrow_start = case_when(arrow_direction == "up" ~ min, TRUE ~ max))

  #View(df_lines_30)

  df_lines_35 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2035)) %>%
    group_by(model, year) %>%
    summarize(min = min(value),
              max = max(value)) %>%
    mutate(diff = max - min) %>%
    mutate(median = median(diff)) %>%
    arrange(desc(max)) %>%
    data.frame() %>%
    mutate(stagger = row_number()) %>%
    mutate(stagger = stagger - 1) %>%
    mutate(stagger = stagger / max(stagger)) %>%
    mutate(stagger = stagger - 0.5)
  # View(df_lines_35)

  df_arrow_35 = data_from_graph(plot_type, config, emf_data_long, figmap, figure_num, reg) %>%
    filter(year %in% c(2035), scenario == "IRA") %>%
    select(model, value) %>%
    rename(IRA_value = value)

  df_lines_35 = df_lines_35 %>% left_join(df_arrow_35, by = "model") %>%
    mutate(arrow_direction = case_when(
      max == IRA_value ~ "up",
      min == IRA_value ~ "down",
      TRUE ~ "uh-oh"
    )) %>%
    mutate(arrow_end = case_when(arrow_direction == "up" ~ max, TRUE ~ min),
           arrow_start = case_when(arrow_direction == "up" ~ min, TRUE ~ max))

  # still not entirely sure what this does...
  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  # dot plot for 2030
  dots_30 = ggplot() +
    # geom_point(data = df_30, aes(x = year, y = value, color = scenario),
    #            shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    geom_segment(data = df_lines_30, aes(x = year + stagger, xend = year + stagger, y = arrow_start, yend = arrow_end), color = "black",
                 size = 0.5, arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 25)) +
    # geom_segment(data = df_lines_30, aes(x = year - 0.5, xend = year + 0.5, y = median, yend = median), color = "black",
    #              size = 1) +
    # #segment_code_30 +
    #text_code +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029, 2031)) +
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
    # geom_point(data = df_35, aes(x = year, y = value, color = scenario),
    #            shape = 1, size = 1.5, position = position_dodge(width = 0.05)) +
    geom_segment(data = df_lines_35, aes(x = year + stagger, xend = year + stagger, y = arrow_start, yend = arrow_end), color = "black",
                 size = 0.5, arrow = arrow(length = unit(0.2, "cm"), type = "closed", angle = 25)) +
    # geom_segment(data = df_lines_35, aes(x = year - 0.5, xend = year + 0.5, y = median, yend = median), color = "black",
    #              size = 1) +
    # segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034, 2036)) +
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
