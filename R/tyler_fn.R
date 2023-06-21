drop_warn = function(drop) {
  if(drop == "") {
    print("No models have been dropped from this figure.")
  } else {
    paste("The model(s)", paste(drop, collapse = " & "), "have been dropped from this figure.")
  }
}



spg = function(ts_map_ID, histsrc, title, yname, gd, drop, ymin, ymax, ybreaks, yax) {

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  df = data_from_graph(
    "time_series",
    config,
    clean_data,
    figmap_leep_timeseries,
    ts_map_ID,
    "United States"
  ) %>%
    filter(!model %in% drop) %>%
    filter(!datasrc %in% drop) %>%
    mutate(
      alpha = case_when(
        model == "USREP-ReEDS" ~ 1,
        model == "GCAM-PNNL" ~ 1,
        model == "IPM-EPA" ~ 1,
        scenario == "Historic" ~ 1,
        T ~ 0.6
      )
    ) %>%
    filter(year <= 2021 &
             model == histsrc |
             year >= 2021 & scenario != "Historic") %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(`2021` = case_when(T ~ `2021`[model == histsrc])) %>%
    pivot_longer(cols = starts_with("20"),
                 names_to = "year",
                 values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(year = as.numeric(year))

  figure = ggplot(df, aes(year,value, color = scenario, group = interaction(model, scenario))) + geom_line(aes(alpha = alpha), size = 0.5) +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme_emf() +
    scale_x_continuous(breaks = c(2005, 2021, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax) +
    scale_alpha(range = c(0.5, 1), guide = F) +
    labs(title = title,
         x = element_blank(),
         y = yname) +
    theme(legend.position = gd,
          axis.text.x = element_text(angle = 45, hjust = 1))

  return(figure)
}



dotted = function(df, spg, metric, ymin, ymax) {

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)

  df_30 = df %>%
    filter(year %in% c(2030))
  df_stat_30 = df_30 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))

  df_35 = df %>%
    filter(year %in% c(2035))
  df_stat_35 = df_35 %>%
    group_by(scenario,year,variable_rename) %>%
    summarise(mean = mean(value),
              median = median(value))


  mean_diff_30 = df_stat_30[df_stat_30$scenario == "No IRA","mean"]$mean - df_stat_30[df_stat_30$scenario == "IRA","mean"]$mean
  mean_diff_35 = df_stat_35[df_stat_35$scenario == "No IRA","mean"]$mean - df_stat_35[df_stat_35$scenario == "IRA","mean"]$mean
  median_diff_30 = df_stat_30[df_stat_30$scenario == "No IRA","median"]$median - df_stat_30[df_stat_30$scenario == "IRA","median"]$median
  median_diff_35 = df_stat_35[df_stat_35$scenario == "No IRA","median"]$median - df_stat_35[df_stat_35$scenario == "IRA","median"]$median
  # stick em all in a data frame
  stats = data.frame("year" = c(2030,2035,2030,2035), "metric" = c("mean","mean","median","median"),
                     "difference" = c(mean_diff_30, mean_diff_35, median_diff_30, median_diff_35))

  # a few of the ggplot lines of code vary based on the year or on the metric chosen, so conditionally choose the right one here
  if (metric == "mean") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - 0.1, xend = year + 0.1, y = mean, yend = mean, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - 0.1, xend = year + 0.1, y = mean, yend = mean, color = scenario),
                                   size = 1)
  } else if (metric == "median") {
    segment_code_30 = geom_segment(data = df_stat_30, aes(x = year - 0.1, xend = year + 0.1, y = median, yend = median, color = scenario),
                                   size = 1)
    segment_code_35 = geom_segment(data = df_stat_35, aes(x = year - 0.1, xend = year + 0.1, y = median, yend = median, color = scenario),
                                   size = 1)
  }

  dots_30 = ggplot() +
    geom_point(data = df_30, aes(x = year, y = value, color = scenario, alpha = alpha), shape = 1, size = 1.5, position = position_dodge(width = 0.2)) +
    segment_code_30 +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029.9, 2030.1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))+
    scale_alpha(range = c(0.6, 1), guide = F)

  # dot plot for 2035
  dots_35 = ggplot() +
    geom_point(data = df_35, aes(x = year, y = value, color = scenario, alpha = alpha), shape = 1, size = 1.5, position = position_dodge(width = 0.2)) +
    segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034.9, 2035.1)) +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust=1),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,0,0,0))+
    scale_alpha(range = c(0.6, 1), guide = F)

  figure = spg + dots_30 + dots_35 +
    plot_layout(widths = c(10,1,1)) +
    theme(plot.margin = margin(0,0,0,0))

  return(figure)
}




html = function(df, title) {
  base = df %>%
    filter(year == 2005)

  baseline = base$value[1]

  base <- as.data.frame(base) %>%
    select(Year = year,
           `Absolute Level (Mt CO2)` = value) %>%
    mutate(Scenario = "Baseline",
           `% Reductions from 2005` = NA,
           `% Reductions from No IRA` = NA,
           `Reductions from No IRA (Mt CO2)` = NA)  %>%
    select(Year, Scenario, `Absolute Level (Mt CO2)`, `% Reductions from 2005`, `% Reductions from No IRA`, `Reductions from No IRA (Mt CO2)`)

  NoIRA <- df %>%
    filter(year == 2030 | year == 2035) %>%
    filter(scenario == "No IRA") %>%
    group_by(year) %>%
    summarize(`No IRA Median` = median(value),
              `No IRA Min` = min(value),
              `No IRA Max` =  max(value))%>%
    pivot_longer(cols = !year, names_to = "Scenario", values_to = "Absolute Level (Mt CO2)") %>%
    mutate(`2005_perc_red` = ((baseline-`Absolute Level (Mt CO2)`)/baseline) * 100) %>%
    mutate(
      Year = year,
      `% Reductions from 2005` = `2005_perc_red`,
      `% Reductions from No IRA` = NA,
      `Reductions from No IRA (Mt CO2)` = NA
    ) %>%
    select(!year & !`2005_perc_red`)

  IRA <- df %>%
    filter(year == 2030 | year == 2035) %>%
    filter(scenario == "IRA") %>%
    select(!scenario) %>%
    group_by(year) %>%
    summarize(
      `IRA Max` = max(value),
      `IRA Min` = min(value),
      `IRA Median` = median(value)) %>%
    pivot_longer(cols = !year, names_to = "Scenario", values_to = "Absolute Level (Mt CO2)") %>%
    mutate(
      `% Reductions from 2005` = ((baseline-`Absolute Level (Mt CO2)`)/baseline) * 100
    )%>%
    mutate(
      `% Reductions from No IRA` = case_when(
        year == 2030 & Scenario == "IRA Max" ~
          ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]) * 100,

        year == 2030 & Scenario == "IRA Median" ~
          ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]) * 100,

        year == 2030 & Scenario == "IRA Min" ~
          ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]) * 100,

        year == 2035 & Scenario == "IRA Max" ~
          ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]) * 100,

        year == 2035 & Scenario == "IRA Median" ~
          ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]) * 100,

        year == 2035 & Scenario == "IRA Min" ~
          ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]) * 100
      ),

      `Reductions from No IRA (Mt CO2)` = case_when(
        year == 2030 & Scenario == "IRA Max" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`),

        year == 2030 & Scenario == "IRA Median" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`),

        year == 2030 & Scenario == "IRA Min" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`),

        year == 2035 & Scenario == "IRA Max" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`),

        year == 2035 & Scenario == "IRA Median" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`),

        year == 2035 & Scenario == "IRA Min" ~
          (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)
      )
    ) %>%
    rename(Year = year)

  IRA = rbind(NoIRA, IRA)

  IRA <- IRA %>%
    arrange(desc(IRA)) %>%
    arrange(Year)

  IRA = rbind(base, IRA)

  IRA$`Absolute Level (Mt CO2)` = round(IRA$`Absolute Level (Mt CO2)`, 3)
  IRA$`% Reductions from 2005` = round(IRA$`% Reductions from 2005`, 3)
  IRA$`% Reductions from No IRA` = round(IRA$`% Reductions from No IRA`, 3)
  IRA$`Reductions from No IRA (Mt CO2)` = round(IRA$`Reductions from No IRA (Mt CO2)`,3)

  tab = IRA %>%
    tableHTML(caption = paste(title), rownames = F)

  return(tab)
}

#Function to create standard percent difference figs
pd = function(ts_map_ID, title, yname, gd, drop) {
  #Take data from leep_timeseries map based on ID.
  df = data_from_graph(
    "time_series",
    config,
    clean_data,
    figmap_leep_timeseries,
    ts_map_ID,
    "United States"
  ) %>%
    filter(!scenario == "No IRA") %>%
    filter(year > 2021) %>%
    filter(!model %in% drop) %>%
    group_by(model, year, scenario, variable_rename) %>%
    summarize(value= sum(value))

  medians = df %>%
    filter(year %in% c(2030, 2035)) %>%
    filter(value != 0) %>%
    group_by(year) %>%
    summarize(median = median(value))

  #Set 2021 value to zero for all models
  hist <- df %>%
    group_by(model) %>%
    slice(1) %>%
    mutate(scenario = "Historic",
           value = 0,
           year = 2021)

  #Stitch it back together with main data
  df <- rbind(df, hist)

  #Plot standard figure
  figure = ggplot() +
    geom_line(data = df, aes(
      x = year,
      y = value,
      group = model,
      color = model),
      size = 0.75) +
    geom_point(aes(x = 2021, y = 0), color = "black") +
    geom_point(
      data = medians,
      aes(x = year, y = median),
      color = "black",
      shape = 16,
      size = 2
    ) +
    scale_subpalette(subpalettes, "Emissions|CO2|Percent difference from No IRA") + #Standard subpallete
    labs(title = title,
         x = "",
         y = yname) +
    theme_emf() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing.x = unit(4, "mm"),
      legend.position = gd
    ) +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035))
  return(list(figure = figure,
              pd_df = df))
}

ad = function(diff_ID, title, yname, gd, drop) {

  df = data_from_graph("diff_bar",
                       config,
                       clean_data,
                       figmap_leep_diffbar,
                       diff_ID,
                       "United States") %>%
    filter(!model %in% drop)%>%
    filter(year > 2021) %>%
    group_by(model, year, scenario, variable_rename) %>%
    summarize(diff = sum(diff))

  medians = df %>%
    filter(year %in% c(2030, 2035)) %>%
    group_by(year) %>%
    summarize(median = median(diff))

  hist <- df %>%
    group_by(model) %>%
    slice(1) %>%
    mutate(scenario = "Historic",
           diff = 0,
           year = 2021)

  df <- rbind(df, hist)

  figure = ggplot() +
    geom_line(data = df,
              aes(
                x = year,
                y = diff,
                group = model,
                color = model
              ),
              size = 0.75) +
    geom_point(aes(x = 2021, y = 0), color = "black") +
    geom_point(
      data = medians,
      aes(x = year, y = median),
      color = "black",
      shape = 16,
      size = 2
    ) +
    scale_subpalette(subpalettes, "Emissions|CO2|Percent difference from No IRA") +
    labs(title = title,
         x = "",
         y = yname) +
    theme_emf() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing.x = unit(4, "mm"),
      legend.position = gd
    ) +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035))
  return(list(figure = figure,
              ad_df = df))
}

four_corners = function(title, ts_map_ID, pd_map_ID, ad_map_ID, drop, histsrc, metric, unit, fig_no) {
  if (metric == "Generation") {
    clean_data = clean_data %>%
      mutate(value = case_when(unit == "Quads" ~ value * 293.07, TRUE ~ value),
             unit = case_when(unit == "Quads" ~ "TWh", TRUE ~ unit))
  }

  #Time Series Dataframe
  ts_df = data_from_graph(
    "time_series",
    config,
    clean_data,
    figmap_leep_timeseries,
    ts_map_ID,
    "United States"
  ) %>%
    filter(!model %in% drop) %>%
    filter(!datasrc %in% drop)%>%
    filter(year >= 2021) %>%
    filter(year <= 2021 &
             model == histsrc |
             year > 2021 & scenario != "Historic") %>%
    group_by(model, scenario, year, variable_rename) %>%
    summarize(value = sum(value)) %>%
    pivot_wider(names_from = year, values_from = value)

  ts_df = ts_df %>%
    mutate(`2021` = case_when(T ~ ts_df$`2021`[ts_df$model == histsrc])) %>%
    pivot_longer(cols = starts_with("20"),
                 names_to = "year",
                 values_to = "value") %>%
    filter(!is.na(value)) %>%
    mutate(year = as.numeric(year)) %>%
    mutate(figure_num = fig_no)

  noIRAmedians = ts_df[ts_df$scenario == "No IRA",] %>%
    filter(year == 2030 | year == 2035) %>%
    group_by(year) %>%
    summarize(median = median(value))

  IRAmedians = ts_df[ts_df$scenario == "IRA",] %>%
    filter(year == 2030 | year == 2035) %>%
    group_by(year) %>%
    summarize(median = median(value))

  NoIRAfigure = ggplot(ts_df[ts_df$scenario == "No IRA", ], aes(year, value, color = model)) +
    geom_line(size = 0.75) +
    scale_subpalette(subpalettes, "Emissions|CO2|Percent difference from No IRA") +
    theme_emf() +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
    labs(title = "No IRA",
         y = paste0(metric," (",unit,")"),
         x = element_blank()) +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_point(aes(x = 2021, y = ts_df$value[ts_df$year == 2021][1]), color = "black") +
    geom_point(
      data = noIRAmedians,
      aes(x = year, y = median),
      color = "black",
      shape = 16,
      size = 2
    )

  IRAfigure = ggplot(ts_df[ts_df$scenario == "IRA", ], aes(year, value, color = model)) +
    geom_line(size = 0.75) +
    scale_subpalette(subpalettes, "Emissions|CO2|Percent difference from No IRA") +
    theme_emf() +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
    labs(title = "IRA",
         y = paste0(metric," (",unit,")"),
         x = element_blank()) +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_point(aes(x = 2021, y = ts_df$value[ts_df$year == 2021][1]), color = "black") +
    geom_point(
      data = IRAmedians,
      aes(x = year, y = median),
      color = "black",
      shape = 16,
      size = 2
    )

  #Percent Difference
  pdfigure = pd(pd_map_ID, "", expression(paste("Percent Difference (%)")), "none", drop)
  pdfigure$pd_df = pdfigure$pd_df %>% mutate(figure_num = fig_no)

  #Absolute Difference
  adfigure = ad(diff_ID = ad_map_ID, "", paste0("Absolute Difference (",unit,")"), "none", drop)
  adfigure$ad_df = adfigure$ad_df %>% mutate(figure_num = fig_no)

  figure = (NoIRAfigure | IRAfigure) / (adfigure$figure | pdfigure$figure)+
    plot_layout(guides = "collect") +
    plot_annotation(title = title)

  return(list(
    figure = figure,
    ts_df = ts_df,
    ad_df = adfigure$ad_df,
    pd_df = pdfigure$pd_df
  ))
}
