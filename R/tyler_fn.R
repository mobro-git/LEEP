drop_warn = function(drop) {
  if(drop == "") {
    print("No models have been dropped from this figure.")
  } else {
    paste("The model(s)", paste(drop, collapse = " & "), "have been dropped from this figure.")
  }
}

spg_clean = function(ts_map_ID, drop, histsrc, config, clean_data, figmap_leep_timeseries) { #add in parameters from other functions being called in

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
        T ~ 1
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

  return(data = df)
}

spg2 = function(df, title, yname, gd, ymin, ymax, ybreaks, yax_format, annotate, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0), config, figmap_leep_timeseries) {

  hist = df %>%
    filter(scenario == "Historic")
  strt = df %>%
    filter(scenario != "Historic") %>%
    group_by(model, scenario) %>%
    filter(year == min(year))
  df = df %>%
    filter(year %in% c(2025, 2030, 2035))

  df = rbind(hist, strt, df)

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)
  subpalettes$`Emissions|CO2|Energy|Demand|Industry`[1] = "black"
  annoclr = subpalettes$`Emissions|CO2|Energy|Demand|Industry`
  histclr = annoclr[1]
  noIRAclr = annoclr[2]
  IRAclr = annoclr[3]

  figure = ggplot(df, aes(year,value, color = scenario, group = interaction(model, scenario))) +
    geom_line(aes(alpha = alpha, linetype = scenario), size = 0.5) +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme_emf() +
    scale_linetype_manual(values = c("Historic" = "solid", "IRA" = "solid", "No IRA" = "longdash"))+
    scale_x_continuous(breaks = c(2005, 2021, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax_format) +
    scale_alpha(range = c(1, 1), guide = F) +
    labs(title = title,
         x = element_blank(),
         y = yname) +
    theme(legend.position = gd,
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(-0.15, "cm")) +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historical", color = histclr, alpha = annotate) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = noIRAclr, alpha = annotate) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = IRAclr, alpha = annotate)

  return(list(figure = figure, data = df))
}

spg2_2010 = function(df, title, yname, gd, ymin, ymax, ybreaks, yax_format, annotate, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0), config, figmap_leep_timeseries) {

  hist = df %>%
    filter(scenario == "Historic")
  strt = df %>%
    filter(scenario != "Historic") %>%
    group_by(model, scenario) %>%
    filter(year == min(year))
  df = df %>%
    filter(year %in% c(2025, 2030, 2035))

  df = rbind(hist, strt, df)

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)
  subpalettes$`Emissions|CO2|Energy|Demand|Industry`[1] = "black"
    annoclr = subpalettes$`Emissions|CO2|Energy|Demand|Industry`
    histclr = annoclr[1]
    noIRAclr = annoclr[2]
    IRAclr = annoclr[3]

  figure = ggplot(df, aes(year,value, color = scenario, group = interaction(model, scenario))) +
    geom_line(aes(alpha = alpha, linetype = scenario), size = 0.5) +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme_emf() +
    scale_linetype_manual(values = c("Historic" = "solid", "IRA" = "solid", "No IRA" = "longdash"))+
    scale_x_continuous(breaks = c(2010, 2021, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax_format) +
    scale_alpha(range = c(1, 1), guide = F) +
    labs(title = title,
         x = element_blank(),
         y = yname) +
    theme(legend.position = gd,
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(-0.15, "cm")) +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historical", color = histclr, alpha = annotate) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = noIRAclr, alpha = annotate) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = IRAclr, alpha = annotate)

  return(list(figure = figure, data = df))
}

spg3 = function(df, title, yname, gd, ymin, ymax, ybreaks, yax_format, annotate, historic_coord = c(0,0), preira_coord = c(0,0), ira_coord = c(0,0), config, figmap_leep_timeseries) {

  hist = df %>%
    filter(scenario == "Historic")
  strt = df %>%
    filter(scenario != "Historic") %>%
    group_by(model, scenario) %>%
    filter(year == min(year))
  df = df %>%
    filter(year %in% c(2025, 2030, 2035))

  df = rbind(hist, strt, df)

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)
  subpalettes$`Emissions|CO2|Energy|Demand|Industry`[1] = "black"
    annoclr = subpalettes$`Emissions|CO2|Energy|Demand|Industry`
    histclr = annoclr[1]
    noIRAclr = annoclr[2]
    IRAclr = annoclr[3]

  figure = ggplot(df, aes(year,value, color = scenario, group = interaction(model, scenario))) +
    geom_line(aes(alpha = alpha, linetype = scenario), size = 0.5) +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    theme_emf() +
    scale_linetype_manual(values = c("Historic" = "solid", "IRA" = "solid", "No IRA" = "longdash"))+
    scale_x_continuous(breaks = c(2020, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax_format) +
    scale_alpha(range = c(1, 1), guide = F) +
    labs(title = title,
         x = element_blank(),
         y = yname) +
    theme(legend.position = gd,
          axis.ticks = element_line(color = "black"),
          axis.ticks.length = unit(-0.15, "cm")) +
    annotate("text", x = historic_coord[1], y = historic_coord[2], label = "Historical", color = histclr, alpha = annotate) +
    annotate("text", x = preira_coord[1], y = preira_coord[2], label = "No IRA", color = noIRAclr, alpha = annotate) +
    annotate("text", x = ira_coord[1], y = ira_coord[2], label = "IRA", color = IRAclr, alpha = annotate)

  return(list(figure = figure, data = df))
}

# spg = function(ts_map_ID, histsrc, title, yname, gd, drop, ymin, ymax, ybreaks, yax, config, figmap_leep_timeseries) {
#
#   subpalettes = create_subpalettes(figmap_leep_timeseries, config)
#
#   df = data_from_graph(
#     "time_series",
#     config,
#     clean_data,
#     figmap_leep_timeseries,
#     ts_map_ID,
#     "United States"
#   ) %>%
#     filter(!model %in% drop) %>%
#     filter(!datasrc %in% drop) %>%
#     mutate(
#       alpha = case_when(
#         model == "USREP-ReEDS" ~ 1,
#         model == "GCAM-PNNL" ~ 1,
#         model == "IPM-EPA" ~ 1,
#         scenario == "Historic" ~ 1,
#         T ~ 1
#       )
#     ) %>%
#     filter(year <= 2021 &
#              model == histsrc |
#              year >= 2021 & scenario != "Historic") %>%
#     pivot_wider(names_from = year, values_from = value) %>%
#     mutate(`2021` = case_when(T ~ `2021`[model == histsrc])) %>%
#     pivot_longer(cols = starts_with("20"),
#                  names_to = "year",
#                  values_to = "value") %>%
#     filter(!is.na(value)) %>%
#     mutate(year = as.numeric(year))
#
#   figure = ggplot(df, aes(year,value, color = scenario, group = interaction(model, scenario))) + geom_line(aes(alpha = alpha), size = 0.5) +
#     scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
#     theme_emf() +
#     scale_x_continuous(breaks = c(2005, 2021, 2025, 2030, 2035)) +
#     scale_y_continuous(limits = c(ymin, ymax), breaks = ybreaks, labels = yax) +
#     scale_alpha(range = c(1, 1), guide = F) +
#     labs(title = title,
#          x = element_blank(),
#          y = yname) +
#     theme(legend.position = gd,
#           axis.text.x = element_text(angle = 45, hjust = 1))
#
#   return(figure)
# }



dotted = function(df, spg, metric, ymin, ymax, config, figmap_leep_timeseries, ddge = 0.2) {

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
    geom_point(data = df_30, aes(x = year, y = value, color = scenario, alpha = alpha, shape = scenario),size = 1.5, position = position_dodge(width = ddge)) +
    segment_code_30 +
    scale_x_continuous(breaks = c(2030), labels = c("2030"), limits = c(2029.9, 2030.1)) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    scale_shape_manual(values = c("IRA" = 1, "No IRA" = 2)) +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,1,0,1))+
    scale_alpha(range = c(1, 1), guide = F)

  # dot plot for 2035
  dots_35 = ggplot() +
    geom_point(data = df_35, aes(x = year, y = value, color = scenario, alpha = alpha, shape = scenario), size = 1.5, position = position_dodge(width = ddge)) +
    segment_code_35 +
    scale_x_continuous(breaks = c(2035), labels = c("2035"), limits = c(2034.9, 2035.1)) +
    scale_y_continuous(limits = c(ymin,ymax)) +
    theme_emf() +
    scale_subpalette(subpalettes, "Emissions|CO2|Energy|Demand|Industry") +
    scale_shape_manual(values = c("IRA" = 1, "No IRA" = 2)) +
    theme(panel.grid = element_blank(), plot.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.ticks = element_blank(), panel.border = element_blank(),
          legend.position = "none", plot.margin = margin(0,1,0,1))+
    scale_alpha(range = c(1, 1), guide = F)

  figure = spg + dots_30 + dots_35 +
    plot_layout(widths = c(10,1,1)) +
    theme(plot.margin = margin(0,0,0,0))

  return(figure)
}



#This is an outdated version of the html call that creates scenario-consistent outputs
# html = function(df, title) {
#   base = df %>%
#     filter(year == 2005)
#
#   baseline = base$value[1]
#
#   base <- as.data.frame(base) %>%
#     select(Year = year,
#            `Absolute Level (Mt CO2)` = value) %>%
#     mutate(Scenario = "Baseline",
#            `% Reductions from 2005` = NA,
#            `% Reductions from No IRA` = NA,
#            `Reductions from No IRA (Mt CO2)` = NA)  %>%
#     select(Year, Scenario, `Absolute Level (Mt CO2)`, `% Reductions from 2005`, `% Reductions from No IRA`, `Reductions from No IRA (Mt CO2)`)
#
#   NoIRA <- df %>%
#     filter(year == 2030 | year == 2035) %>%
#     filter(scenario == "No IRA") %>%
#     group_by(year) %>%
#     summarize(`No IRA Median` = median(value),
#               `No IRA Min` = min(value),
#               `No IRA Max` =  max(value))%>%
#     pivot_longer(cols = !year, names_to = "Scenario", values_to = "Absolute Level (Mt CO2)") %>%
#     mutate(`2005_perc_red` = ((baseline-`Absolute Level (Mt CO2)`)/baseline) * 100) %>%
#     mutate(
#       Year = year,
#       `% Reductions from 2005` = `2005_perc_red`,
#       `% Reductions from No IRA` = NA,
#       `Reductions from No IRA (Mt CO2)` = NA
#     ) %>%
#     select(!year & !`2005_perc_red`)
#
#   IRA <- df %>%
#     filter(year == 2030 | year == 2035) %>%
#     filter(scenario == "IRA") %>%
#     select(!scenario) %>%
#     group_by(year) %>%
#     summarize(
#       `IRA Max` = max(value),
#       `IRA Min` = min(value),
#       `IRA Median` = median(value)) %>%
#     pivot_longer(cols = !year, names_to = "Scenario", values_to = "Absolute Level (Mt CO2)") %>%
#     mutate(
#       `% Reductions from 2005` = ((baseline-`Absolute Level (Mt CO2)`)/baseline) * 100
#     )%>%
#     mutate(
#       `% Reductions from No IRA` = case_when(
#         year == 2030 & Scenario == "IRA Max" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]) * 100,
#
#         year == 2030 & Scenario == "IRA Median" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]) * 100,
#
#         year == 2030 & Scenario == "IRA Min" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]) * 100,
#
#         year == 2035 & Scenario == "IRA Max" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]) * 100,
#
#         year == 2035 & Scenario == "IRA Median" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]) * 100,
#
#         year == 2035 & Scenario == "IRA Min" ~
#           ((NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)/NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]) * 100
#       ),
#
#       `Reductions from No IRA (Mt CO2)` = case_when(
#         year == 2030 & Scenario == "IRA Max" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`),
#
#         year == 2030 & Scenario == "IRA Median" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`),
#
#         year == 2030 & Scenario == "IRA Min" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2030 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`),
#
#         year == 2035 & Scenario == "IRA Max" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Max"]-`Absolute Level (Mt CO2)`),
#
#         year == 2035 & Scenario == "IRA Median" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Median"]-`Absolute Level (Mt CO2)`),
#
#         year == 2035 & Scenario == "IRA Min" ~
#           (NoIRA$`Absolute Level (Mt CO2)`[NoIRA$Year == 2035 & NoIRA$Scenario == "No IRA Min"]-`Absolute Level (Mt CO2)`)
#       )
#     ) %>%
#     rename(Year = year)
#
#   IRA = rbind(NoIRA, IRA)
#
#   IRA <- IRA %>%
#     arrange(desc(IRA)) %>%
#     arrange(Year)
#
#   IRA = rbind(base, IRA)
#
#   IRA$`Absolute Level (Mt CO2)` = round(IRA$`Absolute Level (Mt CO2)`, 3)
#   IRA$`% Reductions from 2005` = round(IRA$`% Reductions from 2005`, 3)
#   IRA$`% Reductions from No IRA` = round(IRA$`% Reductions from No IRA`, 3)
#   IRA$`Reductions from No IRA (Mt CO2)` = round(IRA$`Reductions from No IRA (Mt CO2)`,3)
#
#   tab = IRA %>%
#     tableHTML(caption = paste(title), rownames = F)
#
#   write.csv(IRA, paste("./output/final_figures/stats_tables/", fig_no, "SummaryTable ",title,".csv",sep=""))
#
#   return(tab)
# }

#Updated version of the html function that calculates % differences from No IRA a bit diffrently
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
    )

  pd_30 = df %>%
    filter(year == 2030) %>%
    select(year, model, value, scenario) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    mutate(pd = ((`No IRA` - IRA) / `No IRA`)*100)

  pd_35 = df %>%
    filter(year == 2035) %>%
    select(year, model, value, scenario) %>%
    pivot_wider(names_from = scenario, values_from = value) %>%
    mutate(pd = ((`No IRA` - IRA) / `No IRA`)*100)

  IRA = IRA %>%
    mutate(
      `% Reductions from No IRA` = case_when(
        year == 2030 & Scenario == "IRA Max" ~ max(pd_30$pd),

        year == 2030 & Scenario == "IRA Median" ~ median(pd_30$pd),

        year == 2030 & Scenario == "IRA Min" ~ min(pd_30$pd),

        year == 2035 & Scenario == "IRA Max" ~max(pd_35$pd),

        year == 2035 & Scenario == "IRA Median" ~ median(pd_35$pd),

        year == 2035 & Scenario == "IRA Min" ~ min(pd_35$pd)
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

  IRA$`Absolute Level (Mt CO2)` = round(IRA$`Absolute Level (Mt CO2)`, 2)
  IRA$`% Reductions from 2005` = round(IRA$`% Reductions from 2005`, 2)
  IRA$`% Reductions from No IRA` = round(IRA$`% Reductions from No IRA`, 2)
  IRA$`Reductions from No IRA (Mt CO2)` = round(IRA$`Reductions from No IRA (Mt CO2)`, 2)

  tab = IRA %>%
    tableHTML(caption = paste(title), rownames = F)

  write.csv(IRA, paste("./output/final_figures/stats_tables/", fig_no, "SummaryTable ",title,".csv",sep=""))

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
      color = model,
      linetype = model),
      size = 0.75) +
    geom_point(aes(x = 2021, y = 0), color = "black") +
    geom_point(
      data = medians,
      aes(x = year, y = median),
      color = "black",
      shape = 16,
      size = 2
    ) +
    scale_linetype_manual(values = c("USREP-ReEDS" = "solid",
                                     "EPS-EI" = "solid",
                                     "GCAM-CGS" = "solid",
                                     "GCAM-PNNL" = "solid",
                                     "Haiku-RFF" = "solid",
                                     "IPM-NRDC" = "solid",
                                     "IPM-EPA" = "solid",
                                     "MARKAL-NETL" = "solid",
                                     "NEMS-RHG" = "solid",
                                     "NEMS-OP" = "twodash",
                                     "REGEN-EPRI" = "dashed",
                                     "RIO-REPEAT" = "dotted",
                                     "ReEDS-NREL" = "longdash",
                                     "NEMS-EIA" = "dotdash"))+
    scale_subpalette(subpalettes, "Emissions|CO2|Percent difference from No IRA") + #Standard subpallete
    labs(title = title,
         x = "",
         y = yname) +
    theme_emf() +
    theme( axis.ticks = element_line(color = "black"),
           axis.ticks.length = unit(-0.15, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing.x = unit(4, "mm"),
      legend.position = gd
    ) +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035))
  return(list(figure = figure,
              pd_df = df))
}

ad = function(diff_ID, title, metric, gd, drop) {

  if (metric == "Generation") {
    clean_data = clean_data %>%
      mutate(value = case_when(unit == "Quads" ~ value * 293.07, TRUE ~ value),
             unit = case_when(unit == "Quads" ~ "TWh", TRUE ~ unit))
  }

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

  if (metric == "Generation") {
  figure = ggplot() +
    geom_line(data = df,
              aes(
                x = year,
                y = diff,
                group = model,
                color = model,
                linetype = model
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
    scale_linetype_manual(values = c("USREP-ReEDS" = "solid",
                                     "EPS-EI" = "solid",
                                     "GCAM-CGS" = "solid",
                                     "GCAM-PNNL" = "solid",
                                     "Haiku-RFF" = "solid",
                                     "IPM-NRDC" = "solid",
                                     "IPM-EPA" = "solid",
                                     "MARKAL-NETL" = "solid",
                                     "NEMS-RHG" = "solid",
                                     "NEMS-OP" = "twodash",
                                     "REGEN-EPRI" = "dashed",
                                     "RIO-REPEAT" = "dotted",
                                     "ReEDS-NREL" = "longdash",
                                     "NEMS-EIA" = "dotdash"))+
    labs(title = title,
         x = "",
         y = expression(paste("Absoltue Difference (TWh)"))) +
    theme_emf() +
    theme( axis.ticks = element_line(color = "black"),
           axis.ticks.length = unit(-0.15, "cm"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing.x = unit(4, "mm"),
      legend.position = gd
    ) +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
    scale_y_continuous(labels = scales::comma)
  } else {
    figure = ggplot() +
      geom_line(data = df,
                aes(
                  x = year,
                  y = diff,
                  group = model,
                  color = model,
                  linetype = model
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
      scale_linetype_manual(values = c("USREP-ReEDS" = "solid",
                                       "EPS-EI" = "solid",
                                       "GCAM-CGS" = "solid",
                                       "GCAM-PNNL" = "solid",
                                       "Haiku-RFF" = "solid",
                                       "IPM-NRDC" = "solid",
                                       "IPM-EPA" = "solid",
                                       "MARKAL-NETL" = "solid",
                                       "NEMS-RHG" = "solid",
                                       "NEMS-OP" = "twodash",
                                       "REGEN-EPRI" = "dashed",
                                       "RIO-REPEAT" = "dotted",
                                       "ReEDS-NREL" = "longdash",
                                       "NEMS-EIA" = "dotdash")) +
      labs(title = title,
           x = "",
           y = expression(paste("Absolute Difference (Mt C", O[2], "/yr)"))) +
      theme_emf() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing.x = unit(4, "mm"),
        legend.position = gd
      ) +
      scale_x_continuous(breaks = c(2021, 2025, 2030, 2035))+
      scale_y_continuous(labels = scales::comma)
  }
  return(list(figure = figure,
              ad_df = df))
}

delta = function(pd_map_ID, drop, config, clean_data, figmap_leep_timeseries) {

  subpalettes = create_subpalettes(figmap_leep_timeseries, config)


  df = data_from_graph(
    "time_series",
    config,
    clean_data,
    figmap_leep_timeseries,
    pd_map_ID,
    "United States"
  ) %>%
    filter(!scenario == "No IRA") %>%
    filter(year > 2021) %>%
    filter(!model %in% drop) %>%
    group_by(model, year, scenario, variable_rename) %>%
    summarize(value= sum(value))

  medians = df %>%
    group_by(year) %>%
    summarize(median = median(value))
  med_hist = medians %>%
    filter(year == 2030) %>%
    mutate(year = 2021) %>%
    mutate(median = 0)

  medians = rbind(medians, med_hist)

  medians = medians %>%
    filter(year %in% c(2021, 2025, 2030, 2035))

  #Set 2021 value to zero for all models
  hist <- df %>%
    group_by(model) %>%
    slice(1) %>%
    mutate(scenario = "IRA",
           value = 0,
           year = 2021)

  #Stitch it back together with main data
  df <- rbind(df, hist)

  df = df %>%
    filter(year %in% c(2021, 2025, 2030, 2035))

  figure = ggplot() +
    geom_hline(aes(yintercept=0), color = "#9E9E9E", size = 0.75) +
    geom_line(data = df, aes(
      x = year,
      y = value,
      group = model,
      color = scenario,
      alpha = 1),
      size = 0.75) +
    #    geom_point(aes(x = 2021, y = 0), color = "black") +
    geom_line(
      data = medians,
      aes(x = year, y = median),
      color = "black", size = 0.75
    ) +
    scale_subpalette(subpalettes, "Final Energy")+ #Standard subpallete
    labs(title = "",
         x = "",
         y = expression("% Difference from No IRA")) +
    theme_emf() +
    theme(
      axis.ticks = element_line(color = "black"),
      axis.ticks.length = unit(-0.15, "cm"),
      panel.spacing.x = unit(4, "mm"),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
    scale_y_continuous(name = NULL, sec.axis = sec_axis(~., name = expression("% Difference from No IRA"))) +
    guides(y = "none")
  return(list(figure = figure, medians = medians, df = df))
}




four_corners = function(title, ts_map_ID, pd_map_ID, ad_map_ID, drop, histsrc, metric, unit, fig_no, ymin, ymax, brk) {
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
    geom_line(aes(linetype = model),size = 0.75) +
    scale_subpalette(subpalettes, "Emissions|CO2|Percent difference from No IRA") +
    theme_emf() +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = brk, labels = scales::comma) +
    scale_linetype_manual(values = c("USREP-ReEDS" = "solid",
                                     "EPS-EI" = "solid",
                                     "GCAM-CGS" = "solid",
                                     "GCAM-PNNL" = "solid",
                                     "Haiku-RFF" = "solid",
                                     "IPM-NRDC" = "solid",
                                     "IPM-EPA" = "solid",
                                     "MARKAL-NETL" = "solid",
                                     "NEMS-RHG" = "solid",
                                     "NEMS-OP" = "twodash",
                                     "REGEN-EPRI" = "dashed",
                                     "RIO-REPEAT" = "dotted",
                                     "ReEDS-NREL" = "longdash",
                                     "NEMS-EIA" = "dotdash")) +
    labs(title = "No IRA",
         x = element_blank()) +
    theme(  axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(-0.15, "cm"),
           legend.position = "right",
         plot.title = element_text(hjust = 0.5),
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
    geom_line(aes(linetype = model),size = 0.75) +
    scale_subpalette(subpalettes, "Emissions|CO2|Percent difference from No IRA") +
    theme_emf() +
    scale_x_continuous(breaks = c(2021, 2025, 2030, 2035)) +
    scale_y_continuous(limits = c(ymin, ymax), breaks = brk, labels = scales::comma) +
    labs(title = "IRA",
         y = expression(paste("Generation (TWh)")),
         x = element_blank()) +
    scale_linetype_manual(values = c("USREP-ReEDS" = "solid",
                                     "EPS-EI" = "solid",
                                     "GCAM-CGS" = "solid",
                                     "GCAM-PNNL" = "solid",
                                     "Haiku-RFF" = "solid",
                                     "IPM-NRDC" = "solid",
                                     "IPM-EPA" = "solid",
                                     "MARKAL-NETL" = "solid",
                                     "NEMS-RHG" = "solid",
                                     "NEMS-OP" = "twodash",
                                     "REGEN-EPRI" = "dashed",
                                     "RIO-REPEAT" = "dotted",
                                     "ReEDS-NREL" = "longdash",
                                     "NEMS-EIA" = "dotdash"))+
    theme(  axis.ticks = element_line(color = "black"),
            axis.ticks.length = unit(-0.15, "cm"),
           legend.position = "right",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_point(aes(x = 2021, y = ts_df$value[ts_df$year == 2021][1]), color = "black") +
    geom_point(
      data = IRAmedians,
      aes(x = year, y = median),
      color = "black",
      shape = 16,
      size = 2
    )

  if(metric == "Generation") {
    NoIRAfigure = NoIRAfigure + labs(y = expression(paste("Generation (TWh)")))
    IRAfigure = IRAfigure + labs(y = expression(paste("Generation (TWh)")))
  }
  if(metric == "Primary Energy") {
    NoIRAfigure = NoIRAfigure + labs(y = expression(paste("Primary Energy (Quads)")))
    IRAfigure = IRAfigure + labs(y = expression(paste("Primary Energy (Quads)")))
  }
  if(metric == "Emissions") {
    NoIRAfigure = NoIRAfigure + labs(expression(paste("Emissions (Mt C", O[2], "/yr)")))
    IRAfigure = IRAfigure + labs(expression(paste("Emissions (Mt C", O[2], "/yr)")))
  }

  #Percent Difference
  pdfigure = pd(pd_map_ID, "", expression(paste("Percent Difference (%)")), "none", drop)
  pdfigure$pd_df = pdfigure$pd_df %>% mutate(figure_num = fig_no)

  #Absolute Difference
  adfigure = ad(diff_ID = ad_map_ID, "", metric, "none", drop)
  adfigure$ad_df = adfigure$ad_df %>% mutate(figure_num = fig_no)

  figure = (NoIRAfigure | IRAfigure) / (adfigure$figure | pdfigure$figure)+
    plot_layout(guides = "collect") +
    plot_annotation(caption = title)

  return(list(
    figure = figure,
    ts_df = ts_df,
    ad_df = adfigure$ad_df,
    pd_df = pdfigure$pd_df
  ))

}

