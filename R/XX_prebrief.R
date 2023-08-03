ranges = function(all_data, summary_data, filter_variable, subtitle = "", top = FALSE, bottom = FALSE) {
  data_small = all_data %>% filter(sector == filter_variable)
  summ_small = summary_data %>% filter(sector == filter_variable)
  p = data_small %>%
    ggplot() +
    geom_point(aes(x = pct_diff, y = scenario, color = scenario, shape = scenario), alpha = 0.8, size = 1) +
    geom_segment(data = summ_small,
                 aes(x = min, xend = max, y = scenario, yend = scenario, color = scenario),
                 alpha = 0.3, linewidth = 3) +
    geom_point(data = summ_small, aes(x = min, y = scenario, color = scenario, shape = scenario), size = 3) +
    geom_point(data = summ_small, aes(x = max, y = scenario, color = scenario, shape = scenario), size = 3) +
    geom_point(data = summ_small, aes(x = median, y = scenario, color = scenario),
               shape = "|", size = 1.5, stroke = 3.5) +
    geom_vline(xintercept = 50, color = "black", linewidth = 0.1, alpha = 0.4) +
    # scale_y_discrete(name = filter_variable) +
    ggtitle(subtitle) +
    scale_color_manual(values = c(subpalettes$`Economy-Wide Emissions`[['No IRA']],
                                  subpalettes$`Economy-Wide Emissions`[['IRA']]),
                       breaks = c("No IRA", "IRA")) +
    scale_shape_manual(values = c("No IRA" = 17, "IRA" = 16),
                       breaks = c("No IRA", "IRA")) +
    facet_grid(year ~ ., switch = "y") +
    theme_emf() +
    theme(axis.ticks = element_blank(), axis.text.y = element_blank(),
          #axis.title.y = element_text(size = 8),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 10, hjust = 0, face = "bold",
                                    margin = margin(0,0,1,0)),
          strip.text.y.left = element_text(size = 6, face = "plain", angle = 0),
          strip.text.y.right = element_text(size = 6, face = "plain", angle = 0),
          axis.text.x = element_text(face = "plain", size = 6),
          axis.title.x = element_text(face = "plain", size = 8),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          #axis.line.x = element_line(color = "#595959", linewidth = 0.25),
          axis.line.x = element_line(color = "white", linewidth = 0.25),
          plot.margin = margin(0,0,5,0))

  if (!(bottom)){
    p = p + theme(axis.title.x = element_blank()) +
      scale_x_continuous(limits = c(0,100), expand = c(0,0), position = "top",
                         name = expression("Percent Reduction from 2005 Levels (%)"))
  }
  if (!(top) & !(bottom)) {
    p = p + theme(axis.text.x = element_blank()) +
      scale_x_continuous(limits = c(0,100), expand = c(0,0), position = "bottom",
                         name = expression("Percent Reduction from 2005 Levels (%)"))
  }
  if (bottom) {
    p = p + scale_x_continuous(limits = c(0,100), expand = c(0,0), position = "bottom",
                               name = expression("Percent Reduction from 2005 Levels (%)"))
  }

  return(p)
}
