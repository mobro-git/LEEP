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
                                   "NEMS-EIA" = "dotdash"))+
  labs(title = "No IRA",
       y = expression(paste("Generation (TWh)")),
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
