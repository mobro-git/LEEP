
primarynrg_historic = readxl::read_xlsx("data-extra/ira_comparison_raw/historic_primary_energy.xlsx") %>%
  mutate(model = "EIA",
         scenario = "Historic") %>%
  pivot_longer(cols = c("coal","gas","oil"), names_to = "var", values_to = "quads") %>%
  mutate(variable = case_when(
    var == "coal" ~ "Primary Energy|Coal",
    var == "gas" ~ "Primary Energy|Gas",
    var == "oil" ~ "Primary Energy|Oil"
  )) %>%
  # converting from quads to EJ/Yr
  mutate(value = quads*1.05505585262,
         unit = "EJ/yr",
         region = "United States") %>%
  select(model,scenario,year,variable,value,unit,region)

write.csv(primarynrg_historic, "data-raw/model-runs/EIA-MER_primaryenergy.csv", row.names = FALSE)
